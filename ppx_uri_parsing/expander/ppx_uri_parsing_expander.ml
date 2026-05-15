open Core
open Ppxlib

let map_loc { txt; loc } ~f = { txt = f txt; loc }
let with_loc txt ~loc = { txt; loc }

let suffix s type_name =
  if String.equal type_name "t" then "" else [%string "_%{s}_%{type_name}"]
;;

module Signature = struct
  let val_declaration ~loc ~prefix ~suffix ~type_ =
    Ast_helper.Sig.value
      ~loc
      (Ast_helper.Val.mk (with_loc [%string "%{prefix}%{suffix}"] ~loc) type_)
  ;;

  let wrap_with_param_inputs ~loc params output_type =
    List.fold_right params ~init:output_type ~f:(fun (param, _) output_type ->
      [%type: ([%t param], _) Ppx_uri_parsing_lib.Derived_parser.t -> [%t output_type]])
  ;;

  let signature ~loc ~path:_ (_rec_flag, type_decls) =
    match type_decls with
    | [ type_decl ] ->
      let type_name = type_decl.ptype_name.txt in
      let type_param =
        Ast_helper.Typ.constr
          (type_decl.ptype_name |> Ast_builder.Default.Located.map_lident)
          (List.map type_decl.ptype_params ~f:fst)
      in
      let suffix = suffix "for" type_name in
      (* val parser%{suffix} : %{type_name} Uri_parsing.Parser.t *)
      (* We don't emit a top-level [parser] value when the type has any type parameters. *)
      let parser =
        match List.is_empty type_decl.ptype_params with
        | false -> []
        | true ->
          [ val_declaration
              ~loc
              ~prefix:"parser"
              ~suffix
              ~type_:[%type: [%t type_param] Uri_parsing.Parser.t]
          ]
      in
      (* module Ppx_uri_parsing_lib *)
      let ppx_uri_parsing_lib =
        [%sigi:
          module Ppx_uri_parsing_lib : sig
            [%%i
              val_declaration
                ~loc
                ~prefix:"parser"
                ~suffix
                ~type_:
                  ([%type:
                     ([%t type_param], [ `Parser ]) Ppx_uri_parsing_lib.Derived_parser.t]
                   |> wrap_with_param_inputs ~loc type_decl.ptype_params)]
          end]
      in
      parser @ [ ppx_uri_parsing_lib ]
    | [] ->
      (* I'm not sure if is syntactically possible, but just in case: *)
      Location.raise_errorf ~loc "ppx_uri_parsing does not support empty types"
    | _ :: _ :: _ ->
      Location.raise_errorf
        ~loc
        "ppx_uri_parsing does not support mutually recursive types"
  ;;
end

module Structure = struct
  (* By default, we strip 's from field & constructor names and use kebab-case for field
     and constructor names. The capitalization case can be customized with [~capitalize].

     (' is the only character allowed in constructor names that should be escaped in urls,
     and - is generally preferred in URIs over _, e.g. because they are more visible when
     links are underlined.) *)
  let capitalize_name name ~capitalize =
    let capitalize =
      Option.value_map
        capitalize
        ~f:(fun { txt; loc } ->
          try Capitalization.of_string txt with
          | e -> Location.raise_errorf ~loc "%s" (Exn.to_string e))
        ~default:Capitalization.Kebab_case
    in
    name
    |> String.substr_replace_all ~pattern:"'" ~with_:""
    |> Capitalization.apply_to_snake_case capitalize
  ;;

  (* Uri_parsing will loop infinitely trying to construct a parser for a recursive type.
     It's better to just give an error message upfront. *)
  let check_for_recursive_type type_decl =
    let checker =
      object
        inherit Ast_traverse.iter as super

        method! core_type core_type =
          match core_type.ptyp_desc with
          | Ptyp_constr ({ txt = Lident name; loc }, params) ->
            if String.equal name type_decl.ptype_name.txt
            then
              Location.raise_errorf
                ~loc
                "ppx_uri_parsing: Uri_parsing does not support recursive types"
            else List.iter params ~f:super#core_type
          | _ -> super#core_type core_type
      end
    in
    checker#type_kind type_decl.ptype_kind
  ;;

  let type_param_names params =
    List.map params ~f:(fun (param, _) ->
      let type_name =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree param.ptyp_desc with
        | Ptyp_var (name, _jkind) -> [%string "_'%{name}"]
        | Ptyp_any _jkind -> gen_symbol ~prefix:"_'a" ()
        | _ ->
          (* Type declaration parameters should only contain things like 'a or _. *)
          assert false
      in
      with_loc type_name ~loc:param.ptyp_loc)
  ;;

  (*=adds
      (type _'a _'b _'c)
      (parser_for_'a : (_'a, _) Derived_parser.t)
      (parser_for_'b : (_'b, _) Derived_parser.t)
      (parser_for_'c : (_'c, _) Derived_parser.t)
     for a type like ('a, 'b, 'c) t.
     (these [_'a] names are less likely to conflict with existing types than [a], etc.)

     This kind of polymorphism is needed to implement support for types with type
     parameters. You can see some examples in
     ppx/ppx_uri_parsing/test/inline/parsers_for_tuples.ml
  *)
  let param_inputs ~type_param_names : Ppxlib_jane.Shim.Pexp_function.function_param list =
    let open Ppxlib_jane.Shim.Pexp_function in
    let type_args =
      List.map type_param_names ~f:(fun type_name ->
        { pparam_loc = type_name.loc; pparam_desc = Pparam_newtype (type_name, None) })
    in
    let parser_args =
      List.map type_param_names ~f:(fun ({ txt; loc } as type_name) ->
        let open (val Ast_builder.make loc) in
        { pparam_loc = loc
        ; pparam_desc =
            Pparam_val
              ( Nolabel
              , None
              , [%pat?
                  ([%p ppat_var (with_loc [%string "parser_for%{txt}"] ~loc)] :
                    ( [%t ptyp_constr (Located.map_lident type_name) []]
                      , _ )
                      Derived_parser.t)] )
        })
    in
    type_args @ parser_args
  ;;

  (*=adds
     let _ = parser_for_'a in
     let _ = parser_for_'b in
     let _ = parser_for_'c in
     parser_body,
     just in case people have ghost type parameters *)
  let ignore_param_inputs ~type_param_names ~parser_body =
    List.fold_right type_param_names ~init:parser_body ~f:(fun { txt; loc } acc ->
      let ident =
        Ast_helper.Exp.ident ~loc { txt = Lident [%string "parser_for%{txt}"]; loc }
      in
      [%expr
        let _ = [%e ident] in
        [%e acc]])
  ;;

  (* just [Typed_field%{of_suffix}]/[Typed_variant%{of_suffix}] without any type
     parameters, or
     [Typed_fields_lib.S_of_S1 (Typed_field%{of_suffix}) (struct type nonrec t = a end)],
     etc. with type parameters. based off of ppx metaquot expansion *)
  let typed_module ~loc ~typed_field_or_variant ~type_param_names ~of_suffix : module_expr
    =
    let base =
      Ast_helper.Mod.ident
        ~loc
        { txt = Lident [%string "%{typed_field_or_variant}%{of_suffix}"]; loc }
    in
    if List.length type_param_names = 0
    then base
    else
      List.fold
        type_param_names
        ~init:
          (Ast_helper.Mod.apply
             ~loc
             (Ast_helper.Mod.ident
                ~loc
                (with_loc
                   (Ldot
                      ( Lident [%string "%{typed_field_or_variant}s_lib"]
                      , [%string "S_of_S%{List.length type_param_names#Int}"] ))
                   ~loc))
             base)
        ~f:(fun acc type_name ->
          let loc = type_name.loc in
          Ast_helper.Mod.apply
            ~loc
            acc
            (Ast_helper.Mod.structure
               ~loc
               [%str
                 type nonrec t =
                   [%t
                     Ast_helper.Typ.constr
                       ~loc
                       (Ast_builder.Default.Located.map_lident type_name)
                       []]]))
  ;;

  (* derives typed_fields and uri_parsing on each inline record *)
  let typed_variant_anonymous_records ~loc ~of_suffix ~constructors ~capitalize
    : module_expr
    =
    let uri_parsing =
      match capitalize with
      | None -> [%expr uri_parsing]
      | Some str ->
        let str = Ast_helper.Exp.constant (Pconst_string (str.txt, str.loc, None)) in
        [%expr uri_parsing ~capitalize:[%e str]]
    in
    let derivations =
      List.filter_map constructors ~f:(fun constructor ->
        match constructor.pcd_args with
        | Pcstr_record labels ->
          (* based on the expansion of
             [%stri type a = Typed_variant.Typed_variant_anonymous_records.a = { ... } [@@deriving typed_fields, uri_parsing]] *)
          let name = map_loc constructor.pcd_name ~f:String.lowercase in
          Ast_helper.Type.mk
            ~loc
            name
            ~manifest:
              (Ast_helper.Typ.constr
                 ~loc:name.loc
                 (map_loc name ~f:(fun name ->
                    Ldot
                      ( Ldot
                          ( Lident [%string "Typed_variant%{of_suffix}"]
                          , "Typed_variant_anonymous_records" )
                      , name )))
                 [])
            ~kind:(Ptype_record labels)
            ~attrs:
              [ Ast_helper.Attr.mk
                  ~loc
                  (with_loc "deriving" ~loc)
                  (PStr [%str typed_fields, [%e uri_parsing]])
              ]
          |> Some
        | _ -> None)
      |> Ppx_typed_fields.For_ppxs.sanitize_type_declarations ~loc
      |> List.map ~f:(fun decl ->
        Ast_helper.Str.type_
          ~loc
          Recursive
          [ (* This should only be non-empty for the last item, where we attached
               [@@deriving typed_fields, uri_parsing] ourselves. *)
            (if List.is_empty decl.ptype_attributes
             then
               { decl with
                 ptype_attributes =
                   [ Ast_helper.Attr.mk
                       ~loc
                       (with_loc "deriving" ~loc)
                       (PStr [%str [%e uri_parsing]])
                   ]
               }
             else decl)
          ])
    in
    Ast_helper.Mod.structure ~loc derivations
  ;;

  (* Removes all attributes from the type. We only call this on parts of the type that
     we're duplicating to derive things on, and the original attributes shouldn't be
     disturbed. It shouldn't be possible to have meaningful attributes erased here except
     in really strange cases like
     [type t = ([ `Foo of (bool[@sexp.bool]) ][@uri_parsing.sexpable])]. *)
  let erase_attributes core_type =
    let attribute_eraser =
      object
        inherit Ast_traverse.map
        method! attributes _attrs = []
      end
    in
    attribute_eraser#core_type core_type
  ;;

  (* converts e.g. all ['a] to [_'a] *)
  let convert_type_variables core_type =
    let type_variable_converter =
      object
        inherit Ast_traverse.map as super

        method! core_type core_type =
          match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
          | Ptyp_var (label, _) ->
            { core_type with
              ptyp_desc =
                Ptyp_constr
                  ({ txt = Lident [%string "_'%{label}"]; loc = core_type.ptyp_loc }, [])
            }
          | _ -> super#core_type core_type
      end
    in
    type_variable_converter#core_type core_type
  ;;

  let helpful_error_message ~loc =
    [%stri
      "Hint: Some parsers (e.g. for options and lists) are only compatible with value \
       parsers (e.g. for ints, bools, or sexpable values). Think about the URI you want \
       in the end, and try constructing your type to match that."]
  ;;

  (* e.g. [(_, 'kind__001_) Derived_parser.t] used for constraining function inputs and
     outputs to match so that [helpful_error_message] is printed in the right places *)
  let input_type ~loc =
    let kind = Ast_helper.Typ.var ~loc (gen_symbol ~prefix:"kind" ()) in
    [%type: (_, [%t kind]) Derived_parser.t]
  ;;

  (* e.g. [parser_for_int] if [core_type] is [int], or [parser_for_option parser_for_int]
     if [core_type] is [int option] *)
  let rec parser_for_type core_type : expression =
    let loc = core_type.ptyp_loc in
    let is_fragment = Attribute.get Attrs.fragment core_type in
    let core_type =
      match is_fragment, core_type with
      | None, _ -> core_type
      | Some (), ([%type: [%t? arg] option] | [%type: [%t? arg] Option.t]) -> arg
      | Some (), _ ->
        Location.raise_errorf
          ~loc
          "ppx_uri_parsing: [@uri_parsing.fragment] may only be used on option types"
    in
    let parser_source =
      match
        ( Attribute.has_flag Attrs.sexpable core_type
        , Attribute.has_flag Attrs.stringable core_type
        , Attribute.has_flag Attrs.binable core_type
        , Attribute.has_flag Attrs.bool core_type
        , Attribute.get Attrs.custom_parser core_type )
      with
      | true, false, false, false, None -> `Sexpable
      | false, true, false, false, None -> `Stringable
      | false, false, true, false, None -> `Binable
      | false, false, false, true, None -> `Bool
      | false, false, false, false, Some parser -> `Custom parser
      | false, false, false, false, None -> `Default
      | _, _, _, _, _ ->
        Location.raise_errorf
          ~loc
          "ppx_uri_parsing: only one of \
           [@uri_parsing.{sexpable,stringable,binable,bool,custom_parser}] can be \
           specified for a given type"
    in
    let parser =
      match
        parser_source, Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc
      with
      | `Sexpable, _ ->
        [%expr
          fun ~parse_from ~namespace:_ ->
            let module T = struct
              type t = [%t erase_attributes core_type] [@@deriving sexp]
            end
            in
            Parser_with_kind.Value_parser
              (Uri_parsing.Value_parser.sexpable (module T), parse_from)]
      | `Stringable, _ ->
        [%expr
          fun ~parse_from ~namespace:_ ->
            let module T = struct
              type t = [%t erase_attributes core_type] [@@deriving string]
            end
            in
            Parser_with_kind.Value_parser
              (Uri_parsing.Value_parser.stringable (module T), parse_from)]
      | `Binable, _ ->
        [%expr
          fun ~parse_from ~namespace:_ ->
            let module T = struct
              type t = [%t erase_attributes core_type] [@@deriving bin_io]
            end
            in
            Parser_with_kind.Value_parser
              (Uri_parsing.Value_parser.binable_via_base64 (module T), parse_from)]
      | `Bool, _ ->
        [%expr
          fun ~parse_from:_ ~namespace ->
            Parser_with_kind.Parser
              (Uri_parsing.Parser.from_query_flag
                 ~key:(Derived_parser.key_of_namespace namespace)
                 ())]
      | `Custom parser, _ ->
        let target_type = core_type |> convert_type_variables |> erase_attributes in
        [%expr
          ([%e parser]
           : ([%t target_type], _) Derived_parser.t)
           [@error_message "Hint: Did you pass in a custom parser of the correct type?"]]
      | `Default, Ptyp_var (name, _) ->
        Ast_helper.Exp.ident ~loc (with_loc (Lident [%string "parser_for_'%{name}"]) ~loc)
      | `Default, Ptyp_tuple args ->
        let parser_n =
          Ast_helper.Exp.ident
            ~loc
            (with_loc (Lident [%string "parser_for_tuple%{List.length args#Int}"]) ~loc)
        in
        List.fold args ~init:parser_n ~f:(fun acc arg ->
          match arg with
          | Some _, _ ->
            Location.raise_errorf ~loc "ppx_uri_parsing does not support labeled tuples"
          | None, core_type ->
            let input_type = input_type ~loc in
            [%expr
              ([%e acc] : [%t input_type] -> _)
                (([%e parser_for_type core_type]
                 : [%t input_type])
                 [@error_message [%%i helpful_error_message ~loc]])])
      | `Default, Ptyp_constr (name, params) ->
        let parser_for_t =
          let loc = name.loc in
          let for_suffix = suffix "for" in
          let txt =
            match name.txt with
            | Lident name -> Lident [%string "parser%{for_suffix name}"]
            | Ldot (module_, name) ->
              Ldot
                ( Ldot (module_, "Ppx_uri_parsing_lib")
                , [%string "parser%{for_suffix name}"] )
            | Lapply _ -> assert false
          in
          Ast_helper.Exp.ident ~loc { txt; loc }
        in
        List.fold params ~init:parser_for_t ~f:(fun acc core_type ->
          let input_type = input_type ~loc in
          [%expr
            ([%e acc] : [%t input_type] -> _)
              (([%e parser_for_type core_type]
               : [%t input_type])
               [@error_message [%%i helpful_error_message ~loc]])])
      | `Default, Ptyp_alias (core_type, None, _) -> parser_for_type core_type
      | `Default, Ptyp_alias (_, Some _, _) ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support type aliases"
      | `Default, Ptyp_variant _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support polymorphic variants"
      | `Default, Ptyp_arrow _ ->
        Location.raise_errorf
          ~loc
          "ppx_uri_parsing: Functions cannot be serialized into URIs."
      | `Default, Ptyp_unboxed_tuple _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support unboxed tuples"
      | `Default, Ptyp_object _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support objects"
      | `Default, Ptyp_class _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support classes"
      | `Default, Ptyp_package _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support module packages"
      | `Default, Ptyp_quote _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing does not support metaprogramming"
        (* These shouldn't be valid inside type declarations *)
      | `Default, Ptyp_any _
      | `Default, Ptyp_poly _
      | `Default, Ptyp_repr _
      | `Default, Ptyp_newlayout _
      | `Default, Ptyp_of_kind _
      | `Default, Ptyp_extension _
      | `Default, Ptyp_splice _ ->
        Location.raise_errorf ~loc "ppx_uri_parsing: is your type declaration valid?"
    in
    let operate_on_value_parser parser ~when_some ~output =
      match when_some with
      | Some expr ->
        [%expr
          fun ~parse_from ~namespace ->
            let (Parser_with_kind.Value_parser (parser, parse_from)) =
              (([%e parser]
               : (_, [ `Value_parser ]) Derived_parser.t)
               [@error_message [%%i helpful_error_message ~loc]])
                ~parse_from
                ~namespace
            in
            [%e output expr]]
      | None -> parser
    in
    parser
    |> operate_on_value_parser
         ~when_some:(Attribute.get Attrs.fallback core_type)
         ~output:(fun fallback ->
           [%expr
             Parser_with_kind.Value_parser
               ( Uri_parsing.Value_parser.fallback parser ~fallback:[%e fallback]
               , parse_from )])
    |> operate_on_value_parser ~when_some:is_fragment ~output:(fun () ->
      [%expr
        let _ = parse_from in
        Parser_with_kind.Parser (Uri_parsing.Parser.from_fragment parser)])
    |> operate_on_value_parser
         ~when_some:(Attribute.get Attrs.default core_type)
         ~output:(fun default ->
           [%expr
             let _ = parse_from in
             Parser_with_kind.Parser
               (Uri_parsing.Parser.from_query_optional_with_default
                  ~key:(Derived_parser.key_of_namespace namespace)
                  ~equal:[%equal: [%t erase_attributes core_type]]
                  parser
                  ~default:[%e default])])
  ;;

  (* We want core_type attributes to be specifiable on the field/constructor level, so
     that we can write e.g. [{ a : int [@default 0] }] instead of
     [{ a : (int[@default 0]) }]. *)
  let inherit_attrs core_type ~attrs =
    { core_type with ptyp_attributes = core_type.ptyp_attributes @ attrs }
  ;;

  (* e.g. turns (Foo of) [a * b * c] into (Foo of) [(a * b * c)] *)
  let tuple_constructor_args ~loc constructor_args ~attrs : core_type =
    let tupled_args =
      List.map constructor_args ~f:(fun arg ->
        inherit_attrs (Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg) ~attrs)
    in
    Ast_helper.Typ.tuple ~loc tupled_args
  ;;

  let singleton_namespace name ~capitalize =
    let loc = name.loc in
    let name_string =
      Ast_helper.Exp.constant
        ~loc
        (Pconst_string (capitalize_name name.txt ~capitalize, loc, None))
    in
    [%expr [ [%e name_string] ]]
  ;;

  let extract_actual_parser ~loc expr ~parse_from ~namespace : expression =
    [%expr
      Derived_parser.to_parser
        [%e expr]
        ~parse_from:[%e parse_from]
        ~namespace:[%e namespace]]
  ;;

  (* based off of the expansion of

     [%expr function | A -> [%e extract_actual_parser ...] ]
  *)
  let parser_for_field_cases ~loc ~labels ~capitalize : expression =
    let cases =
      List.map labels ~f:(fun label ->
        let loc = label.pld_loc in
        let open (val Ast_builder.make loc) in
        let parse_from =
          match
            ( Attribute.has_flag Attrs.query_record label
            , Attribute.has_flag Attrs.path_record label )
          with
          | true, false -> [%expr Query]
          | false, true -> [%expr Path]
          | false, false -> [%expr parse_from]
          | true, true ->
            Location.raise_errorf
              ~loc
              "ppx_uri_parsing: [@uri_parsing.query] and [@uri_parsing.path] are not \
               compatible"
        in
        let namespace =
          match
            Attribute.get Attrs.key_record label, Attribute.has_flag Attrs.no_key label
          with
          | Some expr, false -> [%expr [ ([%e expr] : string) ]]
          | None, true -> [%expr []]
          | None, false -> singleton_namespace label.pld_name ~capitalize
          | Some _, true ->
            Location.raise_errorf
              ~loc
              "ppx_uri_parsing: [@uri_parsing.key] and [@uri_parsing.no_key] are not \
               compatible"
        in
        let parser =
          extract_actual_parser
            ~loc
            (parser_for_type (inherit_attrs label.pld_type ~attrs:label.pld_attributes))
            ~parse_from
            ~namespace
        in
        let rhs =
          match Attribute.get Attrs.route_record label with
          | Some route ->
            [%expr Uri_parsing.Parser.with_prefix ([%e route] : string list) [%e parser]]
          | None -> parser
        in
        case
          ~lhs:
            (ppat_construct
               (map_loc label.pld_name ~f:(fun name -> Lident (String.capitalize name)))
               None)
          ~guard:None
          ~rhs)
    in
    Ppxlib_jane.Ast_builder.Default.coalesce_fun_arity
      (Ast_builder.Default.pexp_function ~loc cases)
  ;;

  let parser_for_constructor_args constructor : expression =
    let loc = constructor.pcd_loc in
    let parse_from =
      match
        ( Attribute.has_flag Attrs.query_variant constructor
        , Attribute.has_flag Attrs.path_variant constructor )
      with
      | true, false -> [%expr Query]
      | false, true -> [%expr Path]
      | false, false -> [%expr parse_from]
      | true, true ->
        Location.raise_errorf
          ~loc
          "ppx_uri_parsing: [@uri_parsing.query] and [@uri_parsing.path] are not \
           compatible"
    in
    let namespace =
      match Attribute.get Attrs.key_variant constructor with
      | Some expr -> [%expr [ ([%e expr] : string) ]]
      | None -> [%expr []]
    in
    let attrs = constructor.pcd_attributes in
    match constructor.pcd_args, constructor.pcd_res with
    | _, Some _ -> Location.raise_errorf ~loc "ppx_uri_parsing does not support GADTs"
    | Pcstr_tuple args, None ->
      (match args with
       | [] -> [%expr Uri_parsing.Parser.unit]
       | [ arg ] ->
         let pca_type = Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg in
         let loc = pca_type.ptyp_loc in
         pca_type
         |> inherit_attrs ~attrs
         |> parser_for_type
         |> extract_actual_parser ~loc ~parse_from ~namespace
       | _ ->
         args
         |> tuple_constructor_args ~loc ~attrs
         |> parser_for_type
         |> extract_actual_parser ~loc ~parse_from ~namespace)
    | Pcstr_record _, None ->
      let for_suffix = suffix "for" (String.lowercase constructor.pcd_name.txt) in
      (* These parsers are constructed in the [typed_variant_anonymous_records] function
         above. *)
      let inline_record_parser =
        Ast_helper.Exp.ident
          ~loc
          (with_loc
             (Ldot
                ( Ldot (Lident "Typed_variant_anonymous_records", "Ppx_uri_parsing_lib")
                , [%string "parser%{for_suffix}"] ))
             ~loc)
      in
      inline_record_parser |> extract_actual_parser ~loc ~parse_from ~namespace
  ;;

  (* based off of the expansion of

     [%expr function | A -> Uri_parsing.Parser.with_prefix [ "a" ] [%e parser_for_constructor_args ...]) ]
  *)
  let parser_for_variant_cases ~loc ~constructors ~capitalize : expression =
    let cases =
      List.map constructors ~f:(fun constructor ->
        let loc = constructor.pcd_loc in
        let rhs =
          match
            ( Attribute.has_flag Attrs.index constructor
            , Attribute.get Attrs.route_variant constructor )
          with
          | true, None ->
            [%expr
              Uri_parsing.Parser.end_of_path [%e parser_for_constructor_args constructor]]
          | false, Some route ->
            [%expr
              Uri_parsing.Parser.with_prefix
                ([%e route] : string list)
                [%e parser_for_constructor_args constructor]]
          | false, None ->
            [%expr
              Uri_parsing.Parser.with_prefix
                [ [%e
                    Ast_helper.Exp.constant
                      ~loc:constructor.pcd_name.loc
                      (Pconst_string
                         ( capitalize_name constructor.pcd_name.txt ~capitalize
                         , constructor.pcd_name.loc
                         , None ))]
                ]
                [%e parser_for_constructor_args constructor]]
          | true, Some _ ->
            Location.raise_errorf
              ~loc
              "ppx_uri_parsing: [@uri_parsing.index] and [@uri_parsing.route] are not \
               compatible"
        in
        Ast_builder.Default.case
          ~lhs:
            (Ast_helper.Pat.construct
               ~loc
               (map_loc constructor.pcd_name ~f:(fun name ->
                  Lident (String.capitalize name)))
               None)
          ~guard:None
          ~rhs)
    in
    Ppxlib_jane.Ast_builder.Default.coalesce_fun_arity
      (Ast_builder.Default.pexp_function ~loc cases)
  ;;

  (* based off of the expansion of [%expr [ A; B ]] *)
  let path_order_constructors ~loc ~labels : expression =
    List.fold_right labels ~init:[%expr []] ~f:(fun label acc ->
      [%expr
        [%e
          Ast_helper.Exp.construct
            ~loc
            (map_loc label.pld_name ~f:(fun name -> Lident (String.capitalize name)))
            None]
        :: [%e acc]])
  ;;

  (* based off of the expansion of
     [%stri let parser : (_, _) Derived_parser.t = fun ~parse_from ~namespace -> [%e parser_body]]
     and
     [%stri let parser args : (_, _) Derived_parser.t = fun ~parse_from ~namespace -> [%e parser_body]] *)
  let derived_parser ~loc ~for_suffix ~type_param_names ~parser_body : structure_item =
    let open (val Ast_builder.make loc) in
    let type_constraint = [%type: (_, _) Derived_parser.t] in
    let parser_pattern = ppat_var (with_loc [%string "parser%{for_suffix}"] ~loc) in
    let body =
      Ppxlib_jane.Ast_builder.Default.coalesce_fun_arity
        (pexp_fun
           (Labelled "parse_from")
           None
           [%pat? parse_from]
           (pexp_fun
              (Labelled "namespace")
              None
              [%pat? namespace]
              (ignore_param_inputs ~type_param_names ~parser_body)))
    in
    match param_inputs ~type_param_names with
    | [] -> [%stri let [%p parser_pattern] : [%t type_constraint] = [%e body]]
    | params ->
      let fn =
        { pexp_desc =
            Ppxlib_jane.Shim.Pexp_function.to_parsetree
              ~params
              ~constraint_:
                { mode_annotations = []
                ; ret_mode_annotations = []
                ; ret_type_constraint = Some (Pconstraint type_constraint)
                }
              ~body:(Pfunction_body body)
        ; pexp_loc = loc
        ; pexp_loc_stack = []
        ; pexp_attributes = []
        }
      in
      [%stri
        let [%p parser_pattern] =
          [%e Ppxlib_jane.Ast_builder.Default.coalesce_fun_arity fn]
        ;;]
  ;;

  let parser ~loc ~for_suffix =
    let open (val Ast_builder.make loc) in
    let parser_pattern = ppat_var (with_loc [%string "parser%{for_suffix}"] ~loc) in
    let parser_value =
      pexp_ident
        (with_loc
           (Ldot (Lident "Ppx_uri_parsing_lib", [%string "parser%{for_suffix}"]))
           ~loc)
    in
    [%stri
      let [%p parser_pattern] =
        Ppx_uri_parsing_lib.Derived_parser.to_parser
          [%e parser_value]
          ~parse_from:
            (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
          ~namespace:[]
      ;;]
  ;;

  let structure ~loc ~path:_ (rec_flag, type_decls) capitalize =
    match type_decls with
    | [ type_decl ] ->
      (match rec_flag with
       | Recursive -> check_for_recursive_type type_decl
       | _ -> ());
      let type_name = type_decl.ptype_name.txt in
      let type_param_names = type_param_names type_decl.ptype_params in
      let for_suffix = suffix "for" type_name in
      let of_suffix = suffix "of" type_name in
      (* body of Ppx_uri_parsing_lib.parser *)
      let parser_body =
        match
          ( Ppxlib_jane.Shim.Type_kind.of_parsetree type_decl.ptype_kind
          , type_decl.ptype_manifest )
        with
        | Ptype_variant constructors, _ ->
          [%expr
            let module Module_private_for_ppx_uri_parsing = struct
              module Typed_variant_anonymous_records =
                [%m
                typed_variant_anonymous_records ~loc ~of_suffix ~constructors ~capitalize]

              module Typed_variant =
                [%m
                typed_module
                  ~loc
                  ~typed_field_or_variant:"Typed_variant"
                  ~type_param_names
                  ~of_suffix]

              let parse_from =
                match parse_from with
                | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
                | _ -> parse_from
              ;;

              let parser_for_variant
                : type variant_argument_type.
                  variant_argument_type Typed_variant.t
                  -> variant_argument_type Uri_parsing.Parser.t
                =
                [%e parser_for_variant_cases ~loc ~constructors ~capitalize]
              ;;
            end
            in
            Parser_with_kind.Parser
              (Uri_parsing.Parser.Variant.make
                 ~namespace
                 (module Module_private_for_ppx_uri_parsing))]
        | Ptype_record labels, _ ->
          [%expr
            let module Module_private_for_ppx_uri_parsing = struct
              module Typed_field =
                [%m
                typed_module
                  ~loc
                  ~typed_field_or_variant:"Typed_field"
                  ~type_param_names
                  ~of_suffix]

              let parse_from =
                match parse_from with
                | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
                | _ -> parse_from
              ;;

              let parser_for_field
                : type field_argument_type.
                  field_argument_type Typed_field.t
                  -> field_argument_type Uri_parsing.Parser.t
                =
                [%e parser_for_field_cases ~loc ~labels ~capitalize]
              ;;

              module Path_order = Uri_parsing.Path_order (Typed_field)

              let path_order = Path_order.T [%e path_order_constructors ~loc ~labels]
            end
            in
            Parser_with_kind.Parser
              (Uri_parsing.Parser.Record.make
                 ~namespace
                 (module Module_private_for_ppx_uri_parsing))]
        | (Ptype_abstract | Ptype_open | Ptype_record_unboxed_product _), Some core_type
          -> [%expr [%e parser_for_type core_type] ~parse_from ~namespace]
        | Ptype_abstract, None ->
          Location.raise_errorf
            ~loc
            "ppx_uri_parsing: did you forget to define your type?"
        | (Ptype_open | Ptype_record_unboxed_product _), None ->
          Location.raise_errorf
            ~loc
            "ppx_uri_parsing only supports variant and record types"
      in
      (* module Ppx_uri_parsing_lib = ... *)
      let ppx_uri_parsing_lib =
        [%stri
          module Ppx_uri_parsing_lib = struct
            include Ppx_uri_parsing_lib

            [%%i derived_parser ~loc ~for_suffix ~type_param_names ~parser_body]
          end]
      in
      (* let parser%{for_suffix} = ... *)
      let parser =
        if List.is_empty type_decl.ptype_params then [ parser ~loc ~for_suffix ] else []
      in
      ppx_uri_parsing_lib :: parser
    | [] -> Location.raise_errorf ~loc "ppx_uri_parsing does not support empty types"
    | _ :: _ :: _ ->
      Location.raise_errorf
        ~loc
        "ppx_uri_parsing does not support mutually recursive types"
  ;;
end

let signature = Signature.signature
let structure = Structure.structure
