open! Core

module Simple_record = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field = Typed_field

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_field
            : type field_argument_type.
              field_argument_type Typed_field.t
              -> field_argument_type Uri_parsing.Parser.t
            = function
            | A -> Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[ "a" ]
            | B ->
              Derived_parser.to_parser parser_for_string ~parse_from ~namespace:[ "b" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ A; B ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module Record_with_type_parameters = struct
  type ('a, 'b) t =
    { a : 'a
    ; b : 'b
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b) t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser
        (type _'a _'b)
        (parser_for_'a : (_'a, _) Derived_parser.t)
        (parser_for_'b : (_'b, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let _ = parser_for_'b in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field =
            Typed_fields_lib.S_of_S2
              (Typed_field)
              (struct
                type nonrec t = _'a
              end)
              (struct
                type nonrec t = _'b
              end)

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_field
            : type field_argument_type.
              field_argument_type Typed_field.t
              -> field_argument_type Uri_parsing.Parser.t
            = function
            | A -> Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[ "a" ]
            | B -> Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[ "b" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ A; B ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Simple variant"] _ = struct
  type t =
    | A of int
    | B of string
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Uri_parsing.Parser.with_prefix
                [ "a" ]
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
            | B ->
              Uri_parsing.Parser.with_prefix
                [ "b" ]
                (Derived_parser.to_parser parser_for_string ~parse_from ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Variant with type parameters"] _ = struct
  type ('a, 'b) t =
    | A of 'a
    | B of 'b
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b) t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser
        (type _'a _'b)
        (parser_for_'a : (_'a, _) Derived_parser.t)
        (parser_for_'b : (_'b, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let _ = parser_for_'b in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end

          module Typed_variant =
            Typed_variants_lib.S_of_S2
              (Typed_variant)
              (struct
                type nonrec t = _'a
              end)
              (struct
                type nonrec t = _'b
              end)

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Uri_parsing.Parser.with_prefix
                [ "a" ]
                (Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[])
            | B ->
              Uri_parsing.Parser.with_prefix
                [ "b" ]
                (Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Abstract w/ manifest"] _ = struct
  type t = int [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace -> parser_for_int ~parse_from ~namespace
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Inline record"] _ = struct
  type t =
    | T of { foo : int }
    | B of
        { foo : int
        ; bar : string option * int
        }
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct
            type t = Typed_variant.Typed_variant_anonymous_records.t = { foo : int }
            [@@deriving typed_fields, uri_parsing]

            type b = Typed_variant.Typed_variant_anonymous_records.b =
              { foo : int
              ; bar : string option * int
              }
            [@@deriving typed_fields, uri_parsing]
          end

          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | T ->
              Uri_parsing.Parser.with_prefix
                [ "t" ]
                (Derived_parser.to_parser
                   Typed_variant_anonymous_records.Ppx_uri_parsing_lib.parser
                   ~parse_from
                   ~namespace:[])
            | B ->
              Uri_parsing.Parser.with_prefix
                [ "b" ]
                (Derived_parser.to_parser
                   Typed_variant_anonymous_records.Ppx_uri_parsing_lib.parser_for_b
                   ~parse_from
                   ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Sexpable"] _ = struct
  type t =
    | Simple of (Time_ns.t[@sexpable])
    | Nested of (int[@sexpable]) option
    | Type_parameter of (int option[@sexpable])
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | Simple ->
              Uri_parsing.Parser.with_prefix
                [ "simple" ]
                (Derived_parser.to_parser
                   (fun ~parse_from ~namespace:_ ->
                     let module T = struct
                       type t = Time_ns.t [@@deriving sexp]
                     end
                     in
                     Parser_with_kind.Value_parser
                       (Uri_parsing.Value_parser.sexpable (module T), parse_from))
                   ~parse_from
                   ~namespace:[])
            | Nested ->
              Uri_parsing.Parser.with_prefix
                [ "nested" ]
                (Derived_parser.to_parser
                   ((parser_for_option : (_, 'kind__004_) Derived_parser.t -> _)
                      ((fun ~parse_from ~namespace:_ ->
                          let module T = struct
                            type t = int [@@deriving sexp]
                          end
                          in
                          Parser_with_kind.Value_parser
                            (Uri_parsing.Value_parser.sexpable (module T), parse_from)
                       : (_, 'kind__004_) Derived_parser.t)
                       [@error_message
                         "Hint: Some parsers (e.g. for options and lists) are only \
                          compatible with value parsers (e.g. for ints, bools, or \
                          sexpable values). Think about the URI you want in the end, and \
                          try constructing your type to match that."]))
                   ~parse_from
                   ~namespace:[])
            | Type_parameter ->
              Uri_parsing.Parser.with_prefix
                [ "type-parameter" ]
                (Derived_parser.to_parser
                   (fun ~parse_from ~namespace:_ ->
                     let module T = struct
                       type t = int option [@@deriving sexp]
                     end
                     in
                     Parser_with_kind.Value_parser
                       (Uri_parsing.Value_parser.sexpable (module T), parse_from))
                   ~parse_from
                   ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Stringable"] _ = struct
  type t =
    | Simple of (Time_ns.t[@stringable])
    | Nested of (int[@stringable]) option
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | Simple ->
              Uri_parsing.Parser.with_prefix
                [ "simple" ]
                (Derived_parser.to_parser
                   (fun ~parse_from ~namespace:_ ->
                     let module T = struct
                       type t = Time_ns.t [@@deriving string]
                     end
                     in
                     Parser_with_kind.Value_parser
                       (Uri_parsing.Value_parser.stringable (module T), parse_from))
                   ~parse_from
                   ~namespace:[])
            | Nested ->
              Uri_parsing.Parser.with_prefix
                [ "nested" ]
                (Derived_parser.to_parser
                   ((parser_for_option : (_, 'kind__010_) Derived_parser.t -> _)
                      ((fun ~parse_from ~namespace:_ ->
                          let module T = struct
                            type t = int [@@deriving string]
                          end
                          in
                          Parser_with_kind.Value_parser
                            (Uri_parsing.Value_parser.stringable (module T), parse_from)
                       : (_, 'kind__010_) Derived_parser.t)
                       [@error_message
                         "Hint: Some parsers (e.g. for options and lists) are only \
                          compatible with value parsers (e.g. for ints, bools, or \
                          sexpable values). Think about the URI you want in the end, and \
                          try constructing your type to match that."]))
                   ~parse_from
                   ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Binable"] _ = struct
  type t =
    | Simple of (Time_ns.t[@binable])
    | Nested of (int[@binable]) option
    | Type_parameter of (int option[@binable])
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | Simple ->
              Uri_parsing.Parser.with_prefix
                [ "simple" ]
                (Derived_parser.to_parser
                   (fun ~parse_from ~namespace:_ ->
                     let module T = struct
                       type t = Time_ns.t [@@deriving bin_io]
                     end
                     in
                     Parser_with_kind.Value_parser
                       (Uri_parsing.Value_parser.binable_via_base64 (module T), parse_from))
                   ~parse_from
                   ~namespace:[])
            | Nested ->
              Uri_parsing.Parser.with_prefix
                [ "nested" ]
                (Derived_parser.to_parser
                   ((parser_for_option : (_, 'kind__011_) Derived_parser.t -> _)
                      ((fun ~parse_from ~namespace:_ ->
                          let module T = struct
                            type t = int [@@deriving bin_io]
                          end
                          in
                          Parser_with_kind.Value_parser
                            ( Uri_parsing.Value_parser.binable_via_base64 (module T)
                            , parse_from )
                       : (_, 'kind__011_) Derived_parser.t)
                       [@error_message
                         "Hint: Some parsers (e.g. for options and lists) are only \
                          compatible with value parsers (e.g. for ints, bools, or \
                          sexpable values). Think about the URI you want in the end, and \
                          try constructing your type to match that."]))
                   ~parse_from
                   ~namespace:[])
            | Type_parameter ->
              Uri_parsing.Parser.with_prefix
                [ "type-parameter" ]
                (Derived_parser.to_parser
                   (fun ~parse_from ~namespace:_ ->
                     let module T = struct
                       type t = int option [@@deriving bin_io]
                     end
                     in
                     Parser_with_kind.Value_parser
                       (Uri_parsing.Value_parser.binable_via_base64 (module T), parse_from))
                   ~parse_from
                   ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Query & path"] _ = struct
  type t =
    | A of int
    | B of int [@query]
    | C of int [@path]
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Uri_parsing.Parser.with_prefix
                [ "a" ]
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
            | B ->
              Uri_parsing.Parser.with_prefix
                [ "b" ]
                (Derived_parser.to_parser parser_for_int ~parse_from:Query ~namespace:[])
            | C ->
              Uri_parsing.Parser.with_prefix
                [ "c" ]
                (Derived_parser.to_parser parser_for_int ~parse_from:Path ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Routes"] _ = struct
  type t =
    | A of int
    | B of int [@index]
    | C of int [@route [ "foo"; "bar" ]]
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Uri_parsing.Parser.with_prefix
                [ "a" ]
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
            | B ->
              Uri_parsing.Parser.end_of_path
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
            | C ->
              Uri_parsing.Parser.with_prefix
                ([ "foo"; "bar" ] : string list)
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Defaults & flags"] _ = struct
  type t =
    { a : bool
    ; b : (bool[@bool])
    ; c : (bool[@default true])
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field = Typed_field

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_field
            : type field_argument_type.
              field_argument_type Typed_field.t
              -> field_argument_type Uri_parsing.Parser.t
            = function
            | A -> Derived_parser.to_parser parser_for_bool ~parse_from ~namespace:[ "a" ]
            | B ->
              Derived_parser.to_parser
                (fun ~parse_from:_ ~namespace ->
                  Parser_with_kind.Parser
                    (Uri_parsing.Parser.from_query_flag
                       ~key:(Derived_parser.key_of_namespace namespace)
                       ()))
                ~parse_from
                ~namespace:[ "b" ]
            | C ->
              Derived_parser.to_parser
                (fun ~parse_from ~namespace ->
                  let (Parser_with_kind.Value_parser (parser, parse_from)) =
                    ((parser_for_bool
                     : (_, [ `Value_parser ]) Derived_parser.t)
                     [@error_message
                       "Hint: Some parsers (e.g. for options and lists) are only \
                        compatible with value parsers (e.g. for ints, bools, or sexpable \
                        values). Think about the URI you want in the end, and try \
                        constructing your type to match that."])
                      ~parse_from
                      ~namespace
                  in
                  let _ = parse_from in
                  Parser_with_kind.Parser
                    (Uri_parsing.Parser.from_query_optional_with_default
                       ~key:(Derived_parser.key_of_namespace namespace)
                       ~equal:[%equal: bool]
                       parser
                       ~default:true))
                ~parse_from
                ~namespace:[ "c" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ A; B; C ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Keep namespace"] _ = struct
  type t =
    | A of int
    | B of int [@key "b"]
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end
          module Typed_variant = Typed_variant

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Uri_parsing.Parser.with_prefix
                [ "a" ]
                (Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[])
            | B ->
              Uri_parsing.Parser.with_prefix
                [ "b" ]
                (Derived_parser.to_parser
                   parser_for_int
                   ~parse_from
                   ~namespace:[ ("b" : string) ])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Skip namespace"] _ = struct
  type t =
    { a : int
    ; b : int [@no_key]
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field = Typed_field

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_field
            : type field_argument_type.
              field_argument_type Typed_field.t
              -> field_argument_type Uri_parsing.Parser.t
            = function
            | A -> Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[ "a" ]
            | B -> Derived_parser.to_parser parser_for_int ~parse_from ~namespace:[]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ A; B ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Custom parser"] _ = struct
  let tuple2_option_parser parser_1 parser_2 ~parse_from ~namespace =
    let open Ppx_uri_parsing_lib in
    Parser_with_kind.Parser
      (Uri_parsing.Parser.optional_query_fields
         (Derived_parser.to_parser
            (parser_for_tuple2 parser_1 parser_2)
            ~parse_from
            ~namespace))
  ;;

  type 'a t =
    | Foo of
        (('a * bool) option
        [@uri_parsing.custom_parser tuple2_option_parser parser_for_'a parser_for_bool])
  [@@deriving typed_variants] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : 'a t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser (type _'a) (parser_for_'a : (_'a, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_variant_anonymous_records = struct end

          module Typed_variant =
            Typed_variants_lib.S_of_S1
              (Typed_variant)
              (struct
                type nonrec t = _'a
              end)

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Path
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_variant
            : type variant_argument_type.
              variant_argument_type Typed_variant.t
              -> variant_argument_type Uri_parsing.Parser.t
            = function
            | Foo ->
              Uri_parsing.Parser.with_prefix
                [ "foo" ]
                (Derived_parser.to_parser
                   ((tuple2_option_parser parser_for_'a parser_for_bool
                    : ((_'a * bool) option, _) Derived_parser.t)
                    [@error_message
                      "Hint: Did you pass in a custom parser of the correct type?"])
                   ~parse_from
                   ~namespace:[])
          ;;

          let _ = parser_for_variant
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Variant.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "Fragment"] _ = struct
  type t =
    { a : int option
    ; b : int option [@fragment]
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser : (_, _) Derived_parser.t =
        fun ~parse_from ~namespace ->
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field = Typed_field

          let parse_from =
            match parse_from with
            | Parse_from.Default _ -> Parse_from.Default Tiebreaker.Query
            | _ -> parse_from
          ;;

          let _ = parse_from

          let parser_for_field
            : type field_argument_type.
              field_argument_type Typed_field.t
              -> field_argument_type Uri_parsing.Parser.t
            = function
            | A ->
              Derived_parser.to_parser
                ((parser_for_option : (_, 'kind__014_) Derived_parser.t -> _)
                   ((parser_for_int
                    : (_, 'kind__014_) Derived_parser.t)
                    [@error_message
                      "Hint: Some parsers (e.g. for options and lists) are only \
                       compatible with value parsers (e.g. for ints, bools, or sexpable \
                       values). Think about the URI you want in the end, and try \
                       constructing your type to match that."]))
                ~parse_from
                ~namespace:[ "a" ]
            | B ->
              Derived_parser.to_parser
                (fun ~parse_from ~namespace ->
                  let (Parser_with_kind.Value_parser (parser, parse_from)) =
                    ((parser_for_int
                     : (_, [ `Value_parser ]) Derived_parser.t)
                     [@error_message
                       "Hint: Some parsers (e.g. for options and lists) are only \
                        compatible with value parsers (e.g. for ints, bools, or sexpable \
                        values). Think about the URI you want in the end, and try \
                        constructing your type to match that."])
                      ~parse_from
                      ~namespace
                  in
                  let _ = parse_from in
                  Parser_with_kind.Parser (Uri_parsing.Parser.from_fragment parser))
                ~parse_from
                ~namespace:[ "b" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ A; B ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser
    end

    let parser =
      Ppx_uri_parsing_lib.Derived_parser.to_parser
        Ppx_uri_parsing_lib.parser
        ~parse_from:
          (Ppx_uri_parsing_lib.Parse_from.Default Ppx_uri_parsing_lib.Tiebreaker.Path)
        ~namespace:[]
    ;;

    let _ = parser
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end
