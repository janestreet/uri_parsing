open! Core

module Tiebreaker = struct
  type t =
    | Path
    | Query
end

module Parse_from = struct
  type t =
    | Path
    | Default of Tiebreaker.t
    | Query
end

module Parser_with_kind = struct
  type ('a, 'kind) t =
    | Parser : 'a Uri_parsing.Parser.t -> ('a, [ `Parser ]) t
    | Value_parser :
        'a Uri_parsing.Value_parser.t * Parse_from.t
        -> ('a, [ `Value_parser ]) t

  type 'a packed = T : ('a, _) t -> 'a packed
end

module Derived_parser = struct
  type ('a, 'kind) t =
    parse_from:Parse_from.t
    -> namespace:string list
    -> ('a, ([< `Parser | `Value_parser ] as 'kind)) Parser_with_kind.t

  let key_of_namespace = function
    | [] -> "value"
    | namespace -> List.last_exn namespace
  ;;

  let to_parser t ~parse_from ~namespace =
    let packed = Parser_with_kind.T (t ~parse_from ~namespace) in
    match packed with
    | T (Parser parser) -> parser
    | T (Value_parser (value_parser, parse_from)) ->
      (match parse_from with
       | Path | Default Path -> Uri_parsing.Parser.from_path value_parser
       | Query | Default Query ->
         Uri_parsing.Parser.from_query_required
           ~key:(key_of_namespace namespace)
           value_parser)
  ;;
end

let parser_for_int ~parse_from ~namespace:_ =
  Parser_with_kind.Value_parser (Uri_parsing.Value_parser.int, parse_from)
;;

let parser_for_string ~parse_from ~namespace:_ =
  Parser_with_kind.Value_parser (Uri_parsing.Value_parser.string, parse_from)
;;

let parser_for_float ~parse_from ~namespace:_ =
  Parser_with_kind.Value_parser (Uri_parsing.Value_parser.float, parse_from)
;;

let parser_for_bool ~parse_from ~namespace:_ =
  Parser_with_kind.Value_parser (Uri_parsing.Value_parser.bool, parse_from)
;;

let parser_for_option
  (type a)
  (parser_for_a : (a, [ `Value_parser ]) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let (Parser_with_kind.Value_parser (value_parser_for_a, _)) =
    parser_for_a ~parse_from ~namespace
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.from_query_optional
       ~key:(Derived_parser.key_of_namespace namespace)
       value_parser_for_a)
;;

let parser_for_list
  (type a)
  (parser_for_a : (a, [ `Value_parser ]) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let (Parser_with_kind.Value_parser (value_parser_for_a, _)) =
    parser_for_a ~parse_from ~namespace
  in
  match parse_from with
  | Path ->
    (* We don't want to do this in the default case since it's not compatible with other
       path parsers, but we still want it to be expressible. *)
    Parser_with_kind.Parser (Uri_parsing.Parser.from_remaining_path value_parser_for_a)
  | _ ->
    Parser_with_kind.Parser
      (Uri_parsing.Parser.from_query_many
         ~key:(Derived_parser.key_of_namespace namespace)
         value_parser_for_a)
;;

(* Modified versions of ppx-generated code. *)

let parser_for_tuple2
  (type a b)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple2 =
      { first : a
      ; second : b
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple2

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second } -> first, second)
       ~unparse:(fun (first, second) -> { first; second }))
;;

let parser_for_tuple3
  (type a b c)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  (parser_for_c : (c, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple3 =
      { first : a
      ; second : b
      ; third : c
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple3

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
      | Third -> Derived_parser.to_parser parser_for_c ~parse_from ~namespace:[ "third" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second; third } -> first, second, third)
       ~unparse:(fun (first, second, third) -> { first; second; third }))
;;

let parser_for_tuple4
  (type a b c d)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  (parser_for_c : (c, _) Derived_parser.t)
  (parser_for_d : (d, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple4 =
      { first : a
      ; second : b
      ; third : c
      ; fourth : d
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple4

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
      | Third -> Derived_parser.to_parser parser_for_c ~parse_from ~namespace:[ "third" ]
      | Fourth ->
        Derived_parser.to_parser parser_for_d ~parse_from ~namespace:[ "fourth" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third; Fourth ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second; third; fourth } -> first, second, third, fourth)
       ~unparse:(fun (first, second, third, fourth) -> { first; second; third; fourth }))
;;

let parser_for_tuple5
  (type a b c d e)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  (parser_for_c : (c, _) Derived_parser.t)
  (parser_for_d : (d, _) Derived_parser.t)
  (parser_for_e : (e, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple5 =
      { first : a
      ; second : b
      ; third : c
      ; fourth : d
      ; fifth : e
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple5

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
      | Third -> Derived_parser.to_parser parser_for_c ~parse_from ~namespace:[ "third" ]
      | Fourth ->
        Derived_parser.to_parser parser_for_d ~parse_from ~namespace:[ "fourth" ]
      | Fifth -> Derived_parser.to_parser parser_for_e ~parse_from ~namespace:[ "fifth" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third; Fourth; Fifth ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second; third; fourth; fifth } ->
         first, second, third, fourth, fifth)
       ~unparse:(fun (first, second, third, fourth, fifth) ->
         { first; second; third; fourth; fifth }))
;;

let parser_for_tuple6
  (type a b c d e f)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  (parser_for_c : (c, _) Derived_parser.t)
  (parser_for_d : (d, _) Derived_parser.t)
  (parser_for_e : (e, _) Derived_parser.t)
  (parser_for_f : (f, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple6 =
      { first : a
      ; second : b
      ; third : c
      ; fourth : d
      ; fifth : e
      ; sixth : f
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple6

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
      | Third -> Derived_parser.to_parser parser_for_c ~parse_from ~namespace:[ "third" ]
      | Fourth ->
        Derived_parser.to_parser parser_for_d ~parse_from ~namespace:[ "fourth" ]
      | Fifth -> Derived_parser.to_parser parser_for_e ~parse_from ~namespace:[ "fifth" ]
      | Sixth -> Derived_parser.to_parser parser_for_f ~parse_from ~namespace:[ "sixth" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third; Fourth; Fifth; Sixth ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second; third; fourth; fifth; sixth } ->
         first, second, third, fourth, fifth, sixth)
       ~unparse:(fun (first, second, third, fourth, fifth, sixth) ->
         { first; second; third; fourth; fifth; sixth }))
;;

let parser_for_tuple7
  (type a b c d e f g)
  (parser_for_a : (a, _) Derived_parser.t)
  (parser_for_b : (b, _) Derived_parser.t)
  (parser_for_c : (c, _) Derived_parser.t)
  (parser_for_d : (d, _) Derived_parser.t)
  (parser_for_e : (e, _) Derived_parser.t)
  (parser_for_f : (f, _) Derived_parser.t)
  (parser_for_g : (g, _) Derived_parser.t)
  ~parse_from
  ~namespace
  =
  let module Module_private_for_ppx_uri_parsing = struct
    type tuple7 =
      { first : a
      ; second : b
      ; third : c
      ; fourth : d
      ; fifth : e
      ; sixth : f
      ; seventh : g
      }
    [@@deriving typed_fields]

    module Typed_field = Typed_field_of_tuple7

    let parser_for_field
      : type tuple_slot_type.
        tuple_slot_type Typed_field.t -> tuple_slot_type Uri_parsing.Parser.t
      = function
      | First -> Derived_parser.to_parser parser_for_a ~parse_from ~namespace:[ "first" ]
      | Second ->
        Derived_parser.to_parser parser_for_b ~parse_from ~namespace:[ "second" ]
      | Third -> Derived_parser.to_parser parser_for_c ~parse_from ~namespace:[ "third" ]
      | Fourth ->
        Derived_parser.to_parser parser_for_d ~parse_from ~namespace:[ "fourth" ]
      | Fifth -> Derived_parser.to_parser parser_for_e ~parse_from ~namespace:[ "fifth" ]
      | Sixth -> Derived_parser.to_parser parser_for_f ~parse_from ~namespace:[ "sixth" ]
      | Seventh ->
        Derived_parser.to_parser parser_for_g ~parse_from ~namespace:[ "seventh" ]
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)

    let path_order = Path_order.T [ First; Second; Third; Fourth; Fifth; Sixth; Seventh ]
  end
  in
  Parser_with_kind.Parser
    (Uri_parsing.Parser.project
       (Uri_parsing.Parser.Record.make
          ~namespace
          (module Module_private_for_ppx_uri_parsing))
       ~parse_exn:(fun { first; second; third; fourth; fifth; sixth; seventh } ->
         first, second, third, fourth, fifth, sixth, seventh)
       ~unparse:(fun (first, second, third, fourth, fifth, sixth, seventh) ->
         { first; second; third; fourth; fifth; sixth; seventh }))
;;

module type S = sig
  type t

  val parser : t Uri_parsing.Parser.t

  module Ppx_uri_parsing_lib : sig
    val parser : (t, [ `Parser ]) Derived_parser.t
  end
end

module type S_value_parser = sig
  type t

  val parser : t Uri_parsing.Parser.t

  module Ppx_uri_parsing_lib : sig
    val parser : (t, [ `Value_parser ]) Derived_parser.t
  end
end

module Make_sexpable (M : sig
    type t [@@deriving sexp]
  end) =
struct
  module Ppx_uri_parsing_lib = struct
    let parser ~parse_from ~namespace:_ =
      Parser_with_kind.Value_parser
        (Uri_parsing.Value_parser.sexpable (module M), parse_from)
    ;;
  end

  let parser =
    Derived_parser.to_parser
      Ppx_uri_parsing_lib.parser
      (* These are the defaults for non-record/variant types (see
         ppx/ppx_uri_parsing/test/inline/test_inline.ml:311), so they match what happens
         when you just have e.g. type t = Foo.t [@uri_parsing.sexpable]. *)
      ~parse_from:(Parse_from.Default Tiebreaker.Path)
      ~namespace:[]
  ;;
end

module Make_stringable (M : sig
    type t [@@deriving string]
  end) =
struct
  module Ppx_uri_parsing_lib = struct
    let parser ~parse_from ~namespace:_ =
      Parser_with_kind.Value_parser
        (Uri_parsing.Value_parser.stringable (module M), parse_from)
    ;;
  end

  let parser =
    Derived_parser.to_parser
      Ppx_uri_parsing_lib.parser
      ~parse_from:(Parse_from.Default Tiebreaker.Path)
      ~namespace:[]
  ;;
end

module Make_binable (M : sig
    type t [@@deriving bin_io]
  end) =
struct
  module Ppx_uri_parsing_lib = struct
    let parser ~parse_from ~namespace:_ =
      Parser_with_kind.Value_parser
        (Uri_parsing.Value_parser.binable_via_base64 (module M), parse_from)
    ;;
  end

  let parser =
    Derived_parser.to_parser
      Ppx_uri_parsing_lib.parser
      ~parse_from:(Parse_from.Default Tiebreaker.Path)
      ~namespace:[]
  ;;
end

module Make_from_value_parser (M : sig
    type t

    val value_parser : t Uri_parsing.Value_parser.t
  end) =
struct
  module Ppx_uri_parsing_lib = struct
    let parser ~parse_from ~namespace:_ =
      Parser_with_kind.Value_parser (M.value_parser, parse_from)
    ;;
  end

  let parser =
    Derived_parser.to_parser
      Ppx_uri_parsing_lib.parser
      ~parse_from:(Parse_from.Default Tiebreaker.Path)
      ~namespace:[]
  ;;
end

module Make_from_parser (M : sig
    type t

    val parser : t Uri_parsing.Parser.t
  end) =
struct
  module Ppx_uri_parsing_lib = struct
    let parser ~parse_from:_ ~namespace:_ = Parser_with_kind.Parser M.parser
  end
end
