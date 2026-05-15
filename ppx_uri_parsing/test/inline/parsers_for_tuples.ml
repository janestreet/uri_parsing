open! Core

(* This file is the "source" for parser_for_tuple2, etc. *)

module%test [@name "parser_for_tuple2"] _ = struct
  type ('a, 'b) tuple2 =
    { first : 'a
    ; second : 'b
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b) tuple2) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser_for_tuple2
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
              (Typed_field_of_tuple2)
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
            | First ->
              Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[ "first" ]
            | Second ->
              Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[ "second" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ First; Second ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser_for_tuple2
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "parser_for_tuple3"] _ = struct
  type ('a, 'b, 'c) tuple3 =
    { first : 'a
    ; second : 'b
    ; third : 'c
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b, 'c) tuple3) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser_for_tuple3
        (type _'a _'b _'c)
        (parser_for_'a : (_'a, _) Derived_parser.t)
        (parser_for_'b : (_'b, _) Derived_parser.t)
        (parser_for_'c : (_'c, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let _ = parser_for_'b in
        let _ = parser_for_'c in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field =
            Typed_fields_lib.S_of_S3
              (Typed_field_of_tuple3)
              (struct
                type nonrec t = _'a
              end)
              (struct
                type nonrec t = _'b
              end)
              (struct
                type nonrec t = _'c
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
            | First ->
              Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[ "first" ]
            | Second ->
              Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[ "second" ]
            | Third ->
              Derived_parser.to_parser parser_for_'c ~parse_from ~namespace:[ "third" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ First; Second; Third ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser_for_tuple3
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "parser_for_tuple4"] _ = struct
  type ('a, 'b, 'c, 'd) tuple4 =
    { first : 'a
    ; second : 'b
    ; third : 'c
    ; fourth : 'd
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b, 'c, 'd) tuple4) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser_for_tuple4
        (type _'a _'b _'c _'d)
        (parser_for_'a : (_'a, _) Derived_parser.t)
        (parser_for_'b : (_'b, _) Derived_parser.t)
        (parser_for_'c : (_'c, _) Derived_parser.t)
        (parser_for_'d : (_'d, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let _ = parser_for_'b in
        let _ = parser_for_'c in
        let _ = parser_for_'d in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field =
            Typed_fields_lib.S_of_S4
              (Typed_field_of_tuple4)
              (struct
                type nonrec t = _'a
              end)
              (struct
                type nonrec t = _'b
              end)
              (struct
                type nonrec t = _'c
              end)
              (struct
                type nonrec t = _'d
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
            | First ->
              Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[ "first" ]
            | Second ->
              Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[ "second" ]
            | Third ->
              Derived_parser.to_parser parser_for_'c ~parse_from ~namespace:[ "third" ]
            | Fourth ->
              Derived_parser.to_parser parser_for_'d ~parse_from ~namespace:[ "fourth" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ First; Second; Third; Fourth ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser_for_tuple4
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module%test [@name "parser_for_tuple5"] _ = struct
  type ('a, 'b, 'c, 'd, 'e) tuple5 =
    { first : 'a
    ; second : 'b
    ; third : 'c
    ; fourth : 'd
    ; fifth : 'e
    }
  [@@deriving typed_fields] [@@deriving_inline uri_parsing]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : ('a, 'b, 'c, 'd, 'e) tuple5) -> ()

    module Ppx_uri_parsing_lib = struct
      include Ppx_uri_parsing_lib

      let parser_for_tuple5
        (type _'a _'b _'c _'d _'e)
        (parser_for_'a : (_'a, _) Derived_parser.t)
        (parser_for_'b : (_'b, _) Derived_parser.t)
        (parser_for_'c : (_'c, _) Derived_parser.t)
        (parser_for_'d : (_'d, _) Derived_parser.t)
        (parser_for_'e : (_'e, _) Derived_parser.t)
        : (_, _) Derived_parser.t
        =
        fun ~parse_from ~namespace ->
        let _ = parser_for_'a in
        let _ = parser_for_'b in
        let _ = parser_for_'c in
        let _ = parser_for_'d in
        let _ = parser_for_'e in
        let module Module_private_for_ppx_uri_parsing = struct
          module Typed_field =
            Typed_fields_lib.S_of_S5
              (Typed_field_of_tuple5)
              (struct
                type nonrec t = _'a
              end)
              (struct
                type nonrec t = _'b
              end)
              (struct
                type nonrec t = _'c
              end)
              (struct
                type nonrec t = _'d
              end)
              (struct
                type nonrec t = _'e
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
            | First ->
              Derived_parser.to_parser parser_for_'a ~parse_from ~namespace:[ "first" ]
            | Second ->
              Derived_parser.to_parser parser_for_'b ~parse_from ~namespace:[ "second" ]
            | Third ->
              Derived_parser.to_parser parser_for_'c ~parse_from ~namespace:[ "third" ]
            | Fourth ->
              Derived_parser.to_parser parser_for_'d ~parse_from ~namespace:[ "fourth" ]
            | Fifth ->
              Derived_parser.to_parser parser_for_'e ~parse_from ~namespace:[ "fifth" ]
          ;;

          let _ = parser_for_field

          module Path_order = Uri_parsing.Path_order (Typed_field)

          let path_order = Path_order.T [ First; Second; Third; Fourth; Fifth ]
          let _ = path_order
        end
        in
        Parser_with_kind.Parser
          (Uri_parsing.Parser.Record.make
             ~namespace
             (module Module_private_for_ppx_uri_parsing))
      ;;

      let _ = parser_for_tuple5
    end
  end [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end
