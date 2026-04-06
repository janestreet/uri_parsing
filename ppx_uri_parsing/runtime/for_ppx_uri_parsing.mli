open! Core

module Tiebreaker : sig
  type t =
    | Path
    | Query
end

module Parse_from : sig
  (** An indicator of where the parser should try to parse from. [Path] and [Query] come
      from user attributes and should not be overridden. [Default _] is not that
      opinionated and only provides a tiebreaker for situations where there's no obvious
      choice (e.g. value parsers).

      For example, [~parse_from:Path] on a record [{ a : int }] would read [a] from the
      path, while [~parse_from:(Default Path)] would still read [a] from the query. *)
  type t =
    | Path
    | Default of Tiebreaker.t
    | Query
end

module Parser_with_kind : sig
  (** A wrapped parser or value parser. *)
  type ('a, 'kind) t =
    | Parser : 'a Uri_parsing.Parser.t -> ('a, [ `Parser ]) t
    | Value_parser :
        'a Uri_parsing.Value_parser.t * Parse_from.t
        -> ('a, [ `Value_parser ]) t
end

module Derived_parser : sig
  (** A parser with access to a [Parse_from.t] and namespace. *)
  type ('a, 'kind) t =
    parse_from:Parse_from.t
    -> namespace:string list
    -> ('a, ([< `Parser | `Value_parser ] as 'kind)) Parser_with_kind.t

  val to_parser
    :  ('a, _) t
    -> parse_from:Parse_from.t
    -> namespace:string list
    -> 'a Uri_parsing.Parser.t

  (** Gets a key that respects the ppx [namespace] and [key] attributes. *)
  val key_of_namespace : string list -> string
end

val parser_for_int : (int, [ `Value_parser ]) Derived_parser.t
val parser_for_string : (string, [ `Value_parser ]) Derived_parser.t
val parser_for_float : (float, [ `Value_parser ]) Derived_parser.t
val parser_for_bool : (bool, [ `Value_parser ]) Derived_parser.t

val parser_for_option
  :  ('a, [ `Value_parser ]) Derived_parser.t
  -> ('a option, [ `Parser ]) Derived_parser.t

val parser_for_list
  :  ('a, [ `Value_parser ]) Derived_parser.t
  -> ('a list, [ `Parser ]) Derived_parser.t

val parser_for_tuple2
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('a * 'b, [ `Parser ]) Derived_parser.t

val parser_for_tuple3
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('c, _) Derived_parser.t
  -> ('a * 'b * 'c, [ `Parser ]) Derived_parser.t

val parser_for_tuple4
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('c, _) Derived_parser.t
  -> ('d, _) Derived_parser.t
  -> ('a * 'b * 'c * 'd, [ `Parser ]) Derived_parser.t

val parser_for_tuple5
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('c, _) Derived_parser.t
  -> ('d, _) Derived_parser.t
  -> ('e, _) Derived_parser.t
  -> ('a * 'b * 'c * 'd * 'e, [ `Parser ]) Derived_parser.t

val parser_for_tuple6
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('c, _) Derived_parser.t
  -> ('d, _) Derived_parser.t
  -> ('e, _) Derived_parser.t
  -> ('f, _) Derived_parser.t
  -> ('a * 'b * 'c * 'd * 'e * 'f, [ `Parser ]) Derived_parser.t

val parser_for_tuple7
  :  ('a, _) Derived_parser.t
  -> ('b, _) Derived_parser.t
  -> ('c, _) Derived_parser.t
  -> ('d, _) Derived_parser.t
  -> ('e, _) Derived_parser.t
  -> ('f, _) Derived_parser.t
  -> ('g, _) Derived_parser.t
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g, [ `Parser ]) Derived_parser.t
