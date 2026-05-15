open! Core

module Id : sig
  type t = int [@@deriving sexp]

  module Ppx_uri_parsing_lib : sig
    val parser : (t, [ `Value_parser ]) Ppx_uri_parsing_lib.Derived_parser.t
  end
end
