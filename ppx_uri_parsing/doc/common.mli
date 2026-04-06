open! Core

module Id : sig
  type t = int [@@deriving sexp]

  module For_ppx_uri_parsing : sig
    val parser : (t, [ `Value_parser ]) For_ppx_uri_parsing.Derived_parser.t
  end
end
