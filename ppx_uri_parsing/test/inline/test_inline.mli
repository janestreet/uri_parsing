open! Core

module Simple_record : sig
  type t =
    { a : int
    ; b : string
    }
  [@@deriving_inline uri_parsing]

  include sig
    [@@@ocaml.warning "-32-60"]

    val parser : t Uri_parsing.Parser.t

    module Ppx_uri_parsing_lib : sig
      val parser : (t, [ `Parser ]) Ppx_uri_parsing_lib.Derived_parser.t
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end

module Record_with_type_parameters : sig
  type ('a, 'b) t =
    { a : 'a
    ; b : 'b
    }
  [@@deriving_inline uri_parsing]

  include sig
    [@@@ocaml.warning "-32-60"]

    module Ppx_uri_parsing_lib : sig
      val parser
        :  ('a, _) Ppx_uri_parsing_lib.Derived_parser.t
        -> ('b, _) Ppx_uri_parsing_lib.Derived_parser.t
        -> (('a, 'b) t, [ `Parser ]) Ppx_uri_parsing_lib.Derived_parser.t
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end
