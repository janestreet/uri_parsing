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

    module For_ppx_uri_parsing : sig
      val parser : (t, [ `Parser ]) For_ppx_uri_parsing.Derived_parser.t
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

    module For_ppx_uri_parsing : sig
      val parser
        :  ('a, _) For_ppx_uri_parsing.Derived_parser.t
        -> ('b, _) For_ppx_uri_parsing.Derived_parser.t
        -> (('a, 'b) t, [ `Parser ]) For_ppx_uri_parsing.Derived_parser.t
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@ppxlib.inline.end]
end
