open! Core

(* Tests that deriving uri_parsing in the signature is compatible with deriving
   uri_parsing in the structure. *)

module Record : sig
  type t =
    { a : int
    ; b : string
    }
  [@@deriving uri_parsing]
end

module Variant : sig
  type t =
    | An_int of int
    | A_string of string option
    | Something_else of string * bool
    | No_payload
  [@@deriving uri_parsing]
end

module Record_inside_record : sig
  type t =
    { child : Record.t
    ; second_child : int
    }
  [@@deriving uri_parsing]
end

module Variant_inside_record : sig
  type t =
    { child : Variant.t
    ; second_child : int
    }
  [@@deriving uri_parsing]
end

module Variant_inside_variant : sig
  type t =
    | An_int of int
    | A_string of string
    | Something_else of Variant.t
    | No_payload
  [@@deriving uri_parsing]
end

module Record_inside_variant : sig
  type t =
    | An_int of int
    | A_string of string
    | Something_else of Record.t
    | No_payload
  [@@deriving uri_parsing]
end

module More_nested : sig
  type t =
    | An_int of int
    | A_string of string
    | Foo of Record_inside_variant.t
    | No_payload
  [@@deriving uri_parsing]
end

module Tuples_options_lists : sig
  type t =
    | A of int * int list * Record.t * string option
    | B of int
  [@@deriving uri_parsing]
end

module Inline_records : sig
  type t =
    | A of
        { a : int
        ; b : string
        }
    | T of
        { more_complex : int option
        ; more_complex_2 : bool option * string
        }
  [@@deriving uri_parsing]
end

module Type_parameters : sig
  type ('first, 'second) foo [@@deriving uri_parsing]
end

module Use_type_parameters : sig
  type bar = (int, string) Type_parameters.foo [@@deriving uri_parsing]
end

module Parser_functor : sig
  type t [@@deriving uri_parsing]
end
