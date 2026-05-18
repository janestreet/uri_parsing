open! Core

module Id = struct
  module T = struct
    type t = int [@@deriving sexp]

    let value_parser = Uri_parsing.Value_parser.name "id" Uri_parsing.Value_parser.int
  end

  include T
  include Ppx_uri_parsing_lib.Make_from_value_parser (T)
end
