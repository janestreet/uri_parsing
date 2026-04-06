open! Core

module Id = struct
  type t = int [@@deriving sexp]

  module For_ppx_uri_parsing = struct
    open For_ppx_uri_parsing

    let parser ~parse_from ~namespace:_ =
      Parser_with_kind.Value_parser
        (Uri_parsing.Value_parser.name "id" Uri_parsing.Value_parser.int, parse_from)
    ;;
  end
end
