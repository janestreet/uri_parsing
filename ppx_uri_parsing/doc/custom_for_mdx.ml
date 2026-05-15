open! Core
open Common
open Ppx_uri_parsing_lib

(* $MDX part-begin=custom *)
let parser_for_id ~parse_from ~namespace:_ =
  Parser_with_kind.Value_parser
    (Uri_parsing.Value_parser.name "id" Uri_parsing.Value_parser.int, parse_from)
;;

module Search_params = struct
  type t =
    { query : string
    ; author : Id.t option [@uri_parsing.custom_parser parser_for_option parser_for_id]
    ; categories : (Id.t[@uri_parsing.custom_parser parser_for_id]) list
    }
  [@@deriving typed_fields, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Search_params.parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────┐
    │ All urls                                                         │
    ├──────────────────────────────────────────────────────────────────┤
    │ /?author=<optional<id>>&categories=<multiple<id>>&query=<string> │
    └──────────────────────────────────────────────────────────────────┘
    |}]
;;
(* $MDX part-end *)
