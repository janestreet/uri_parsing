open! Core

(* $MDX part-begin=attrs *)
module Search_params = struct
  type t =
    { query : string [@uri_parsing.default ""]
    ; only_search_favorites : bool [@uri_parsing.bool]
    }
  [@@deriving typed_fields, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Search_params.parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────────────────────────────────────┐
    │ All urls                                           │
    ├────────────────────────────────────────────────────┤
    │ /?[only-search-favorites]&query=<optional<string>> │
    └────────────────────────────────────────────────────┘
    |}]
;;
(* $MDX part-end *)
