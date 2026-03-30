open! Core
open Common

(* $MDX part-begin=attrs *)
module Url = struct
  type t =
    | Homepage [@uri_parsing.index]
    | Discussion of Id.t [@uri_parsing.route [ "forum" ]]
    | Search of { query : string } [@uri_parsing.route []]
  [@@deriving typed_variants, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Url.parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────┐
    │ All urls         │
    ├──────────────────┤
    │ /                │
    │ /?query=<string> │
    │ /forum/<id>      │
    └──────────────────┘
    |}]
;;
(* $MDX part-end *)
