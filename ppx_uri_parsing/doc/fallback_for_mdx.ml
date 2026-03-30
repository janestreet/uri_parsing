open! Core
open Common

(* $MDX part-begin=attrs *)
module Admin_page = struct
  type t =
    | Settings
    | Edit_user of Id.t [@uri_parsing.fallback 0]
  [@@deriving typed_variants, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Admin_page.parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────┐
    │ All urls                  │
    ├───────────────────────────┤
    │ /edit-user/<fallback<id>> │
    │ /settings                 │
    └───────────────────────────┘
    |}]
;;

(* $MDX part-end *)
