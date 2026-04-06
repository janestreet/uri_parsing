open! Core
open Common

(* $MDX part-begin=attrs *)
module Url = struct
  type t =
    | Homepage of { section : string option [@uri_parsing.fragment] }
    | Discussion of Id.t [@uri_parsing.query]
    | Search of
        { query : string
        ; author_id : Id.t option
        ; categories : Id.t list
        } [@uri_parsing.path]
  [@@deriving typed_variants, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Url.parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────┐
    │ All urls                                                 │
    ├──────────────────────────────────────────────────────────┤
    │ /discussion?value=<id>                                   │
    │ /homepage#<string>                                       │
    │ /search/<string>/<multiple<id>>?author-id=<optional<id>> │
    └──────────────────────────────────────────────────────────┘
    |}]
;;
(* $MDX part-end *)
