open! Core
open Common

(* $MDX part-begin=simple-variant *)
module Admin_page = struct
  type t =
    | Settings
    | Edit_user of Id.t
  [@@deriving typed_variants, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Admin_page.parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────┐
    │ All urls        │
    ├─────────────────┤
    │ /edit-user/<id> │
    │ /settings       │
    └─────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=simple-record *)
module Search_params = struct
  type t =
    { query : string
    ; author_id : Id.t option
    ; categories : Id.t list
    }
  [@@deriving typed_fields, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Search_params.parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────────────────────────┐
    │ All urls                                                            │
    ├─────────────────────────────────────────────────────────────────────┤
    │ /?author-id=<optional<id>>&categories=<multiple<id>>&query=<string> │
    └─────────────────────────────────────────────────────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=simple-manifest *)
module User_data = struct
  type t = string * int [@@deriving uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors User_data.parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────┐
    │ All urls        │
    ├─────────────────┤
    │ /<string>/<int> │
    └─────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=capitalize *)
module T = struct
  type t = User_data of { user_id : Id.t }
  [@@deriving typed_variants, uri_parsing ~capitalize:"snake_case"]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors T.parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────┐
    │ All urls                │
    ├─────────────────────────┤
    │ /user_data?user_id=<id> │
    └─────────────────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=sexpable *)
module My_id = struct
  type t = (Id.t[@uri_parsing.sexpable]) [@@deriving uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors My_id.parser;
  [%expect
    {|
    URL parser looks good!
    ┌─────────────┐
    │ All urls    │
    ├─────────────┤
    │ /<sexpable> │
    └─────────────┘
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=key *)
module Contrived_key_example = struct
  type t =
    | Default of { a : int * int }
    | Keep_both_keys of { a : int * int } [@uri_parsing.key "kept"]
    | Skip_both_keys of { a : int * int [@uri_parsing.no_key] }
    | Keep_outer_skip_inner of { a : int * int [@uri_parsing.no_key] }
    [@uri_parsing.key "kept"]
  [@@deriving typed_variants, uri_parsing]
end

let%expect_test _ =
  Uri_parsing.Parser.check_ok_and_print_urls_or_errors Contrived_key_example.parser;
  [%expect
    {|
    URL parser looks good!
    ┌───────────────────────────────────────────────────────────┐
    │ All urls                                                  │
    ├───────────────────────────────────────────────────────────┤
    │ /default?a.first=<int>&a.second=<int>                     │
    │ /keep-both-keys?kept.a.first=<int>&kept.a.second=<int>    │
    │ /keep-outer-skip-inner?kept.first=<int>&kept.second=<int> │
    │ /skip-both-keys?first=<int>&second=<int>                  │
    └───────────────────────────────────────────────────────────┘
    |}]
;;

(* $MDX part-end *)
