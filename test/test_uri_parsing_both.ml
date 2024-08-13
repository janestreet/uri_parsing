open! Core
open Uri_parsing
open Test_util

let%expect_test "both_parser (from query)" =
  let a = Parser.from_query_required ~key:"a" Value_parser.int in
  let b = Parser.from_query_required ~key:"b" Value_parser.bool in
  let parser = Versioned_parser.first_parser (Parser.both a b) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────┐
    │ All urls           │
    ├────────────────────┤
    │ /?a=<int>&b=<bool> │
    └────────────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "1" ]; "b", [ "false" ] ])
    ~sexp_of_t:[%sexp_of: int * bool]
    ~expect:(fun () -> [%expect {| (1 false) |}])
;;

let%expect_test "both_parser (with namespace)" =
  let a = Parser.from_query_required ~key:"a" Value_parser.int in
  let b = Parser.from_query_required ~key:"b" Value_parser.bool in
  let parser =
    Versioned_parser.first_parser (Parser.both ~namespace:[ "some"; "namespace" ] a b)
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────┐
    │ All urls                                         │
    ├──────────────────────────────────────────────────┤
    │ /?some.namespace.a=<int>&some.namespace.b=<bool> │
    └──────────────────────────────────────────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:
      (String.Map.of_alist_exn
         [ "some.namespace.a", [ "1" ]; "some.namespace.b", [ "false" ] ])
    ~sexp_of_t:[%sexp_of: int * bool]
    ~expect:(fun () -> [%expect {| (1 false) |}])
;;

let%expect_test "both_parser (combination)" =
  let a = Parser.from_path Value_parser.int in
  let b = Parser.from_query_optional ~key:"b" Value_parser.bool in
  let c = Parser.from_query_required ~key:"c" Value_parser.float in
  let d = Parser.from_remaining_path Value_parser.string in
  let parser =
    Versioned_parser.first_parser (Parser.both c d |> Parser.both b |> Parser.both a)
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  (* NOTE: This test also shows that nested [both]'s like this will add a namespace. This
     is normal and expected, but still worth noting/captuing this behavior in tests. *)
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                    │
    ├─────────────────────────────────────────────────────────────────────────────┤
    │ /<int>/<multiple<string>>?second.b=<optional<bool>>&second.second.c=<float> │
    └─────────────────────────────────────────────────────────────────────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[ "1"; "some"; "remaining"; "path" ]
    ~query:(String.Map.of_alist_exn [ "second.second.c", [ "1.4" ] ])
    ~sexp_of_t:[%sexp_of: int * (bool option * (float * string list))]
    ~expect:(fun () -> [%expect {| (1 (() (1.4 (some remaining path)))) |}])
;;

let%expect_test "both_parser - duplicate keys" =
  let a = Parser.from_query_required ~key:"a" Value_parser.int in
  let b = Parser.from_query_required ~key:"a" Value_parser.int in
  let parser = Versioned_parser.first_parser (Parser.both a b) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌──────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name           │ Error message                                                                            │
    ├──────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Duplicate keys check │ ("Duplicate query keys found! This probably occurred due at least one [~key] being renam │
    │                      │ ed to the same string in a [from_query_*] parser. Another possibility is that a renamed  │
    │                      │ key conflicted with an inferred parser."                                                 │
    │                      │  (duplicate_query_keys_by_url ((/?a=<int>&a=<int> (a)))))                                │
    └──────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "both_parser - not sane path order" =
  (* The thing that is not sane about this parser is that a remaining path happens
     _before_ a normal parser. *)
  let a = Parser.from_remaining_path Value_parser.int in
  let b = Parser.from_path Value_parser.int in
  let parser = Versioned_parser.first_parser (Parser.both a b) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌───────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                        │ Error message                                                                            │
    ├───────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Remaining path does not block other parsers check │ "Error! There cannot be path parsers inside of a [with_remaining_path] combinator! The r │
    │                                                   │ eason for this is that there won't be any values after that parser!"                     │
    └───────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* The with_remaining_path parser happening at the end is fine. *)
  let parser = Versioned_parser.first_parser (Parser.both b a) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────────────┐
    │ All urls               │
    ├────────────────────────┤
    │ /<int>/<multiple<int>> │
    └────────────────────────┘
    |}]
;;
