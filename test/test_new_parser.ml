open! Core
open Uri_parsing
open Test_util

let%expect_test "Basic use of [Parser.new_parser]" =
  let int = Parser.from_query_required ~key:"a" Value_parser.int in
  let string = Parser.from_query_required ~key:"b" Value_parser.string in
  let parser =
    Versioned_parser.first_parser
      (Parser.new_parser int ~previous:string ~f:(fun x -> String.length x))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────┐
    │ All urls     │
    ├──────────────┤
    │ /?a=<int>    │
    │ /?b=<string> │
    └──────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "1" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 1 |}]);
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "b", [ "hi!!!" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 5 |}]);
  [%expect
    {|
    -1,1 +1,1
    -|((b (hi!!!)))
    +|((a (5)))
    |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "I am not an int!" ]; "b", [ "hi!!!" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 5 |}]);
  [%expect
    {|
    -1,2 +1,1
    -|((a ("I am not an int!"))
    -| (b (hi!!!)))
    +|((a (5)))
    |}]
;;

let%expect_test "[Parser.new_parser] f fails" =
  let int = Parser.from_query_required ~key:"a" Value_parser.int in
  let string = Parser.from_query_required ~key:"b" Value_parser.string in
  let parser =
    Versioned_parser.first_parser
      (Parser.new_parser int ~previous:string ~f:(fun x -> Int.of_string x))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────┐
    │ All urls     │
    ├──────────────┤
    │ /?a=<int>    │
    │ /?b=<string> │
    └──────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  Expect_test_helpers_core.require_does_raise (fun () ->
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [ "a", [ "not an int" ]; "b", [ "not an int" ] ])
      ~sexp_of_t:[%sexp_of: int]
      ~expect:(fun () -> [%expect.unreachable]));
  [%expect {| (Failure "Int.of_string: \"not an int\"") |}]
;;

let%expect_test "[Parser.new_parser] duplicate keys - OK" =
  let int = Parser.from_query_required ~key:"a" Value_parser.int in
  let string = Parser.from_query_required ~key:"a" Value_parser.string in
  let parser =
    Versioned_parser.first_parser
      (Parser.new_parser int ~previous:string ~f:(fun x -> String.length x))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  (* NOTE: This is OK as only one of them will ever parse. *)
  [%expect
    {|
    URL parser looks good!
    ┌──────────────┐
    │ All urls     │
    ├──────────────┤
    │ /?a=<int>    │
    │ /?a=<string> │
    └──────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "not an int" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 10 |}]);
  [%expect
    {|
    -1,1 +1,1
    -|((a ("not an int")))
    +|((a (10)))
    |}]
;;

let%expect_test "[Parser.new_parser] duplicate keys - ambiguity" =
  let int = Parser.from_query_required ~key:"a" Value_parser.int in
  let string = Parser.from_query_required ~key:"a" Value_parser.string in
  let always_there = Parser.from_query_required ~key:"a" Value_parser.bool in
  let parser =
    Versioned_parser.first_parser
      (Parser.both
         always_there
         (Parser.new_parser int ~previous:string ~f:(fun x -> String.length x)))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  (* NOTE: This is expected, as here there is an actual collision. *)
  [%expect
    {|
    Error with parser.
    ┌──────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name           │ Error message                                                                            │
    ├──────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Duplicate keys check │ ("Duplicate query keys found! This probably occurred due at least one [~key] being renam │
    │                      │ ed to the same string in a [from_query_*] parser. Another possibility is that a renamed  │
    │                      │ key conflicted with an inferred parser."                                                 │
    │                      │  (duplicate_query_keys_by_url                                                            │
    │                      │   ((/?a=<bool>&a=<int> (a)) (/?a=<bool>&a=<string> (a)))))                               │
    └──────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "[Parser.new_parser] different parsers have different expected prefixes" =
  let int =
    Parser.with_prefix [ "int" ] (Parser.from_query_required ~key:"a" Value_parser.int)
  in
  let string =
    Parser.with_prefix
      [ "string" ]
      (Parser.from_query_required ~key:"a" Value_parser.string)
  in
  let parser = Parser.new_parser int ~previous:string ~f:(fun x -> String.length x) in
  let module Url = struct
    type t =
      | Parser of int
      | Other
    [@@deriving typed_variants]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Parser -> parser
      | Other -> Parser.unit
    ;;
  end
  in
  let parser = Versioned_parser.first_parser (Parser.Variant.make (module Url)) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  (* NOTE: This is expected, the same branch will only listen for the new_ parser and the
     second parser will be unreachable. *)
  [%expect
    {|
    ("Ambiguity, a Parser.new_parser's new parser and previous parser have different expected paths. The previous parser won't ever parse."
     (new_next_path ((((Match int)) Stop_prefix)))
     (prev_next_path ((((Match string)) Stop_prefix))))
    Error with parser.
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name                                                                               │ Error message                                                                            │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ Unreachable previous parser. The previous parser and the new parser have different paths │ ("Ambiguity, a Parser.new_parser's new parser and previous parser have different expecte │
    │ .                                                                                        │ d paths. The previous parser won't ever parse."                                          │
    │                                                                                          │  (new_ ((Prefix ((Match int))))) (prev ((Prefix ((Match string))))))                     │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "[Parser.new_parser] different parsers have the same prefix" =
  let int =
    Parser.with_prefix [ "int" ] (Parser.from_query_required ~key:"a" Value_parser.int)
  in
  let string =
    Parser.with_prefix [ "int" ] (Parser.from_query_required ~key:"a" Value_parser.string)
  in
  let parser = Parser.new_parser int ~previous:string ~f:(fun x -> String.length x) in
  let module Url = struct
    type t =
      | Parser of int
      | Other
    [@@deriving typed_variants]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | Parser -> parser
      | Other -> Parser.unit
    ;;
  end
  in
  let parser = Versioned_parser.first_parser (Parser.Variant.make (module Url)) in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  (* NOTE: This is expected, as here there is an actual collision. *)
  [%expect
    {|
    URL parser looks good!
    ┌─────────────────┐
    │ All urls        │
    ├─────────────────┤
    │ /int?a=<int>    │
    │ /int?a=<string> │
    │ /other          │
    └─────────────────┘
    |}]
;;

let%expect_test "[Parser.new_parser] four-nested-in-a-row" =
  let int = Parser.from_query_required ~key:"a" Value_parser.int in
  let float = Parser.from_query_required ~key:"a" Value_parser.float in
  let bool = Parser.from_query_required ~key:"a" Value_parser.bool in
  let string = Parser.from_query_required ~key:"a" Value_parser.string in
  let parser =
    Versioned_parser.first_parser
      (Parser.new_parser
         int
         ~previous:
           (Parser.new_parser
              float
              ~previous:(Parser.new_parser bool ~previous:string ~f:Bool.of_string)
              ~f:(function
             | true -> 1.0
             | false -> 0.0))
         ~f:(fun x -> Float.to_int x))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────┐
    │ All urls     │
    ├──────────────┤
    │ /?a=<bool>   │
    │ /?a=<float>  │
    │ /?a=<int>    │
    │ /?a=<string> │
    └──────────────┘
    |}];
  let projection = Versioned_parser.eval parser in
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "1" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 1 |}]);
  [%expect {| |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "true" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 1 |}]);
  [%expect
    {|
    -1,1 +1,1
    -|((a (true)))
    +|((a (1)))
    |}];
  expect_output_and_identity_roundtrip
    projection
    ~path:[]
    ~query:(String.Map.of_alist_exn [ "a", [ "false" ] ])
    ~sexp_of_t:[%sexp_of: int]
    ~expect:(fun () -> [%expect {| 0 |}]);
  [%expect
    {|
    -1,1 +1,1
    -|((a (false)))
    +|((a (0)))
    |}]
;;

let%expect_test "[Parser.new_parser] path order check" =
  let int = Parser.from_query_required Value_parser.int in
  let string = Parser.from_query_required Value_parser.string in
  let module Url = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.new_parser int ~previous:string ~f:(fun x -> String.length x)
      | B ->
        Parser.with_prefix
          [ "b" ]
          (Parser.new_parser int ~previous:string ~f:(fun x -> String.length x))
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)
  end
  in
  let parser =
    Versioned_parser.first_parser
      (Parser.Record.make
         (module struct
           include Url

           let path_order = Path_order.T []
         end))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    Error with parser.
    ┌────────────────────────┬───────────────────────────────────────────────────────────────────────────────────────┐
    │ Check name             │ Error message                                                                         │
    ├────────────────────────┼───────────────────────────────────────────────────────────────────────────────────────┤
    │ Sane path orders check │ ("Each path parser must be present in path order. The following fields were missing:" │
    │                        │  (missing_fields (B)))                                                                │
    └────────────────────────┴───────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  let parser =
    Versioned_parser.first_parser
      (Parser.Record.make
         (module struct
           include Url

           let path_order = Path_order.T [ B ]
         end))
  in
  Versioned_parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────┐
    │ All urls                 │
    ├──────────────────────────┤
    │ /b?a=<int>&b=<int>       │
    │ /b?a=<int>&b=<string>    │
    │ /b?a=<string>&b=<int>    │
    │ /b?a=<string>&b=<string> │
    └──────────────────────────┘
    |}]
;;

let%expect_test "[Parser.new_parser] remaining_path check" =
  let int = Parser.from_path Value_parser.int in
  let string = Parser.from_remaining_path Value_parser.string in
  let module Url = struct
    type t =
      { a : string list
      ; b : int
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> string
      | B -> Parser.new_parser int ~previous:string ~f:(fun x -> List.length x)
    ;;

    module Path_order = Uri_parsing.Path_order (Typed_field)
  end
  in
  let parser =
    Versioned_parser.first_parser
      (Parser.Record.make
         (module struct
           include Url

           let path_order = Path_order.T [ A; B ]
         end))
  in
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
  let parser =
    Versioned_parser.first_parser
      (Parser.Record.make
         (module struct
           include Url

           let path_order = Path_order.T [ B; A ]
         end))
  in
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
    |}]
;;
