open! Core
open Uri_parsing
open! Test_util

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = Expect_test_helpers_base.hide_positions_in_string (sanitize s)
end

module%test Testing_a_record = struct
  module The_type = struct
    type t =
      { a : int option
      ; b : string
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.from_fragment Value_parser.int
      | B -> Parser.from_query_required Value_parser.string
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module The_type) |> Versioned_parser.first_parser

  let%expect_test "[check_ok]" =
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────┐
      │ All urls           │
      ├────────────────────┤
      │ /?b=<string>#<int> │
      └────────────────────┘
      |}]
  ;;

  let projection = Versioned_parser.eval parser

  let%expect_test "parsing/unparsing" =
    (* Fragment exists. *)
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [ "b", [ "hi" ] ])
      ~fragment:"2"
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| ((a (2)) (b hi)) |}]);
    (* Fragment does not exist. *)
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [ "b", [ "hi" ] ])
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| ((a ()) (b hi)) |}])
  ;;
end

module%test Testing_a_top_level_type = struct
  let%expect_test "Parsing a top-level from_fragment parser." =
    let parser = Parser.from_fragment Value_parser.int |> Versioned_parser.first_parser in
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────┐
      │ All urls │
      ├──────────┤
      │ /#<int>  │
      └──────────┘
      |}];
    let projection = Versioned_parser.eval parser in
    (* Fragment exists. *)
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:String.Map.empty
      ~fragment:"42"
      ~sexp_of_t:[%sexp_of: int option]
      ~expect:(fun () -> [%expect {| (42) |}]);
    (* Fragment does not exist. *)
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:String.Map.empty
      ?fragment:None
      ~sexp_of_t:[%sexp_of: int option]
      ~expect:(fun () -> [%expect {| () |}])
  ;;
end

module%test Testing_a_variant = struct
  module The_type = struct
    type t =
      | An_int of int option
      | A_string of string option
      | Something_else of string
      | No_payload
    [@@deriving typed_variants, sexp, equal]

    let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
      | An_int -> Parser.from_fragment Value_parser.int
      | A_string -> Parser.from_fragment Value_parser.string
      | Something_else -> Parser.from_path Value_parser.string
      | No_payload -> Parser.unit
    ;;
  end

  let parser = Parser.Variant.make (module The_type) |> Versioned_parser.first_parser
  let projection = Versioned_parser.eval parser

  let%expect_test "[check_ok]" =
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────────┐
      │ All urls                 │
      ├──────────────────────────┤
      │ /a_string#<string>       │
      │ /an_int#<int>            │
      │ /no_payload              │
      │ /something_else/<string> │
      └──────────────────────────┘
      |}];
    (* Failure from fragment needing a path prefix (expected): *)
    Expect_test_helpers_base.require_does_raise (fun () ->
      expect_output_and_identity_roundtrip
        projection
        ~path:[]
        ~query:String.Map.empty
        ~fragment:"42"
        ~sexp_of_t:[%sexp_of: The_type.t]
        ~expect:(fun () -> [%expect.unreachable]));
    [%expect
      {|
      ("Error while parsing! No matching variant contructor found for current path!"
       (components (
         (path  ())
         (query ())
         (fragment 42)))
       (available_patterns (
         (an_int ((pattern ((Match an_int))) (needed_match Prefix)))
         (a_string ((pattern ((Match a_string))) (needed_match Prefix)))
         (something_else ((pattern ((Match something_else))) (needed_match Prefix)))
         (no_payload ((pattern ((Match no_payload))) (needed_match Prefix))))))
      |}];
    expect_output_and_identity_roundtrip
      projection
      ~path:[ "a_string" ]
      ~query:String.Map.empty
      ~fragment:"42"
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| (A_string (42)) |}]);
    expect_output_and_identity_roundtrip
      projection
      ~path:[ "an_int" ]
      ~query:String.Map.empty
      ~fragment:"42"
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| (An_int (42)) |}]);
    expect_output_and_identity_roundtrip
      projection
      ~path:[ "something_else"; "capybara" ]
      ~query:String.Map.empty
      ~fragment:"42"
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| (Something_else capybara) |}]);
    expect_output_and_identity_roundtrip
      projection
      ~path:[ "no_payload" ]
      ~query:String.Map.empty
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| No_payload |}]);
    (* If a fragment exists, parsing will succeed and the fragment will get ignored. This
       is intended. *)
    expect_output_and_identity_roundtrip
      projection
      ~path:[ "no_payload" ]
      ~query:String.Map.empty
      ~fragment:"42"
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| No_payload |}])
  ;;
end

module%test Testing_duplicate_from_path_parsers = struct
  module The_type = struct
    type t =
      { a : int option
      ; b : string option
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.from_fragment Value_parser.int
      | B -> Parser.from_fragment Value_parser.string
    ;;

    module Path_order = Path_order (Typed_field)

    let path_order = Path_order.T []
  end

  let parser = Parser.Record.make (module The_type) |> Versioned_parser.first_parser

  let%expect_test "[check_ok]" =
    (* NOTE: If two parsers are provided, we provide a [check_ok] error. We also provide
       the error location when that happens. *)
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      Error with parser.
      ┌───────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
      │ Check name                │ Error message                                                                            │
      ├───────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
      │ Multiple fragment parsers │ ("Fragment parser ambiguity, there can be at most one fragment parser for a given URL sh │
      │                           │ ape, but found at least 2."                                                              │
      │                           │  (second_parser_is_here lib/uri_parsing/test/test_fragment_parsing.ml:LINE:COL))           │
      └───────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end
