open! Core
open Uri_parsing
open! Test_util

module%test Testing_a_record = struct
  module The_type = struct
    type t =
      { a : bool
      ; b : bool
      ; c : bool
      }
    [@@deriving typed_fields, sexp, equal]

    let parser_for_field : type a. a Typed_field.t -> a Parser.t = function
      | A -> Parser.from_query_flag ()
      | B -> Parser.from_query_flag ~key:"overriden" ()
      | C ->
        Parser.from_query_optional_with_default
          ~equal:[%equal: bool]
          Value_parser.bool
          ~default:false
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
      ┌──────────────────────────────────────┐
      │ All urls                             │
      ├──────────────────────────────────────┤
      │ /?[a]&[overriden]&c=<optional<bool>> │
      └──────────────────────────────────────┘
      |}]
  ;;

  let projection = Versioned_parser.eval parser

  let%expect_test "parsing/unparsing" =
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [ "a", []; "overriden", []; "c", [ "true" ] ])
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| ((a true) (b true) (c true)) |}]);
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [])
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| ((a false) (b false) (c false)) |}])
  ;;

  let%expect_test "We are backwards compatible with other boolean parsers with a payload" =
    expect_output_and_identity_roundtrip
      projection
      ~path:[]
      ~query:(String.Map.of_alist_exn [ "a", [ "true" ]; "overriden", [ "false" ] ])
      ~sexp_of_t:[%sexp_of: The_type.t]
      ~expect:(fun () -> [%expect {| ((a true) (b false) (c false)) |}]);
    (* The previous URL format is auto-converted to the "payload-less" format. *)
    [%expect
      {|
      -1,2 +1,1
      -|((a         (true))
      -| (overriden (false)))
      +|((a ()))
      |}]
  ;;

  let%expect_test "non-empty failure" =
    (* NOTE: If there are values in a record field, then we raise. *)
    Expect_test_helpers_core.require_does_raise (fun () ->
      expect_output_and_identity_roundtrip
        projection
        ~path:[]
        ~query:(String.Map.of_alist_exn [ "a", [ "something-else" ] ])
        ~sexp_of_t:[%sexp_of: The_type.t]
        ~expect:(fun () -> [%expect.unreachable]));
    [%expect
      {|
      ("Error while parsing record field:"
        (error_message (
          "Expected no values in query boolean flag, but found values"
          (values (something-else))))
        (field_name a)
        (unparseable_components ((path ()) (query ((a (something-else)))))))
      |}]
  ;;
end

module%test Testing_a_top_level_type = struct
  let%expect_test "parser without an explicit key results in an error." =
    let parser = Parser.from_query_flag () |> Versioned_parser.first_parser in
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      Error with parser.
      ┌──────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
      │ Check name                               │ Error message                                                                            │
      ├──────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
      │ Enough information needed to parse check │ "Could not infer query key! In most cases, the keys of the key of a query field can be i │
      │                                          │ nferred from the labels of the record it's in or the constructor of the variant it's in. │
      │                                          │  However, the [from_query_*] parser's key name could not be inferred. You can fix this i │
      │                                          │ n two ways. 1. Wrap your type around a record/variant OR 2. explicitly assign a key thro │
      │                                          │ ugh the optional [?key] parameter."                                                      │
      └──────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "parser with an explicit key is fine." =
    let parser =
      Parser.from_query_flag ~key:"some-flag" () |> Versioned_parser.first_parser
    in
    Versioned_parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────────┐
      │ All urls      │
      ├───────────────┤
      │ /?[some-flag] │
      └───────────────┘
      |}]
  ;;
end
