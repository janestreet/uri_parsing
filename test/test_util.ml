open! Core
open Uri_parsing

let diff_queries a b =
  let sexp_of_query query =
    String.Map.sexp_of_t (List.sexp_of_t String.sexp_of_t) query
  in
  Expect_test_patdiff.print_patdiff_s (sexp_of_query a) (sexp_of_query b)
;;

let diff_paths a b =
  let sexp_of_query query = List.sexp_of_t String.sexp_of_t query in
  Expect_test_patdiff.print_patdiff_s (sexp_of_query a) (sexp_of_query b)
;;

let expect_output_and_identity_roundtrip
  ?(expect_diff = fun () -> ())
  ~path
  ~query
  ~sexp_of_t
  ~expect
  (projection : (Components.t, 'a Parse_result.t) Projection.t)
  =
  let result = projection.parse_exn { query; path } in
  print_s (sexp_of_t result.result);
  expect ();
  let { Components.query = unparsed_query; path = unparsed_path } =
    projection.unparse result
  in
  diff_paths path unparsed_path;
  diff_queries query unparsed_query;
  expect_diff ()
;;
