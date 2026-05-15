open! Core

module Record = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────┐
      │ All urls             │
      ├──────────────────────┤
      │ /?a=<int>&b=<string> │
      └──────────────────────┘
      |}]
  ;;
end

module Variant = struct
  type t =
    | An_int of int [@uri_parsing.query] [@uri_parsing.index]
    | A_string of string option [@uri_parsing.route [ "different_name" ]]
    | Something_else of string * bool
    | No_payload
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────────────────────────┐
      │ All urls                                 │
      ├──────────────────────────────────────────┤
      │ /?value=<int>                            │
      │ /different_name?value=<optional<string>> │
      │ /no-payload                              │
      │ /something-else/<string>/<bool>          │
      └──────────────────────────────────────────┘
      |}]
  ;;
end

module Record_inside_record = struct
  type t =
    { child : Record.t
    ; second_child : int
    }
  [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────────────────┐
      │ All urls                                            │
      ├─────────────────────────────────────────────────────┤
      │ /?child.a=<int>&child.b=<string>&second-child=<int> │
      └─────────────────────────────────────────────────────┘
      |}]
  ;;
end

module Variant_inside_record = struct
  type t =
    { child : Variant.t
    ; second_child : int
    }
  [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────────────────────────────────────────────────────────────┐
      │ All urls                                                          │
      ├───────────────────────────────────────────────────────────────────┤
      │ /?child.value=<int>&second-child=<int>                            │
      │ /different_name?child.value=<optional<string>>&second-child=<int> │
      │ /no-payload?second-child=<int>                                    │
      │ /something-else/<string>/<bool>?second-child=<int>                │
      └───────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end

module Variant_inside_variant = struct
  type t =
    | An_int of int
    | A_string of string
    | Something_else of Variant.t
    | No_payload
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────────────────────────────────────────────┐
      │ All urls                                                │
      ├─────────────────────────────────────────────────────────┤
      │ /a-string/<string>                                      │
      │ /an-int/<int>                                           │
      │ /no-payload                                             │
      │ /something-else/different_name?value=<optional<string>> │
      │ /something-else/no-payload                              │
      │ /something-else/something-else/<string>/<bool>          │
      │ /something-else?value=<int>                             │
      └─────────────────────────────────────────────────────────┘
      |}]
  ;;
end

module Record_inside_variant = struct
  type t =
    | An_int of int
    | A_string of string
    | Something_else of Record.t
    | No_payload
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────┐
      │ All urls                           │
      ├────────────────────────────────────┤
      │ /a-string/<string>                 │
      │ /an-int/<int>                      │
      │ /no-payload                        │
      │ /something-else?a=<int>&b=<string> │
      └────────────────────────────────────┘
      |}]
  ;;
end

module More_nested = struct
  type t =
    | An_int of int
    | A_string of string
    | Foo of Record_inside_variant.t
    | No_payload
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────────┐
      │ All urls                               │
      ├────────────────────────────────────────┤
      │ /a-string/<string>                     │
      │ /an-int/<int>                          │
      │ /foo/a-string/<string>                 │
      │ /foo/an-int/<int>                      │
      │ /foo/no-payload                        │
      │ /foo/something-else?a=<int>&b=<string> │
      │ /no-payload                            │
      └────────────────────────────────────────┘
      |}]
  ;;
end

module Tuples_options_lists = struct
  type t =
    | A of int * int list * Record.t * string option
    | B of int
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────────────────────────────────────────────────────────────────────────┐
      │ All urls                                                                                 │
      ├──────────────────────────────────────────────────────────────────────────────────────────┤
      │ /a/<int>?fourth=<optional<string>>&second=<multiple<int>>&third.a=<int>&third.b=<string> │
      │ /b/<int>                                                                                 │
      └──────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end

module Inline_records = struct
  type t =
    | A of
        { a : int
        ; b : string
        }
    | T of
        { more_complex : int option
        ; more_complex_2 : bool option * string
        }
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────────────────────────────────────────────────────────────────────────┐
      │ All urls                                                                                 │
      ├──────────────────────────────────────────────────────────────────────────────────────────┤
      │ /a?a=<int>&b=<string>                                                                    │
      │ /t?more-complex-2.first=<optional<bool>>&more-complex-2.second=<string>&more-complex=<op │
      │ tional<int>>                                                                             │
      └──────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end

module Type_parameters = struct
  type ('first, 'second) foo = 'first * 'second [@@deriving uri_parsing]
end

module Use_type_parameters = struct
  type bar = (int, string) Type_parameters.foo [@@deriving uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_bar;
    [%expect
      {|
      URL parser looks good!
      ┌─────────────────┐
      │ All urls        │
      ├─────────────────┤
      │ /<int>/<string> │
      └─────────────────┘
      |}]
  ;;
end

module%test Stringable_sexpable_binable = struct
  (* ppx_string_conv doesn't support type parameters, tuples, etc. *)
  type t =
    | A of (int[@uri_parsing.sexpable]) * (string option[@uri_parsing.sexpable])
    | B of (int[@uri_parsing.stringable]) * (string[@uri_parsing.stringable])
    | C of (int[@uri_parsing.binable]) * (string[@uri_parsing.binable])
    | D of (int * string) [@uri_parsing.sexpable]
    | E of (int * string) [@uri_parsing.binable]
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────────┐
      │ All urls                               │
      ├────────────────────────────────────────┤
      │ /a/<sexpable>/<sexpable>               │
      │ /b/<string>/<string>                   │
      │ /c/<base64<binable>>/<base64<binable>> │
      │ /d/<sexpable>                          │
      │ /e/<base64<binable>>                   │
      └────────────────────────────────────────┘
      |}]
  ;;
end

module%test Bool_attr = struct
  type t =
    { foo : bool [@uri_parsing.bool]
    ; bar : bool
    }
  [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────┐
      │ All urls           │
      ├────────────────────┤
      │ /?[foo]&bar=<bool> │
      └────────────────────┘
      |}]
  ;;
end

module%test Default = struct
  type t = { foo : int [@default 42] } [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────────────────┐
      │ All urls              │
      ├───────────────────────┤
      │ /?foo=<optional<int>> │
      └───────────────────────┘
      |}]
  ;;
end

module%test Key_attr = struct
  type t =
    | Regular of { a : int * int }
    | Keep_namespace of { a : int * int } [@uri_parsing.key "kept"]
    | Skip_namespace of { a : int * int [@uri_parsing.no_key] }
    | Keep_and_skip of { a : int * int [@uri_parsing.no_key] } [@uri_parsing.key "kept"]
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────────────────────────┐
      │ All urls                                               │
      ├────────────────────────────────────────────────────────┤
      │ /keep-and-skip?kept.first=<int>&kept.second=<int>      │
      │ /keep-namespace?kept.a.first=<int>&kept.a.second=<int> │
      │ /regular?a.first=<int>&a.second=<int>                  │
      │ /skip-namespace?first=<int>&second=<int>               │
      └────────────────────────────────────────────────────────┘
      |}]
  ;;

  type t' =
    | Regular of { a : int option }
    | Keep_namespace of { a : int option } [@uri_parsing.key "kept"]
    | Skip_namespace of { a : int option [@uri_parsing.no_key] }
    | Keep_and_skip of { a : int option [@uri_parsing.no_key] } [@uri_parsing.key "kept"]
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_t';
    [%expect
      {|
      URL parser looks good!
      ┌───────────────────────────────────────────┐
      │ All urls                                  │
      ├───────────────────────────────────────────┤
      │ /keep-and-skip?kept.value=<optional<int>> │
      │ /keep-namespace?kept.a=<optional<int>>    │
      │ /regular?a=<optional<int>>                │
      │ /skip-namespace?value=<optional<int>>     │
      └───────────────────────────────────────────┘
      |}]
  ;;

  type t'' =
    { a : int
    ; b : int [@uri_parsing.key "changed-name"]
    }
  [@@deriving typed_fields, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_t'';
    [%expect
      {|
      URL parser looks good!
      ┌──────────────────────────────┐
      │ All urls                     │
      ├──────────────────────────────┤
      │ /?a=<int>&changed-name=<int> │
      └──────────────────────────────┘
      |}]
  ;;
end

module%test Capitalization = struct
  type default = The_brown_fox of { jumps_over : string }
  [@@deriving typed_variants, uri_parsing]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_default;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────┐
      │ All urls                           │
      ├────────────────────────────────────┤
      │ /the-brown-fox?jumps-over=<string> │
      └────────────────────────────────────┘
      |}]
  ;;

  type snake = The_brown_fox of { jumps_over : string }
  [@@deriving typed_variants, uri_parsing ~capitalize:"snake_case"]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_snake;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────┐
      │ All urls                           │
      ├────────────────────────────────────┤
      │ /the_brown_fox?jumps_over=<string> │
      └────────────────────────────────────┘
      |}]
  ;;

  type screaming_snake = The_brown_fox of { jumps_over : string }
  [@@deriving typed_variants, uri_parsing ~capitalize:"SCREAMING_SNAKE_CASE"]

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser_for_screaming_snake;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────────────────────┐
      │ All urls                           │
      ├────────────────────────────────────┤
      │ /THE_BROWN_FOX?JUMPS_OVER=<string> │
      └────────────────────────────────────┘
      |}]
  ;;
end

module%test Sexpable_functor = struct
  module T = struct
    type t =
      | Foo
      | Bar
    [@@deriving sexp]
  end

  include T
  include Ppx_uri_parsing_lib.Make_sexpable (T)

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
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
end

module%test Stringable_functor = struct
  module T = struct
    type t =
      | Foo
      | Bar
    [@@deriving string]
  end

  include T
  include Ppx_uri_parsing_lib.Make_stringable (T)

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌───────────┐
      │ All urls  │
      ├───────────┤
      │ /<string> │
      └───────────┘
      |}]
  ;;
end

module%test Binable_functor = struct
  module T = struct
    type t =
      | Foo
      | Bar
    [@@deriving bin_io]
  end

  include T
  include Ppx_uri_parsing_lib.Make_binable (T)

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌────────────────────┐
      │ All urls           │
      ├────────────────────┤
      │ /<base64<binable>> │
      └────────────────────┘
      |}]
  ;;
end

module%test Value_parser_functor = struct
  module T = struct
    type t = int

    let value_parser = Uri_parsing.Value_parser.name "foo" Uri_parsing.Value_parser.int
  end

  include T
  include Ppx_uri_parsing_lib.Make_from_value_parser (T)

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────┐
      │ All urls │
      ├──────────┤
      │ /<foo>   │
      └──────────┘
      |}]
  ;;
end

module Parser_functor = struct
  module T = struct
    type t = int

    let parser =
      Uri_parsing.Parser.from_path
        (Uri_parsing.Value_parser.name "foo" Uri_parsing.Value_parser.int)
    ;;
  end

  include T
  include Ppx_uri_parsing_lib.Make_from_parser (T)

  let%expect_test _ =
    Uri_parsing.Parser.check_ok_and_print_urls_or_errors parser;
    [%expect
      {|
      URL parser looks good!
      ┌──────────┐
      │ All urls │
      ├──────────┤
      │ /<foo>   │
      └──────────┘
      |}]
  ;;
end
