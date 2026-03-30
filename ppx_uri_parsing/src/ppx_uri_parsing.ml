open Ppxlib
open Ppx_uri_parsing_expander

let signature = Deriving.Generator.make_noarg signature

let structure =
  Deriving.Generator.make
    Deriving.Args.(empty +> arg "capitalize" Ast_pattern.(estring __'))
    structure
;;

let uri_parsing =
  Deriving.add "uri_parsing" ~sig_type_decl:signature ~str_type_decl:structure
;;
