open! Core
open Ppxlib

let flag name context = Attribute.declare_flag [%string "uri_parsing.%{name}"] context

let attribute name context =
  Attribute.declare
    [%string "uri_parsing.%{name}"]
    context
    Ast_pattern.(single_expr_payload __)
    Fn.id
;;

let query_variant = flag "query" Attribute.Context.constructor_declaration
let query_record = flag "query" Attribute.Context.label_declaration
let path_variant = flag "path" Attribute.Context.constructor_declaration
let path_record = flag "path" Attribute.Context.label_declaration
let key_variant = attribute "key" Attribute.Context.constructor_declaration
let key_record = attribute "key" Attribute.Context.label_declaration
let route_variant = attribute "route" Attribute.Context.constructor_declaration
let route_record = attribute "route" Attribute.Context.label_declaration
let index = flag "index" Attribute.Context.constructor_declaration
let no_key = flag "no_key" Attribute.Context.label_declaration
let sexpable = flag "sexpable" Attribute.Context.core_type
let stringable = flag "stringable" Attribute.Context.core_type
let binable = flag "binable" Attribute.Context.core_type
let bool = flag "bool" Attribute.Context.core_type
let custom_parser = attribute "custom_parser" Attribute.Context.core_type
let default = attribute "default" Attribute.Context.core_type
let fallback = attribute "fallback" Attribute.Context.core_type
let fragment = flag "fragment" Attribute.Context.core_type
