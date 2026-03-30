open! Core
open Ppxlib

(* On variant constructors & record labels *)
val query_variant : (constructor_declaration, unit) Attribute.t
val query_record : (label_declaration, unit) Attribute.t
val path_variant : (constructor_declaration, unit) Attribute.t
val path_record : (label_declaration, unit) Attribute.t
val key_variant : (constructor_declaration, expression) Attribute.t
val key_record : (label_declaration, expression) Attribute.t
val route_variant : (constructor_declaration, expression) Attribute.t
val route_record : (label_declaration, expression) Attribute.t

(* On variants only *)
val index : (constructor_declaration, unit) Attribute.t

(* On records only *)
val no_key : (label_declaration, unit) Attribute.t

(* On individual types *)
val sexpable : (core_type, unit) Attribute.t
val stringable : (core_type, unit) Attribute.t
val binable : (core_type, unit) Attribute.t
val bool : (core_type, unit) Attribute.t
val custom_parser : (core_type, expression) Attribute.t
val default : (core_type, expression) Attribute.t
val fallback : (core_type, expression) Attribute.t
val fragment : (core_type, unit) Attribute.t
