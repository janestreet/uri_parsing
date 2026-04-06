open! Core
open Ppxlib

val signature
  :  loc:location
  -> path:label
  -> rec_flag * type_declaration list
  -> signature_item list

val structure
  :  loc:location
  -> path:label
  -> rec_flag * type_declaration list
  -> string loc option
  -> structure_item list
