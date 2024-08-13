open! Core
open Uri_parsing

val diff_queries : string list Core.String.Map.t -> string list Core.String.Map.t -> unit
val diff_paths : string list -> string list -> unit

val expect_output_and_identity_roundtrip
  :  ?expect_diff:(unit -> unit)
  -> path:string list
  -> query:string list Core.String.Map.t
  -> sexp_of_t:('a -> Sexp.t)
  -> expect:(unit -> unit)
  -> (Components.t, 'a Parse_result.t) Projection.t
  -> unit
