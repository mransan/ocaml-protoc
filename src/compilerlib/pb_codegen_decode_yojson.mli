(** Code generator for the [decode] JSON function *)

include Pb_codegen_plugin.S

val field_pattern_match :
  r_name:string ->
  rf_label:string ->
  Pb_codegen_ocaml_type.field_type ->
  string * string
(** How to decode a field type *)

val plugin : Pb_codegen_plugin.t
