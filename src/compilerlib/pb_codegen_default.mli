(** Code generator for the [default] functions (i.e builders) *)

include Pb_codegen_plugin.S

val record_field_default_info :
  Pb_codegen_ocaml_type.record_field -> string * string * string
(** This function returns [(field_name, field_default_value, field_type)] for
   a record field. *)

val gen_record_mutable :
  Pb_codegen_ocaml_type.record -> Pb_codegen_formatting.scope -> unit
