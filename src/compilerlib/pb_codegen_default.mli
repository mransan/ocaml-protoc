(** Code generator for the [default] functions (i.e builders) *)

include Pb_codegen_plugin.S

val gen_record_mutable :
  Pb_codegen_ocaml_type.record -> Pb_codegen_formatting.scope -> unit
