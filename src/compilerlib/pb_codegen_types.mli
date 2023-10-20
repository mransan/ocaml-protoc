(** Code generator for the OCaml type *)

include Pb_codegen_plugin.S

val gen_record_mutable :
  Pb_codegen_ocaml_type.record -> Pb_codegen_formatting.scope -> unit
