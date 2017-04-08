(** Code generator for the [default] function *)

include Pb_codegen_sig.S

val gen_record_mutable : 
  gen_file_suffix:string ->
  module_prefix:string -> 
  Pb_codegen_ocaml_type.record ->
  Pb_codegen_formatting.scope -> 
  unit 
