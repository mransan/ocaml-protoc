 (** Code generator for the OCaml type *) 

include Pb_codegen_sig.S

val gen_record_mutable: 
  string -> (* module_prefix *)
  Pb_codegen_ocaml_type.record -> 
  Pb_codegen_formatting.scope -> 
  unit 
