 (** Code generator for the OCaml type *) 

include Pb_codegen_sig.S

val gen_record: 
  ?mutable_:unit -> 
  ?and_:unit -> 
  string -> (* module_ *)
  Pb_codegen_ocaml_type.record -> 
  Pb_codegen_formatting.scope -> 
  unit 
