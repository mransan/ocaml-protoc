module type S = sig 
  
  val gen_sig : 
    ?and_:unit -> 
    Pb_codegen_ocaml_type.type_ -> 
    Pb_codegen_formatting.scope -> 
    bool

  val gen_struct : 
    ?and_:unit -> 
    Pb_codegen_ocaml_type.type_ -> 
    Pb_codegen_formatting.scope -> 
    bool
  
  val ocamldoc_title : string 

  val file_suffix : string 
end (* S *)  
