module type S = sig 
  
  val gen_sig : 
    ?and_:unit -> 
    Ocaml_types.type_ -> 
    Pb_codegen_formatting.scope -> 
    bool

  val gen_struct : 
    ?and_:unit -> 
    Ocaml_types.type_ -> 
    Pb_codegen_formatting.scope -> 
    bool
  
  val ocamldoc_title : string 

end (* S *)  
