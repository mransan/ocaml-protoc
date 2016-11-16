(** Common module signature for all codegen modules *)

module type S = sig 
  
  val gen_sig : 
    ?and_:unit -> 
    Pb_codegen_ocaml_type.type_ -> 
    Pb_codegen_formatting.scope -> bool
  (** [gen_sig t] generates the code for the module signature *)

  val gen_struct : 
    ?and_:unit -> 
    Pb_codegen_ocaml_type.type_ -> 
    Pb_codegen_formatting.scope -> bool
  (** [gen_struct t] generates the code for the module structure *)

  val ocamldoc_title : string 
  (** OCamldoc title *)

end (* S *)  
