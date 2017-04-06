(** Common module signature for all codegen modules *)

(*type context = [| `Single_file | `Multi_file |] *)

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

  val file_suffix : string 
  (** The suffix part of the generated file which will contain the 
      struct and sig *) 

end (* S *)  
