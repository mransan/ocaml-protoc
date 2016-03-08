(** Module to define the signature for code generator 
  *) 

module type S = sig 
  
  val gen_sig : ?and_:unit -> Ocaml_types.type_ -> Fmt.scope -> bool
  (** [gen_sig t] generates the code for the module signature *)

  val gen_struct : ?and_:unit -> Ocaml_types.type_ -> Fmt.scope -> bool
  (** [gen_struct t] generates the code for the module structure *)

  val ocamldoc_title : string 
  (** OCamldoc title *)

end (* S *)  
