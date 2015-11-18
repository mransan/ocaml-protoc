(** Backend for compiling Protobuffer messages to OCaml 
 *)

(** This module focuses on the compilation steps which transforms a 
    fully resolved Protobuffer message into an OCaml representation. 

    After compilation this module also expose code generation 
    functionality. 
 *)


(** {2 Compilation } *) 

val compile :
  Pbtt.resolved Pbtt.proto ->
  Pbtt.resolved Pbtt.proto_type -> 
  Ocaml_types.type_ list 

(** {2 Code Generation} *)

module Codegen : sig
  

end

(* --- Testing purpose only --- *)

val type_name : string list -> string -> string 

val constructor_name : string -> string 

val type_name_of_message : Pbtt.type_scope -> Pbtt.type_scope -> string -> string

