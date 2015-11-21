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

val module_of_file_name : string -> string 

(* --- Testing purpose only --- *)

val type_name : string list -> string -> string 

val constructor_name : string -> string 
