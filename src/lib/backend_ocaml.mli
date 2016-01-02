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
(** [module_of_file_name filename] returns the corresponding OCaml module name
  *)

val rev_split_by_naming_convention : string -> string list 
(** [rev_split_by_naming_convention s] will split [s] according to the protobuf
    coding style convention. The rule split are 
    {ul
    {- character ['_'] is a separator}
    {- the first uppercase letter after a lower case is a separator (ie FooBar will be split into [ ["Bar";"Foo"] ]}  
    }
 *)

(* --- Testing purpose only --- *)

val type_name : string list -> string -> string 

val constructor_name : string -> string 
