(** Common utility functions for OCaml code generation *)

val sp : ('a, unit, string) format -> 'a
(** [sp x] same as sprintf but prefixed with new line *)

val let_decl_of_and : 'a option -> string
(** [let_decl_of_and and_] returns the function declaration ["let"] (when ?and_
   is [None]), ["and"] otherwise.
 *)

val string_of_field_type :?type_qualifier:Ocaml_types.type_qualifier -> Ocaml_types.field_type  -> string 
(** [string_of_field_type ~qualifier type] returns the string for OCaml type. *) 
 
val function_name_of_user_defined : string -> Ocaml_types.user_defined_type -> string
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)

val caml_file_name_of_proto_file_name : string -> string
(* [caml_file_name_of_proto_file_name filename] returns the OCaml file name from
   the protobuf file name
 *)
