(** Protobuf File/Message/Field options *)

(** Protobuf constance
    
    As defined in: {{:https://goo.gl/FzpQY2} Protobuf Language Spec}. *) 
type constant = 
  | Constant_string of string 
  | Constant_bool of bool 
  | Constant_int of int 
  | Constant_float of float 
  | Constant_litteral of string 

(** Option identifier *)
type option_name = string 

type t = option_name * constant 

(** Collection of options
 
    Can be used for field/message or file options *)
type set 

val empty : set

val add : set -> string -> constant -> set 

val merge : set -> set -> set 
(** [merge s1 s2] adds all the options from [s2] to [s1]. This means
    than in case of duplicates [s2] options will override [s1] options. *)

val get : set -> string -> constant option 
