(** Formatting utilities for code generation *)

(** {2 types} *) 

type scope 
(** A scope is a formatting container which can contains line and other
    scopes. 
 *)

(** {2 Creation} *) 

val empty_scope : unit -> scope 
(** [empty_scope ()] returns a brand new scope *)

val line : scope -> string -> unit 
(** [line scope s] adds [s] to [scope] *)

val scope : scope -> (scope -> unit) -> unit 
(** [scope scope f] adds a sub scope and apply [f] to it. *)

(** {2 Printing} *) 

val print : scope -> string 
(** [print scope] returns the formatted scops with a 2 character 
    indentation for each scope. 
 *)
