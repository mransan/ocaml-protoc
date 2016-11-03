(** Formatting utilities for code generation *)

(* TODO: add a little example of how to use this module as well
 * as a unit test for it *)

(** {2 types} *) 

type scope 
(** A scope is a formatting container which can contains line of text as well 
    as other nested scopes

    In other word a scope define an indentation section.
 *)

(** {2 Creation} *) 

val empty_scope : unit -> scope 
(** [empty_scope ()] returns a brand new scope *)

val line : scope -> string -> unit 
(** [line scope s] adds [s] to [scope] *)

val empty_line : scope -> unit 
(** [empty_line scope] adds an empty line to [scope] *)

val scope : scope -> (scope -> unit) -> unit 
(** [scope scope f] adds a sub scope and apply [f] to it. *)

(** {2 Printing} *) 

val print : scope -> string 
(** [print scope] returns the formatted scops with a 2 character 
    indentation for each scope. 
 *)
