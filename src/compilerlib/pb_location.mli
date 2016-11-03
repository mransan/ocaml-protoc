(** File Location utilities *)

type t 
(** Location type *)

(** {2 Creators} *)

val from_lexbuf : Lexing.lexbuf -> t 
(** [from_lexbuf lexbuf] create a location from the current lexbuf location *)

(** {2 Accessors} *) 

(** {2 Utilities} *)

val to_string : t -> string 
(** [to_string loc] convert to the compiler error string *) 
