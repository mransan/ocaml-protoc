(** File Location utilities *)

type t 
(** Location type *)

(** {2 Creators} *)

val make : ?file_name:string -> int -> t 
(** [make ~file_name line] create a location at [file_name] and [line] *) 

val from_lexbuf : Lexing.lexbuf -> t 
(** [from_lexbuf lexbuf] create a location from the current lexbuf location *)

(** {2 Accessors} *) 

val file_name : t -> string option
(** [file_name loc] returns the file name *)

val line : t -> int 
(** [line loc] returns the line *)


(** {2 Utilities} *)

val to_string : t -> string 
(** [to_string loc] convert to the compiler error string *) 
