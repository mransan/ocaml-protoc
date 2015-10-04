(** Miscellaneous functionality *)

val rev_split_by_char : char -> string -> string list 
(** [rev_split_by_char c s] will split the string [s] using the delimieter [c]
    and return the component in reverse order (ie from right to left). 
    
    For instance when splitting a filename with the '.' character, the file
    extension will be the head of the returned list. 
 *)

val concat : string list -> string 
(** [concat l] concatenate a string list without delimited between the given string
 *)
