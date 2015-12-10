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

val pop_last : 'a list -> 'a list 
(** [pop_last l] removes the last element from the list *)

val apply_until : ('a -> 'b option) -> 'a list -> 'b option 
(** [apply_until f l] applies [f ei] until it returns [Some x] 
    
    If [f] returns [None] then [None] is returned. 
 *)

val is_list_empty : 'a list -> bool
(** [is_list_empty l] returns true is the list is empty, false otherwise *)

val string_of_string_list : string list -> string 
(** [string_of_string_list l] returns a debug string of [l] *)
