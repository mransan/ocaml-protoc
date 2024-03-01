(** Protobuf File/Message/Field raw options (i.e. exactly as they parsed) *)

type name_part =
  | Simple_name of string
  | Extension_name of string

type option_name = name_part list
type t = option_name * Pb_option.value
type set = t list

val stringify_option_name : option_name -> string
val empty : set
val add : set -> option_name -> Pb_option.value -> set

val merge : set -> set -> set
(** [merge s1 s2] adds all the options from [s2] to [s1]. This means
    than in case of duplicates [s2] options will override [s1] options. *)

val get : set -> option_name -> Pb_option.value option
val get_ext : set -> string -> Pb_option.value option
val get_simple : set -> string -> Pb_option.value option
val pp_t : Format.formatter -> t -> unit
val pp_set : Format.formatter -> set -> unit
