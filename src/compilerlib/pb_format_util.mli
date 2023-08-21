val pp_print_option :
  ?none:('a -> unit -> unit) -> ('a -> 'b -> unit) -> 'a -> 'b option -> unit

val pp_none : Format.formatter -> unit -> unit
