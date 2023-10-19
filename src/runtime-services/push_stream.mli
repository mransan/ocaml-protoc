(** Producer end of a stream, into which we can push values *)

type 'a t = {
  push: 'a -> unit;
  close: unit -> unit;
}
(** Stream of outgoing values, we can push new ones until we close it *)

val push : 'a t -> 'a -> unit
val close : _ t -> unit
