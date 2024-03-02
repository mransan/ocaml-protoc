(** Protobuf File/Message/Field options *)

(** Protobuf constant

    As defined in: {{:https://goo.gl/FzpQY2} Protobuf Language Spec}. *)
type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type message_literal = (string * value) list
and list_literal = value list

and value =
  | Scalar_value of constant
  | Message_literal of message_literal
  | List_literal of list_literal

(** Option identifier *)
type option_name =
  | Simple_name of string
  | Extension_name of string

type t = option_name * value

type set = t list
(** Collection of options

    Can be used for field/message or file options *)

val stringify_option_name : option_name -> string
val empty : set
val add : set -> option_name -> value -> set

val get : set -> option_name -> value option
val get_ext : set -> string -> value option
val pp_constant : Format.formatter -> constant -> unit
val pp_value : Format.formatter -> value -> unit
val pp_message_literal : Format.formatter -> message_literal -> unit
val pp_message_field : Format.formatter -> string * value -> unit
val pp_t : Format.formatter -> t -> unit
val pp_set : Format.formatter -> set -> unit
