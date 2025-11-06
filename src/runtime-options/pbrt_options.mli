(** Protobuf *)

(** All exceptions which could be raised by the generated option parsers. *)
module E : sig
  type error =
    | Unexpected_option_type of string * string
    | Malformed_variant of string

  exception Failure of error
  (** Decoding/Encoding failure *)

  val unexpected_option_type : string -> string -> 'a
  (** [unexpected_option_type record_name field_name] raises
      [Failure (Unexpected_json_type (record_name, field_name))] *)

  val malformed_variant : string -> 'a
  (** [malformed_variant variant_name] raise
      [Failure (Malformed_variant variant_name)] *)
end

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

(** Helper module for the generated code for common functionality *)

val string : value -> string -> string -> string
val float : value -> string -> string -> float
val int32 : value -> string -> string -> int32
val int64 : value -> string -> string -> int64
val int : value -> string -> string -> int
val bool : value -> string -> string -> bool
val bytes : value -> string -> string -> bytes
val unit : value -> string -> string -> unit
