(** Protobuf JSON encoding runtime *)

(** All exception which could be raised by the generated JSON encoder and decode
    function *)
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

open Ocaml_protoc_compiler_lib

(** Helper module for the generated code for common functionality *)

val string : Pb_option.value -> string -> string -> string
val float : Pb_option.value -> string -> string -> float
val int32 : Pb_option.value -> string -> string -> int32
val int64 : Pb_option.value -> string -> string -> int64
val int : Pb_option.value -> string -> string -> int
val bool : Pb_option.value -> string -> string -> bool
val bytes : Pb_option.value -> string -> string -> bytes
val unit : Pb_option.value -> string -> string -> unit
