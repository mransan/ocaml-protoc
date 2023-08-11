(** Protobuf JSON encoding runtime *)

(** All exception which could be raised by the generated JSON encoder
    and decode function *)
module E : sig
  type error =
    | Unexpected_json_type of string * string
    | Malformed_variant of string

  exception Failure of error
  (** Decoding/Encoding failure *)

  val unexpected_json_type : string -> string -> 'a
  (** [unexpected_json_type record_name field_name] raises 
      [Failure (Unexpected_json_type (record_name, field_name))] *)

  val malformed_variant : string -> 'a
  (** [malformed_variant variant_name] raise 
      [Failure (Malformed_variant variant_name)] *)
end

(** Helper module for the generated code for common 
    functionality *)

val string : Yojson.Basic.t -> string -> string -> string
val float : Yojson.Basic.t -> string -> string -> float
val int32 : Yojson.Basic.t -> string -> string -> int32
val int64 : Yojson.Basic.t -> string -> string -> int64
val int : Yojson.Basic.t -> string -> string -> int
val bool : Yojson.Basic.t -> string -> string -> bool
val bytes : Yojson.Basic.t -> string -> string -> bytes
val make_bool : bool -> Yojson.Basic.t
val make_int : int -> Yojson.Basic.t
val make_float : float -> Yojson.Basic.t
val make_string : string -> Yojson.Basic.t
val make_bytes : bytes -> Yojson.Basic.t
val make_list : Yojson.Basic.t list -> Yojson.Basic.t
