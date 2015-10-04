(** Low-level Protobuf codec *)

(** Type of wire format payload kinds. *)
type payload_kind =
| Varint
| Bits32
| Bits64
| Bytes

module Decoder : sig
  (** Type of failures possible while decoding. *)
  type error =
  | Incomplete
  | Overlong_varint
  | Malformed_field
  | Overflow            of string
  | Unexpected_payload  of string * payload_kind
  | Missing_field       of string
  | Malformed_variant   of string

  (** [error_to_string e] converts error [e] to its string representation. *)
  val error_to_string : error -> string

  exception Failure of error

  (** Type of wire format decoders. *)
  type t

  (** [of_bytes b] creates a decoder positioned at start of bytes [b]. *)
  val of_bytes  : bytes -> t

  (** [of_string s] creates a decoder positioned at start of string [s]. *)
  val of_string : string -> t

  (** [at_end d] returns [true] if [d] has exhausted its input, and [false]
      otherwise. *)
  val at_end    : t -> bool

  (** [skip d pk] skips the next value of kind [pk] in [d].
      If skipping the value would exhaust input of [d], raises
      [Encoding_error Incomplete]. *)
  val skip      : t -> payload_kind -> unit

  (** [varint d] reads a varint from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val varint    : t -> int64

  (** [zigzag d] reads a varint from [d] and zigzag-decodes it.
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val zigzag    : t -> int64

  (** [bits32 d] reads four bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val bits32    : t -> int32

  (** [bits64 d] reads eight bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val bits64    : t -> int64

  (** [bytes d] reads a varint indicating length and then that much
      bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val bytes     : t -> bytes

  (** [nested d] returns a decoder for a message nested in [d].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)
  val nested    : t -> t

  (** [key d] reads a key and a payload kind from [d].
      If [d] has exhausted its input when the function is called, returns [None].
      If [d] has exhausted its input while reading, raises
      [Failure Incomplete].
      If the payload kind is unknown, raises [Failure Malformed_field]. *)
  val key       : t -> (int * payload_kind) option

  (** [decode_exn f b] ≡ [f (create b)]. *)
  val decode_exn      : (t -> 'a) -> bytes -> 'a

  (** [decode f b] ≡ [try Some (decode_exn f b) with Failure _ -> None] *)
  val decode          : (t -> 'a) -> bytes -> 'a option

  (** [int_of_int32 fld v] returns [v] truncated to [int].
      If the value doesn't fit in the range of [int], raises
      [Failure (Overflow fld)]. *)
  val int_of_int32    : string -> int32 -> int

  (** [int_of_int64 fld v] returns [v] truncated to [int].
      If the value doesn't fit in the range of [int], raises
      [Failure (Overflow fld)]. *)
  val int_of_int64    : string -> int64 -> int

  (** [int32_of_int64 fld v] returns [v] truncated to [int32].
      If the value doesn't fit in the range of [int32], raises
      [Failure (Overflow fld)]. *)
  val int32_of_int64  : string -> int64 -> int32

  (** [bool_of_int64 fld v] returns [v] truncated to [bool].
      If the value doesn't fit in the range of [bool], raises
      [Failure (Overflow fld)]. *)
  val bool_of_int64   : string -> int64 -> bool
end

module Encoder : sig
  (** Type of failures possible while encoding. *)
  type error =
  | Overflow of string

  (** [error_to_string e] converts error [e] to its string representation. *)
  val error_to_string : error -> string

  exception Failure of error

  (** Type of wire format encoders. *)
  type t

  (** [create ()] creates a new encoder. *)
  val create    : unit -> t

  (** [to_string e] converts the message assembled in [e] to a string. *)
  val to_string : t -> string

  (** [to_bytes e] converts the message assembled in [e] to bytes. *)
  val to_bytes  : t -> bytes

  (** [varint i e] writes a varint [i] to [e]. *)
  val varint    : int64 -> t -> unit

  (** [zigzag i e] zigzag-encodes a varint [i] and writes it to [e]. *)
  val zigzag    : int64 -> t -> unit

  (** [bits32 i e] writes four bytes of [i] to [e]. *)
  val bits32    : int32 -> t -> unit

  (** [bits64 i e] writes eight bytes of [i] to [e]. *)
  val bits64    : int64 -> t -> unit

  (** [bytes b e] writes a varint indicating length of [b] and then
      [b] to [e]. *)
  val bytes     : bytes -> t -> unit

  (** [nested f e] applies [f] to an encoder for a message nested in [e]. *)
  val nested    : (t -> unit) -> t -> unit

  (** [key (k, pk) e] writes a key and a payload kind to [e]. *)
  val key       : (int * payload_kind) -> t -> unit

  (** [encode_exn f x] ≡ [let e = create () in f x e; to_bytes f]. *)
  val encode_exn      : ('a -> t -> unit) -> 'a -> bytes

  (** [encode f x] ≡ [try Some (encode_exn f x) with Failure _ -> None]. *)
  val encode          : ('a -> t -> unit) -> 'a -> bytes option

  (** [int32_of_int fld v] returns [v] truncated to [int32].
      If the value doesn't fit in the range of [int32], raises
      [Failure (Overflow fld)]. *)
  val int32_of_int    : string -> int -> int32

  (** [int32_of_int64 fld v] returns [v] truncated to [int32].
      If the value doesn't fit in the range of [int32], raises
      [Failure (Overflow fld)]. *)
  val int32_of_int64  : string -> int64 -> int32
end
