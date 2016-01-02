(*
  Copyright (c) 2014 Peter Zotov <whitequark@whitequark.org>
  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
    The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*)

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
  
  (** [varint d] reads a varint to a int from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int_as_varint : t -> int 

  (** [varint d] reads a varint to an int64 from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int64_as_varint : t -> int64
  
  (** [varint d] reads a varint to an int32 from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int32_as_varint : t -> int32
  
  (** [int_as_zigzag d] reads a varint from [d] to an int and zigzag-decodes it.
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int_as_zigzag : t -> int

  (** [int64_as_zigzag d] reads a varint from [d] to an int64 and zigzag-decodes it.
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int64_as_zigzag : t -> int64
  
  (** [int32_as_zigzag d] reads a varint from [d] to an int32 and zigzag-decodes it.
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int32_as_zigzag : t -> int32

  (** [int32_as_bits32d] reads four bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int32_as_bits32 : t -> int32

  (** [bits64 d] reads eight bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val int64_as_bits64 : t -> int64
  
  (** [int_as_bits32 d] reads a int value encoded in 32 bits. *)
  val int_as_bits32 : t -> int 
  
  (** [int_as_bits64 d] reads a int value encoded in 64 bits. *)
  val int_as_bits64 : t -> int 

  (** [bytes d] reads a varint indicating length and then that much
      bytes from [d].
      If [d] has exhausted its input, raises [Failure Incomplete]. *)
  val bytes     : t -> bytes
  
  (** [nested d] returns a decoder for a message nested in [d].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)
  val nested    : t -> t

  (** [bool d] reads a boolean value. Boolean value is *)
  val bool : t -> bool 
  
  (** [float_as_bits32 d] reads a floating point value encoded in 32 bits. *)
  val float_as_bits32 : t -> float
  
  (** [float_as_bits64 d] reads a floating point value encoded in 64 bits. *)
  val float_as_bits64 : t -> float
  
  (** [string d] reads a string value encoded as a byte sequence. *)
  val string : t -> string

  (** [key d] reads a key and a payload kind from [d].
      If [d] has exhausted its input when the function is called, returns [None].
      If [d] has exhausted its input while reading, raises
      [Failure Incomplete].
      If the payload kind is unknown, raises [Failure Malformed_field]. *)
  val key       : t -> (int * payload_kind) option

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

(** This module are provide helper routines for the generated code 
    There should not be used by other client application.
 *) 
module Codegen : sig 
  
  type 'a state = ([> `Default] as 'a) array 

  val programatic_error : int -> 'a 

  val decode : Decoder.t -> (Decoder.t -> (int * 'a) -> 'a) -> 'a state -> unit 

  val required : int -> 'a state -> ('a -> 'b list) -> 'b 

  val optional : int -> 'a state -> ('a -> 'b list) -> 'b option 
  
  val list_ : int -> 'a state -> ('a -> 'b list) -> 'b list 

  val oneof : int list -> 'a state -> ('a -> 'b list) -> 'b 
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
  
  (** [int_as_varint i e] encodes an [int] [i] as a varint to [e]. *)
  val int_as_varint : int -> t -> unit

  (** [int64_as_varint i e] encodes an [int64] [i] as a varint to [e] *)
  val int64_as_varint : int64 -> t -> unit

  (** [int32_as_varinti e] encodes an [int32] [i] as a varint to [e] *)
  val int32_as_varint : int32 -> t -> unit
  
  (** [int_as_zigzag e] zigzag-encodes an [int] [i] to [e]. *)
  val int_as_zigzag : int -> t -> unit

  (** [int64_as_zigzag i e] zigzag-encodes an [int64] [i] to [e]. *)
  val int64_as_zigzag : int64 -> t -> unit
  
  (** [int32_as_zigzag i e] zigzag-encodes an [int32] [i] to [e] *)
  val int32_as_zigzag : int32 -> t -> unit

  (** [bits32 i e] writes four bytes of [i] to [e]. *)
  val int32_as_bits32    : int32 -> t -> unit

  (** [bits64 i e] writes eight bytes of [i] to [e]. *)
  val int64_as_bits64    : int64 -> t -> unit
  
  (** [int_as_bits32 i e] writes four bytes of [i] to [e]. *)
  val int_as_bits32 : int -> t -> unit

  (** [int64_as_bits64 i e] writes eight bytes of [i] to [e]. *)
  val int_as_bits64 : int -> t -> unit

  (** [float_as_bits32 v e] writes four bytes of [v] to [e]. *)
  val float_as_bits32 : float -> t -> unit
  
  (** [float_as_bits64 v e] writes eight bytes of [v] to [e]. *)
  val float_as_bits64 : float -> t -> unit

  (** [bytes b e] writes a varint indicating length of [b] and then
      [b] to [e]. *)
  val bytes     : bytes -> t -> unit

  (** [string b e] writes a varint indicating length of [b] and then
      [b] to [e]. *)
  val string     : string -> t -> unit

  (** [bool b e] writes [b] as a varint of either [0] or [1]. 
    *)
  val bool     : bool -> t -> unit
  
  (** [nested f e] applies [f] to an encoder for a message nested in [e]. *)
  val nested    : (t -> unit) -> t -> unit

  (** [key (k, pk) e] writes a key and a payload kind to [e]. *)
  val key       : (int * payload_kind) -> t -> unit

  (** [int32_of_int fld v] returns [v] truncated to [int32].
      If the value doesn't fit in the range of [int32], raises
      [Failure (Overflow fld)]. *)
  val int32_of_int    : string -> int -> int32

  (** [int32_of_int64 fld v] returns [v] truncated to [int32].
      If the value doesn't fit in the range of [int32], raises
      [Failure (Overflow fld)]. *)
  val int32_of_int64  : string -> int64 -> int32
end
