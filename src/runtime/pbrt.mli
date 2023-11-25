(** Protobuf runtime library *)

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

(** Type of payload in a given field.

    This is only the wire type, the generated code will have a more precise
    type in general. *)
type payload_kind =
  | Varint
  | Bits32
  | Bits64
  | Bytes

(** Decoding protobufs. *)
module Decoder : sig
  (** {2 Types} *)

  type t
  (** The decoder *)

  (** {2 Creator} *)

  val of_bytes : bytes -> t
  (** [of_bytes b] creates a decoder positioned at start of bytes [b]. *)

  val of_subbytes : bytes -> int -> int -> t
  (** [of_subbytes b offset len] creates a decoder positioned at [offset]
      in bytes [b], reading at most [len] bytes. This is similar
      to [of_bytes (Bytes.sub b offset len)] but doesn't copy.
      @since 3.0 *)

  val of_string : string -> t
  (** [of_string s] creates a decoder positioned at start of string [s]. *)

  val of_substring : string -> int -> int -> t
  (** See {!of_subbytes}.
      @since 3.0 *)

  (** {2 Errors} *)

  type error =
    | Incomplete
    | Overlong_varint
    | Malformed_field
    | Overflow of string
    | Unexpected_payload of string * payload_kind
    | Missing_field of string
    | Malformed_variant of string

  val error_to_string : error -> string
  (** [error_to_string e] converts error [e] to its string representation. *)

  exception Failure of error
  (** Raised in decoding combinators *)

  val malformed_variant : string -> 'a
  (** [malformed_variant variant_name] raises the exception
      [Protobuf.Decoder.Failure (Malformed_variant variant_name)] *)

  val unexpected_payload : string -> payload_kind -> 'a
  (** [unexpected_payload field_name pk] raises the exception
      [Protobuf.Decoder.Failure (Unexpected_payload (field_name, pk))] *)

  val missing_field : string -> 'a
  (** [missing_field field_name] raises the exception
      [Protobuf.Decoder.Failure (Missing_field field_name)] *)

  (** {2 Decoding Functions} *)

  val key : t -> (int * payload_kind) option
  (** [key d] reads a key and a payload kind from [d].
      If [d] has exhausted its input when the function is called, returns [None].
      If [d] has exhausted its input while reading, raises
      [Failure Incomplete].
      If the payload kind is unknown, raises [Failure Malformed_field]. *)

  val skip : t -> payload_kind -> unit
  (** [skip d pk] skips the next value of kind [pk] in [d].
      If skipping the value would exhaust input of [d], raises
      [Encoding_error Incomplete]. *)

  val nested : t -> t
  (** [nested d] returns a decoder for a message nested in [d].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val map_entry : t -> decode_key:(t -> 'a) -> decode_value:(t -> 'b) -> 'a * 'b

  val empty_nested : t -> unit
  (** [empty_nested d] skips an empty message of 0 length.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val packed_fold : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [packed_fold f e0 d] folds over the a packed encoding with [f acc d] and
      initial value [e0].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int_as_varint : t -> int
  (** [int_as_varint d] reads an [int] value from [d] with [Varint] encoding.
      If the integer value read cannot be converted to [int] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int_as_zigzag : t -> int
  (** [int_as_zigzag d] reads an [int] value from [d] with zigzag encoding.
      If the integer value read cannot be converted to [int] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int32_as_varint : t -> int32
  (** [int32_as_varint d] reads an [int32] value from [d] with [Varint] encoding.
      If the integer value read cannot be converted to [int32] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int32_as_zigzag : t -> int32
  (** [int32_as_varint d] reads an [int32] value from [d] with zigzag encoding.
      If the integer value read cannot be converted to [int32] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int64_as_varint : t -> int64
  (** [int64_as_varint d] reads an [int64] value from [d] with [Varint] encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int64_as_zigzag : t -> int64
  (** [int64_as_varint d] reads an [int64] value from [d] with zigzag encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int32_as_bits32 : t -> int32
  (** [int32_as_bits32 d] reads an [int32] value from [d] with 32 bit encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int64_as_bits64 : t -> int64
  (** [int64_as_bits64 d] reads an [int64] value from [d] with 64 bit encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  (* These uint functions are like their int counterparts above, except they
     wrap their values in an [`unsigned of d] tag. *)
  val uint32_as_varint : t -> [ `unsigned of int32 ]
  val uint32_as_zigzag : t -> [ `unsigned of int32 ]
  val uint64_as_varint : t -> [ `unsigned of int64 ]
  val uint64_as_zigzag : t -> [ `unsigned of int64 ]
  val uint32_as_bits32 : t -> [ `unsigned of int32 ]
  val uint64_as_bits64 : t -> [ `unsigned of int64 ]

  val bool : t -> bool
  (** [bool d] reads a [bool] value from [d] with varing encoding.
      If the boolean value in [d] is neither 0 or 1 raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val float_as_bits32 : t -> float
  (** [float_as_bits32 d] reads a [float] value from [d] with 32 bit encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val float_as_bits64 : t -> float
  (** [float_as_bits64 d] reads a [float] value from [d] with 64 bit encoding.
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int_as_bits32 : t -> int
  (** [int_as_bits32 d] reads a [int] value from [d] with 32 bit encoding.
      If the integer value read cannot be converted to [int] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val int_as_bits64 : t -> int
  (** [int_as_bits64 d] reads a [int] value from [d] with 64 bit encoding.
      If the integer value read cannot be converted to [int] raises
      [Failure Overflow ""].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val string : t -> string
  (** [string d] reads a [string] value from [d].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val bytes : t -> bytes
  (** [bytes d] reads a [bytes] value from [d].
      If reading the message would exhaust input of [d], raises
      [Failure Incomplete]. *)

  val wrapper_double_value : t -> float option
  val wrapper_float_value : t -> float option
  val wrapper_int64_value : t -> int64 option
  val wrapper_int32_value : t -> int32 option
  val wrapper_bool_value : t -> bool option
  val wrapper_string_value : t -> string option
  val wrapper_bytes_value : t -> bytes option
end
(* Decoder *)

(** Encoding protobufs. *)
module Encoder : sig
  (** {2 Types} *)

  type t
  (** A (mutable) encoder.

      This encoder can be re-used, see {!clear}. *)

  (** {2 Error} *)

  type error = Overflow of string

  val error_to_string : error -> string

  exception Failure of error

  (** {2 Creator} *)

  val create : ?size:int -> unit -> t
  (** Create a new encoder. *)

  val clear : t -> unit
  (** Clear the content of the internal buffer(s), but does not release memory.
      This makes the encoder ready to encode another message.
      @param size initial size in bytes
      @since 2.1 *)

  val reset : t -> unit
  (** Clears the content and resets internal storage
      to its initial memory consumption.

      This is more costly than {!clear} but
      can be useful after a very large message was encoded.
      @since 2.1 *)

  (** {2 Convertion} *)

  val to_bytes : t -> bytes
  (** Extract the content of the encoder to bytes. *)

  val to_string : t -> string
  (** Extract the content of the encoder to a string. Call this after
      encoding a message into the encoder. *)

  val write_chunks : (bytes -> int -> int -> unit) -> t -> unit
  (** [write_chunks w e] calls the write function [w]
      (e.g [output oc] for some output channel [oc]) on every chunk
      inside [e]. The number of chunks is an implementation detail.
      @since 2.1 *)

  (** {2 Encoding Functions}

      These combinators are used by generated code (or user combinators)
      to encode a OCaml value into the wire representation of protobufs. *)

  val key : int * payload_kind -> t -> unit
  (** [key (k, pk) e] writes a key and a payload kind to [e]. *)

  val nested : ('a -> t -> unit) -> 'a -> t -> unit
  (** [nested f x e] applies [f x] to an encoder for a message nested in [e]. *)

  val map_entry :
    encode_key:('a -> t -> unit) ->
    encode_value:('b -> t -> unit) ->
    ('a * payload_kind) * ('b * payload_kind) ->
    t ->
    unit

  val empty_nested : t -> unit
  (** [nested f e] encodes a zero length empty message *)

  val int_as_varint : int -> t -> unit
  (** [int_as_varint i e] encodes [i] in [e] with [Varint] encoding *)

  val int_as_zigzag : int -> t -> unit
  (** [int_as_zigzag i e] encodes [i] in [e] with [Varint] zigzag encoding *)

  val int32_as_varint : int32 -> t -> unit
  (** [int32_as_varint i e] encodes [i] in [e] with [Varint] encoding *)

  val int32_as_zigzag : int32 -> t -> unit
  (** [int32_as_varint i e] encodes [i] in [e] with [Varint] zigzag encoding *)

  val int64_as_varint : int64 -> t -> unit
  (** [int64_as_varint i e] encodes [i] in [e] with [Varint] encoding *)

  val int64_as_zigzag : int64 -> t -> unit
  (** [int64_as_varint i e] encodes [i] in [e] with [Varint] zigzag encoding *)

  val int32_as_bits32 : int32 -> t -> unit
  (** [int32_as_varint i e] encodes [i] in [e] with [Bits32] encoding *)

  val int64_as_bits64 : int64 -> t -> unit
  (** [int64_as_varint i e] encodes [i] in [e] with [Bits64] encoding *)

  val uint32_as_varint : [ `unsigned of int32 ] -> t -> unit
  val uint32_as_zigzag : [ `unsigned of int32 ] -> t -> unit
  val uint64_as_varint : [ `unsigned of int64 ] -> t -> unit
  val uint64_as_zigzag : [ `unsigned of int64 ] -> t -> unit
  val uint32_as_bits32 : [ `unsigned of int32 ] -> t -> unit
  val uint64_as_bits64 : [ `unsigned of int64 ] -> t -> unit

  val bool : bool -> t -> unit
  (** [encode b e] encodes [b] in [e] with [Varint] encoding *)

  val float_as_bits32 : float -> t -> unit
  (** [float_as_bits32 f e] encodes [f] in [e] with [Bits32] encoding *)

  val float_as_bits64 : float -> t -> unit
  (** [float_as_bits64 f e] encodes [f] in [e] with [Bits64] encoding *)

  val int_as_bits32 : int -> t -> unit
  (** [int_as_bits32 i e] encodes [i] in [e] with [Bits32] encoding
      TODO : add error handling
   *)

  val int_as_bits64 : int -> t -> unit
  (** [int_as_bits64 i e] encodes [i] in [e] with [Bits64] encoding
   *)

  val string : string -> t -> unit
  (** [string s e] encodes [s] in [e] *)

  val bytes : bytes -> t -> unit
  (** [string s e] encodes [s] in [e] *)

  val wrapper_double_value : float option -> t -> unit
  val wrapper_float_value : float option -> t -> unit
  val wrapper_int64_value : int64 option -> t -> unit
  val wrapper_int32_value : int32 option -> t -> unit
  val wrapper_bool_value : bool option -> t -> unit
  val wrapper_string_value : string option -> t -> unit
  val wrapper_bytes_value : bytes option -> t -> unit
end

module List_util : sig
  val rev_iter : ('a -> unit) -> 'a list -> unit
  (** [iter_rev f l] iterate over the list in reverse order *)
end

(** Optimized representation for repeated fields *)
module Repeated_field : sig
  type 'a t
  (** optimized data structure for fast inserts so that decoding
      of repeated fields can be efficient.

      Type can be constructed at no cost from an existing array.
    *)

  val make : 'a -> 'a t
  (** [make v] create an initial repeated field container [v] is
      not used but needed to initialize the internal array
      data structure.

      This design flow is intentional to keep optimal
      performance.

      Therefore [lengh (make 1)] will return [0].
   *)

  val of_array_no_copy : 'a array -> 'a t
  (** [of_array_no_copy a] initialized a new repeated field
      container with [a].

      [a] is not copied into [a] but only referenced so any later
      modification to any [a] element will affected [a t] container.
    *)

  val length : 'a t -> int
  (** [length c] returns the number of insterted element in [c].
    *)

  val add : 'a -> 'a t -> unit
  (** [add x c] appends [a] to container [c]

      This operation is not constant time since it might trigger
      an alocation of an array. However it is optimized for the
      total insert time of element one by one.
   *)

  val to_array : 'a t -> 'a array
  (** [to_array c] convert the repeated field container to an
      array.
   *)

  val to_list : 'a t -> 'a list
  (** [to_list c] convert the repeated field container to an
      list.
   *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f c] applies [f] to all element in [c] *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** [iteri f c] applies [f] to all element in [c] *)

  val rev_iter : ('a -> unit) -> 'a t -> unit

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** [fold_left f e0 c] accumulates [e0] through each elements *)

  val map_to_array : ('a -> 'b) -> 'a t -> 'b array
  (** [map_to_array f c] map all [c] element to an array containing [f e_i]
      element.
   *)

  val map_to_list : ('a -> 'b) -> 'a t -> 'b list
  (** [map_to_list f c] map all [c] element to a list containing [f e_i]
      element.
   *)
end

(** Runtime functions for Pretty Printing functionality
 *)
module Pp : sig
  type formatter = Format.formatter

  val pp_unit : formatter -> unit -> unit
  (** [pp_unit fmt ()] formats [unit] value *)

  val pp_int : formatter -> int -> unit
  (** [pp_unit fmt i] formats [i] value *)

  val pp_float : formatter -> float -> unit
  (** [pp_unit fmt f] formats [f] value *)

  val pp_bool : formatter -> bool -> unit
  (** [pp_unit fmt b] formats [b] value *)

  val pp_int32 : formatter -> int32 -> unit
  (** [pp_unit fmt i] formats [i] value *)

  val pp_unsigned_of_int32 : formatter -> [ `unsigned of int32 ] -> unit
  (* @since 2.4 *)

  val pp_int64 : formatter -> int64 -> unit
  (** [pp_unit fmt i] formats [i] value *)

  val pp_unsigned_of_int64 : formatter -> [ `unsigned of int64 ] -> unit
  (* @since 2.4 *)

  val pp_string : formatter -> string -> unit
  (** [pp_unit fmt s] formats [s] value *)

  val pp_bytes : formatter -> bytes -> unit
  (** [pp_unit fmt b] formats [b] value *)

  val pp_option : (formatter -> 'a -> unit) -> formatter -> 'a option -> unit
  (** [pp_option f fmt o] formats an option value [o] using [f] formatter when
      [o] is a [Some x] value
   *)

  val pp_wrapper_float : formatter -> float option -> unit
  val pp_wrapper_bool : formatter -> bool option -> unit
  val pp_wrapper_int32 : formatter -> int32 option -> unit
  val pp_wrapper_int64 : formatter -> int64 option -> unit
  val pp_wrapper_string : formatter -> string option -> unit
  val pp_wrapper_bytes : formatter -> bytes option -> unit

  val pp_list : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  (** [pp_list f fmt l] formats a list value [l] using [f] formatter on each
      of the elements.
   *)

  val pp_associative_list :
    (formatter -> 'a -> unit) ->
    (formatter -> 'b -> unit) ->
    formatter ->
    ('a * 'b) list ->
    unit

  val pp_hastable :
    (formatter -> 'a -> unit) ->
    (formatter -> 'b -> unit) ->
    formatter ->
    ('a, 'b) Hashtbl.t ->
    unit

  val pp_record_field :
    ?first:bool ->
    string ->
    (formatter -> 'a -> unit) ->
    formatter ->
    'a ->
    unit
  (** [pp_record_field label_name fmt field_value] formats a
      record [field_value] with [label_name] *)

  val pp_brk : (formatter -> 'a -> unit) -> formatter -> 'a -> unit
  (** [pp_brk fmt r] formats record value [r] with curly brakets.  *)
end
