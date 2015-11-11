(*
Copyright (c) 2014 Peter Zotov <whitequark@whitequark.org>

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

type payload_kind =
| Varint
| Bits32
| Bits64
| Bytes

let min_int_as_int32, max_int_as_int32 = Int32.of_int min_int, Int32.of_int max_int
let min_int_as_int64, max_int_as_int64 = Int64.of_int min_int, Int64.of_int max_int
let min_int32_as_int64, max_int32_as_int64 =
  Int64.of_int32 Int32.min_int, Int64.of_int32 Int32.max_int
let min_int32_as_int, max_int32_as_int =
  if Sys.word_size = 64 then Int32.to_int Int32.min_int, Int32.to_int Int32.max_int
  else 0, 0

module Decoder = struct
  type error =
  | Incomplete
  | Overlong_varint
  | Malformed_field
  | Overflow            of string
  | Unexpected_payload  of string * payload_kind
  | Missing_field       of string
  | Malformed_variant   of string

  let error_to_string e =
    match e with
    | Incomplete -> "Incomplete"
    | Overlong_varint -> "Overlong_varint"
    | Malformed_field -> "Malformed_field"
    | Overflow fld ->
      Printf.sprintf "Overflow(%S)" fld
    | Unexpected_payload (field, kind) ->
      let kind' =
        match kind with
        | Varint -> "Varint"
        | Bits32 -> "Bits32"
        | Bits64 -> "Bits64"
        | Bytes  -> "Bytes"
      in
      Printf.sprintf "Unexpected_payload(%S, %s)" field kind'
    | Missing_field field ->
      Printf.sprintf "Missing_field(%S)" field
    | Malformed_variant name ->
      Printf.sprintf "Malformed_variant(%S)" name

  exception Failure of error

  let () =
    Printexc.register_printer (fun exn ->
      match exn with
      | Failure e -> Some (Printf.sprintf "Protobuf.Decoder.Failure(%s)" (error_to_string e))
      | _         -> None)

  let int_of_int32 fld v =
    if Sys.word_size = 32 && (v < min_int_as_int32 || v > max_int_as_int32) then
      raise (Failure (Overflow fld));
    Int32.to_int v

  let int_of_int64 fld v =
    if (v < min_int_as_int64 || v > max_int_as_int64) then
      raise (Failure (Overflow fld));
    Int64.to_int v

  let int32_of_int64 fld v =
    if (v < min_int32_as_int64 || v > max_int32_as_int64) then
      raise (Failure (Overflow fld));
    Int64.to_int32 v

  let bool_of_int64 fld v =
    if v = Int64.zero then false
    else if v = Int64.one then true
    else raise (Failure (Overflow fld))

  type t = {
            source : bytes;
            limit  : int;
    mutable offset : int;
  }

  let of_bytes source =
    { source;
      offset = 0;
      limit  = Bytes.length source; }

  let of_string source =
    { source = Bytes.of_string source;
      offset = 0;
      limit  = String.length source; }

  let decode_exn f source =
    f (of_bytes source)

  let decode f source =
    try Some (decode_exn f source) with Failure _ -> None

  let at_end d =
    d.limit = d.offset

  let byte d =
    if d.offset >= d.limit then
      raise (Failure Incomplete);
    let byte = int_of_char (Bytes.get d.source d.offset) in
    d.offset <- d.offset + 1;
    byte

  let varint d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int64.(logor (shift_left (logand (of_int b) 0x7fL) s) (read (s + 7)))
      else if s < 56 || (b land 0x7f) <= 1 then
        Int64.(shift_left (of_int b) s)
      else
        raise (Failure Overlong_varint)
    in
    read 0

  let zigzag d =
    let v = varint d in
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))

  let bits32 d =
    let b1 = byte d in
    let b2 = byte d in
    let b3 = byte d in
    let b4 = byte d in
    Int32.(add (shift_left (of_int b4) 24)
           (add (shift_left (of_int b3) 16)
            (add (shift_left (of_int b2) 8)
             (of_int b1))))

  let bits64 d =
    let b1 = byte d in
    let b2 = byte d in
    let b3 = byte d in
    let b4 = byte d in
    let b5 = byte d in
    let b6 = byte d in
    let b7 = byte d in
    let b8 = byte d in
    Int64.(add (shift_left (of_int b8) 56)
           (add (shift_left (of_int b7) 48)
            (add (shift_left (of_int b6) 40)
             (add (shift_left (of_int b5) 32)
              (add (shift_left (of_int b4) 24)
               (add (shift_left (of_int b3) 16)
                (add (shift_left (of_int b2) 8)
                 (of_int b1))))))))

  let bytes d =
    (* strings are always shorter than range of int *)
    let len = Int64.to_int (varint d) in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let str = Bytes.sub d.source d.offset len in
    d.offset <- d.offset + len;
    str

  let nested d =
    (* strings are always shorter than range of int *)
    let len = Int64.to_int (varint d) in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let d' = { d with limit = d.offset + len; } in
    d.offset <- d.offset + len;
    d'

  let key d =
    if d.offset = d.limit
    then None
    else
      (* keys are always in the range of int, but prefix might only fit into int32 *)
      let prefix  = varint d in
      let key, ty = Int64.(to_int (shift_right prefix 3)), Int64.logand 0x7L prefix in
      match ty with
      | 0L -> Some (key, Varint)
      | 1L -> Some (key, Bits64)
      | 2L -> Some (key, Bytes)
      | 5L -> Some (key, Bits32)
      | _  -> raise (Failure Malformed_field)

  let skip d kind =
    let skip_len n =
      if d.offset + n > d.limit then
        raise (Failure Incomplete);
      d.offset <- d.offset + n
    in
    let rec skip_varint () =
      let b = byte d in
      if b land 0x80 <> 0 then skip_varint () else ()
    in
    match kind with
    | Bits32 -> skip_len 4
    | Bits64 -> skip_len 8
    (* strings are always shorter than range of int *)
    | Bytes  -> skip_len (Int64.to_int (varint d))
    | Varint -> skip_varint ()
end

module Encoder = struct
  type error =
  | Overflow of string

  let error_to_string e =
    match e with
    | Overflow fld -> Printf.sprintf "Overflow(%S)" fld

  exception Failure of error

  let () =
    Printexc.register_printer (fun exn ->
      match exn with
      | Failure e -> Some (Printf.sprintf "Protobuf.Encoder.Failure(%s)" (error_to_string e))
      | _         -> None)

  type t = Buffer.t

  let create () =
    Buffer.create 16

  let to_string = Buffer.contents

  let to_bytes = Buffer.to_bytes

  let encode_exn f x =
    let e = create () in f x e; to_bytes e

  let encode f x =
    try Some (encode_exn f x) with Failure _ -> None

  let varint i e =
    let rec write i =
      if Int64.(logand i (lognot 0x7fL)) = Int64.zero then
        Buffer.add_char e (char_of_int Int64.(to_int (logand 0x7fL i)))
      else begin
        Buffer.add_char e (char_of_int Int64.(to_int (logor 0x80L (logand 0x7fL i))));
        write (Int64.shift_right_logical i 7)
      end
    in
    write i

  let smallint i e =
    varint (Int64.of_int i) e

  let zigzag i e =
    varint Int64.(logxor (shift_left i 1) (shift_right i 63)) e

  let bits32 i e =
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl i)));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))

  let bits64 i e =
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL i)));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 8))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 16))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 24))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 32))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 40))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 48))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 56))))

  let bytes b e =
    smallint (Bytes.length b) e;
    Buffer.add_bytes e b

  let nested f e =
    let e' = Buffer.create 16 in
    f e';
    smallint (Buffer.length e') e;
    Buffer.add_buffer e e'

  let key (k, pk) e =
    let pk' =
      match pk with
      | Varint -> 0
      | Bits64 -> 1
      | Bytes  -> 2
      | Bits32 -> 5
    in
    smallint (pk' lor (k lsl 3)) e

  let int32_of_int64 fld v =
    if (v < min_int32_as_int64 || v > max_int32_as_int64) then
      raise (Failure (Overflow fld));
    Int64.to_int32 v

  let int32_of_int fld v =
    if Sys.word_size = 64 && (v < min_int32_as_int || v > max_int32_as_int) then (
      Printf.printf "overflow for %i\n" v; 
      raise (Failure (Overflow fld));
    );
    Int32.of_int v
end
