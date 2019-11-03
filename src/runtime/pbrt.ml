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

type payload_kind =
  | Varint
  | Bits32
  | Bits64
  | Bytes

let min_int_as_int32, max_int_as_int32 =
  Int32.of_int min_int, Int32.of_int max_int

let min_int_as_int64, max_int_as_int64 =
  Int64.of_int min_int, Int64.of_int max_int

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
      | Failure e -> Some (
        Printf.sprintf "Pbrt.Decoder.Failure(%s)" (error_to_string e)
      )
      | _ -> None)

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

  let malformed_variant variant_name =
    raise (Failure (Malformed_variant variant_name))

  let unexpected_payload field_name pk =
    raise (Failure (Unexpected_payload (field_name, pk)))

  let missing_field field_name =
    raise (Failure (Missing_field field_name))

  let at_end d =
    d.limit = d.offset

  let byte d =
    if d.offset >= d.limit then
      raise (Failure Incomplete);
    let byte = int_of_char (Bytes.get d.source d.offset) in
    d.offset <- d.offset + 1;
    byte

  let bool_of_int64 fld v =
    if v = Int64.zero then false
    else if v = Int64.one then true
    else raise (Failure (Overflow fld))

  let int_of_int32 fld v =
    if Sys.word_size = 32 &&
      (v < min_int_as_int32 || v > max_int_as_int32)
    then
      raise (Failure (Overflow fld))
    else
      Int32.to_int v

  let int_of_int64 fld v =
    if (v < min_int_as_int64 || v > max_int_as_int64)
    then
      raise (Failure (Overflow fld))
    else
      Int64.to_int v

  (* TODO: could have a variant of this function which works with int type *)
  let varint d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int64.(logor (shift_left (logand (of_int b) 0x7fL) s) (read (s + 7)))
      else if s < 63 || (b land 0x7f) <= 1 then
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
      (* keys are always in the range of int,
       * but prefix might only fit into int32 *)
      let prefix  = varint d in
      let key, ty =
        Int64.(to_int (shift_right prefix 3)),
        Int64.logand 0x7L prefix in
      match ty with
      | 0L -> Some (key, Varint)
      | 1L -> Some (key, Bits64)
      | 2L -> Some (key, Bytes)
      | 5L -> Some (key, Bits32)
      | _  -> raise (Failure Malformed_field)

  let skip d kind =
    let skip_len n =
      if d.offset + n > d.limit
      then
        raise (Failure Incomplete)
      else
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

  let map_entry d ~decode_key ~decode_value =
    let d = nested d in

    let key_v = ref None in
    let value_v = ref None in

    let rec loop () =
      match key d with
      | None -> ()
      | Some (1, _)  -> key_v := Some (decode_key d); loop ()
      | Some (2, _)  -> value_v := Some (decode_value d); loop ()
      | Some (_, pk) -> (
        skip d pk;
        loop ()
      )
    in
    loop ();
    match !key_v, !value_v with
    | Some key, Some value -> (key, value)
    | _ -> failwith "Missing key or value for map entry"

  let empty_nested d =
    let len = varint d in
    if len <> 0L
    then raise (Failure Incomplete)
    else ()

  let packed_fold f e0 d =
    let d' = nested d in
    let rec loop acc =
      if at_end d'
      then acc
      else loop (f acc d')
    in
    loop e0

  let int_as_varint d =
    Int64.to_int @@ varint d

  let int_as_zigzag d =
    Int64.to_int @@ zigzag d

  let int32_as_varint d =
    Int64.to_int32 (varint d)

  let int32_as_zigzag d =
    Int64.to_int32 (zigzag d)

  let int64_as_varint = varint

  let int64_as_zigzag  = zigzag

  let int32_as_bits32  = bits32

  let int64_as_bits64  = bits64

  let bool d =
    bool_of_int64 "" (varint d)

  let float_as_bits32 d =
    Int32.float_of_bits (bits32 d)

  let float_as_bits64 d =
    Int64.float_of_bits (bits64 d)

  let int_as_bits32 d =
    int_of_int32 "" (bits32 d)

  let int_as_bits64 d =
    int_of_int64 "" (bits64 d)

  let string d =
    (* strings are always shorter than range of int *)
    let len = Int64.to_int (varint d) in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let str = Bytes.sub_string d.source d.offset len in
    d.offset <- d.offset + len;
    str

  let wrapper_double_value d =
    let d = nested d in
    match key d with
    | Some (1, Bits64) -> Some (float_as_bits64 d)
    | _ -> None

  let wrapper_float_value d =
    let d = nested d in
    match key d with
    | Some (1, Bits32) -> Some (float_as_bits32 d)
    | _ -> None

  let wrapper_int64_value d =
    let d = nested d in
    match key d with
    | Some (1, Varint) -> Some (int64_as_varint d)
    | _ -> None

  let wrapper_int32_value d =
    let d = nested d in
    match key d with
    | Some (1, Varint) -> Some (int32_as_varint d)
    | _ -> None

  let wrapper_bool_value d =
    let d = nested d in
    match key d with
    | Some (1, Varint) -> Some (bool d)
    | _ -> None

  let wrapper_string_value d =
    let d = nested d in
    match key d with
    | Some (1, Bytes) -> Some (string d)
    | _ -> None

  let wrapper_bytes_value d =
    let d = nested d in
    match key d with
    | Some (1, Bytes) -> Some (bytes d)
    | _ -> None
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
      | Failure e -> Some (
        Printf.sprintf "Protobuf.Encoder.Failure(%s)" (error_to_string e)
      )
      | _ -> None)

  type t = Buffer.t

  let create () =
    Buffer.create 16

  let to_string = Buffer.contents

  let to_bytes = Buffer.to_bytes

  let varint i e =
    let rec write i =
      if Int64.(logand i (lognot 0x7fL)) = Int64.zero
      then
        Buffer.add_char e (char_of_int Int64.(to_int (logand 0x7fL i)))
      else begin
        Buffer.add_char e (char_of_int Int64.(
            to_int (logor 0x80L (logand 0x7fL i))
        ));
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
    Buffer.add_char e (char_of_int Int32.(to_int (
      logand 0xffl (shift_right i 8))));
    Buffer.add_char e (char_of_int Int32.(to_int (
      logand 0xffl (shift_right i 16))));
    Buffer.add_char e (char_of_int Int32.(to_int (
      logand 0xffl (shift_right i 24))))

  let bits64 i e =
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL i)));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 8))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 16))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 24))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 32))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 40))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 48))));
    Buffer.add_char e (char_of_int Int64.(to_int (
      logand 0xffL (shift_right i 56))))

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

  let map_entry ~encode_key ~encode_value kv t =
    let (
      (key_value, key_pk),
      (value_value, value_pk)) = kv
    in

    nested (fun t ->
      key (1, key_pk) t;
      encode_key key_value t;
      key (2, value_pk) t;
      encode_value value_value t;
    ) t

  let empty_nested e = varint 0L e

  let int_as_varint i e = varint (Int64.of_int i) e

  let int_as_zigzag i e = zigzag (Int64.of_int i) e

  let int32_as_varint i e = varint (Int64.of_int32 i) e

  let int32_as_zigzag i e = zigzag (Int64.of_int32 i) e

  let int64_as_varint = varint

  let int64_as_zigzag = zigzag

  let int32_as_bits32 = bits32

  let int64_as_bits64 = bits64

  let bool b e = varint (if b then 1L else 0L) e

  let float_as_bits32 f e = bits32 (Int32.bits_of_float f) e

  let float_as_bits64 f e = bits64 (Int64.bits_of_float f) e

  let int_as_bits32 i e = bits32 (Int32.of_int i) e

  let int_as_bits64 i e = bits64 (Int64.of_int i) e

  let string s e = bytes (Bytes.of_string s) e

  let double_value_key = (1, Bits64)


  let wrapper_double_value v e =
    nested (fun e ->
      key double_value_key e;
      begin match v with
      | None -> ()
      | Some f -> float_as_bits64 f e
      end
    ) e

  let float_value_key = (1, Bits32)

  let wrapper_float_value v e =
    nested (fun e ->
      key float_value_key e;
      begin match v with
      | None -> ()
      | Some f -> float_as_bits32 f e
      end
    ) e

  let int64_value_key = (1, Varint)

  let wrapper_int64_value v e =
    nested (fun e ->
      key int64_value_key e;
      begin match v with
      | None -> ()
      | Some i -> int64_as_varint i e
      end
    ) e

  let int32_value_key = (1, Varint)

  let wrapper_int32_value v e =
    nested (fun e ->
      key int32_value_key e;
      begin match v with
      | None -> ()
      | Some i -> int32_as_varint i e
      end
    ) e

  let bool_value_key = (1, Varint)

  let wrapper_bool_value v e =
    nested (fun e ->
      key bool_value_key e;
      begin match v with
      | None -> ()
      | Some b -> bool b e
      end
    ) e

  let string_value_key = (1, Bytes)

  let wrapper_string_value v e =
    nested (fun e ->
      key string_value_key e;
      begin match v with
      | None -> ()
      | Some s -> string s e
      end
    ) e

  let bytes_value_key = (1, Bytes)

  let wrapper_bytes_value v e =
    nested (fun e ->
      key bytes_value_key e;
      begin match v with
      | None -> ()
      | Some b -> bytes b e
      end
    ) e

end

module Repeated_field = struct

  (** [t] is a container optimized for fast repeated inserts.

      It is made of a list of growing size array [l] as well as
      a current array [a] in which inserts are performed until
      [a] is full and appended to [l].

      The main growing logic is implemented in the [add] functions.
    *)
  type 'a t = {
    mutable s : int;           (* total size (allocated) of the partial array [a] *)
    mutable i : int;           (* current number of inserted element in [a] *)
    mutable a : 'a array;      (* partial array *)
    mutable l : 'a array list; (* previously filled array [List.hd l] is the last filled array *)
  }

  let make v = {
    s = 16;
    i = 0;
    a = Array.make 16 v;
    l = [];
  }

  let of_array_no_copy a = {
    (* We intentionally don't put [a] argument in [l]
       directly since it would require the allocation of a new
       array and an initial value. Since [Array.length a] could be [0]
       we would not be able to get such a value from the [a] argument.

       Hence the transfer of [a] to [l] will be done in the subsequent
       [add v t] call in which [v] argument is used to initialize the new array.
     *)
    s = Array.length a;
    i = Array.length a;
    a = a;
    l = [];
  }

  let add v ({s; i; a; l} as tmp) =
    match i with
    | i when i = s -> (
      (* [1.3] is an emperical growth factor found to be
         a good balance for allocation of a new
         array.
       *)
      tmp.s <- int_of_float (float_of_int s *.  1.3);
      tmp.i <- 1;
      tmp.l <- a :: l;
      tmp.a <- Array.make tmp.s v;
    )
    | i -> (
      Array.unsafe_set a i v;
      tmp.i  <- i + 1;
    )

  let to_array {s; i; a; l} =
    let l = match i with
      | 0 -> l
      | i when i = s -> a :: l
      | i -> (Array.sub a 0 i) :: l
    in
    Array.concat (List.rev l)

  (** [list_rev_iter f l] iterate over the list in reverse order *)
  let rec list_rev_iter f = function
    | [] -> ()
    | hd::tl -> (
      list_rev_iter f tl;
      f hd
    )

  let iter f {i; a; l; _} =
    list_rev_iter (fun a ->
      let len = Array.length a - 1 in
      for j = 0 to len do
        f (Array.unsafe_get a j)
      done
    ) l;
    let len = i - 1 in
    for j = 0 to len do
      f (Array.unsafe_get a j)
    done

  let iteri f {i; a; l; _} =
    let counter = ref 0 in
    list_rev_iter (fun a ->
      let len = Array.length a - 1 in
      for j = 0 to len do
        f !counter (Array.unsafe_get a j);
        incr counter;
      done
    ) l;
    let len = i - 1 in
    for j = 0 to len do
      f !counter (Array.unsafe_get a j);
      incr counter;
    done

  let fold_left f e0 t =
    let acc = ref e0 in
    iter (fun e ->
       acc := f !acc e
    ) t;
    !acc

  let length {s = _ ; i; a=_; l } : int=
    let len = List.fold_left (fun len a ->
      len + (Array.length a)
    ) 0 l in
    len + i

  let map_to_array f t =
    let len = length t in
    let dest = Array.make len (f @@ Array.unsafe_get t.a 0) in
    let index = ref 0 in

    iter (fun e ->
      Array.unsafe_set dest !index (f e);
      incr index
    ) t;
    dest

  let map_to_list f ({s = _ ; i; a; l}) =

    let rec a_to_list a i res =
      if i < 0
      then res
      else a_to_list a (i - 1) (f (Array.unsafe_get a i) :: res)
    in

    (* start with last (partial) array and its last index *)
    let res = a_to_list a (i - 1) [] in

    (* go over the filled array *)
    List.fold_left (fun acc a ->
      a_to_list a (Array.length a - 1) acc
    ) res l

  external identity : 'a -> 'a = "%identity"

  let to_list t = map_to_list identity t

end (* Repeated_field*)


module Pp = struct
  module F = Format

  type formatter = F.formatter

  let pp_unit fmt () =
    F.pp_print_string fmt "()"

  let pp_int =
    F.pp_print_int

  let pp_float =
    F.pp_print_float

  let pp_bool =
    F.pp_print_bool

  let pp_int32 fmt i =
    F.pp_print_string fmt (Int32.to_string i)

  let pp_int64 fmt i =
    F.pp_print_string fmt (Int64.to_string i)

  let pp_string fmt s =
    F.fprintf fmt "\"%a\"" F.pp_print_string s

  let pp_bytes fmt b =
    pp_string fmt (Bytes.to_string b)

  let pp_option pp_f fmt = function
    | None   -> F.fprintf fmt "@[None@]"
    | Some x -> F.fprintf fmt "@[Some(%a)@]" pp_f x

  let pp_wrapper_float fmt v =
    pp_option pp_float fmt v

  let pp_wrapper_bool fmt v =
    pp_option pp_bool fmt v

  let pp_wrapper_int32 fmt v =
    pp_option pp_int32 fmt v

  let pp_wrapper_int64 fmt v =
    pp_option pp_int64 fmt v

  let pp_wrapper_string fmt v =
    pp_option pp_string fmt v

  let pp_wrapper_bytes fmt v =
    pp_option pp_bytes fmt v

  let pp_list pp_element fmt l =
    let rec pp_i fmt = function
      | [h]  -> Format.fprintf fmt "%a" pp_element h
      | h::t ->
        Format.fprintf fmt "%a;@,%a" pp_element h pp_i t
      | []   -> ()
    in
    F.fprintf fmt "@[<v 1>[%a@,@]]" pp_i l

  let pp_associative_list pp_key pp_value fmt l =
    let pp_element fmt (k, v) =
      F.fprintf fmt "(%a, %a)" pp_key k pp_value v
    in
    pp_list pp_element fmt l

  let pp_hastable pp_key pp_value fmt h =
    let l = Hashtbl.fold (fun a b l ->
      (a, b)::l
    ) h [] in
    pp_associative_list pp_key pp_value fmt l

  let pp_record_field field_name pp_val fmt val_ =
    F.fprintf fmt "@,@[<h>%s = %a;@]" field_name pp_val val_

  let pp_brk pp_record (fmt:F.formatter) r : unit =
    F.fprintf fmt "@[<v>{%a@,@]}" pp_record r



end (* Pp *)
