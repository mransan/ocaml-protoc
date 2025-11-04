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

module Bitfield = struct
  type t = int

  let max_bits = Sys.int_size - 1
  let empty : t = 0
  let pp out (self : t) = Format.fprintf out "<bitfield 0x%x>" self

  let[@inline] set self idx : t =
    assert (idx < max_bits);
    self lor (1 lsl idx)

  let[@inline] get self idx : bool = self land (1 lsl idx) <> 0
end

module Decoder = struct
  type error =
    | Incomplete
    | Overlong_varint
    | Malformed_field
    | Overflow of string
    | Unexpected_payload of string * payload_kind
    | Missing_field of string
    | Malformed_variant of string

  let error_to_string e =
    match e with
    | Incomplete -> "Incomplete"
    | Overlong_varint -> "Overlong_varint"
    | Malformed_field -> "Malformed_field"
    | Overflow fld -> Printf.sprintf "Overflow(%S)" fld
    | Unexpected_payload (field, kind) ->
      let kind' =
        match kind with
        | Varint -> "Varint"
        | Bits32 -> "Bits32"
        | Bits64 -> "Bits64"
        | Bytes -> "Bytes"
      in
      Printf.sprintf "Unexpected_payload(%S, %s)" field kind'
    | Missing_field field -> Printf.sprintf "Missing_field(%S)" field
    | Malformed_variant name -> Printf.sprintf "Malformed_variant(%S)" name

  exception Failure of error

  let () =
    Printexc.register_printer (fun exn ->
        match exn with
        | Failure e ->
          Some (Printf.sprintf "Pbrt.Decoder.Failure(%s)" (error_to_string e))
        | _ -> None)

  type t = {
    source: bytes;
    limit: int;
    mutable offset: int;
  }

  let of_bytes source = { source; offset = 0; limit = Bytes.length source }

  let of_subbytes source offset len =
    if offset + len > Bytes.length source then
      invalid_arg "Pbrt.Decoder.of_subbypes";
    { source; offset; limit = offset + len }

  let of_string source =
    (* safe: we won't modify the bytes *)
    of_bytes (Bytes.unsafe_of_string source)

  let of_substring source offset len =
    of_subbytes (Bytes.unsafe_of_string source) offset len

  let malformed_variant variant_name =
    raise (Failure (Malformed_variant variant_name))

  let unexpected_payload field_name pk =
    raise (Failure (Unexpected_payload (field_name, pk)))

  let[@inline never] missing_field field_name =
    raise (Failure (Missing_field field_name))

  let[@inline never] incomplete () = raise (Failure Incomplete)
  let at_end d = d.limit = d.offset

  let[@inline] byte d =
    if d.offset >= d.limit then incomplete ();
    let byte = int_of_char (Bytes.get d.source d.offset) in
    d.offset <- d.offset + 1;
    byte

  let[@inline] bool_of_int64 fld v =
    if v = Int64.zero then
      false
    else if v = Int64.one then
      true
    else
      raise (Failure (Overflow fld))

  let int_of_int32 fld v =
    if Sys.word_size = 32 && (v < min_int_as_int32 || v > max_int_as_int32) then
      raise (Failure (Overflow fld))
    else
      Int32.to_int v

  let[@inline] int_of_int64 fld v =
    if v < min_int_as_int64 || v > max_int_as_int64 then
      raise (Failure (Overflow fld))
    else
      Int64.to_int v

  let varint d : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = byte d in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        raise (Failure Overlong_varint)
    done;
    !res

  let zigzag d : int64 =
    let v = (varint [@inlined]) d in
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))

  let[@inline] bits32 d =
    if d.offset + 4 > d.limit then incomplete ();
    let x = Bytes.get_int32_le d.source d.offset in
    d.offset <- d.offset + 4;
    x

  let[@inline] bits64 d =
    if d.offset + 8 > d.limit then incomplete ();
    let x = Bytes.get_int64_le d.source d.offset in
    d.offset <- d.offset + 8;
    x

  let int_as_varint d = Int64.to_int @@ (varint [@inlined]) d

  let bytes d =
    (* strings are always shorter than range of int *)
    let len = int_as_varint d in
    if d.offset + len > d.limit then raise (Failure Incomplete);
    let str = Bytes.sub d.source d.offset len in
    d.offset <- d.offset + len;
    str

  let nested d =
    (* strings are always shorter than range of int *)
    let len = int_as_varint d in
    if d.offset + len > d.limit then raise (Failure Incomplete);
    let d' = { d with limit = d.offset + len } in
    d.offset <- d.offset + len;
    d'

  let key d =
    if d.offset = d.limit then
      None
    else (
      (* keys are always in the range of int,
       * but prefix might only fit into int32 *)
      let prefix = (varint [@inlined]) d in
      let key, ty =
        Int64.(to_int (shift_right prefix 3)), Int64.logand 0x7L prefix
      in
      match ty with
      | 0L -> Some (key, Varint)
      | 1L -> Some (key, Bits64)
      | 2L -> Some (key, Bytes)
      | 5L -> Some (key, Bits32)
      | _ -> raise (Failure Malformed_field)
    )

  let skip d kind =
    let skip_len n =
      if d.offset + n > d.limit then
        raise (Failure Incomplete)
      else
        d.offset <- d.offset + n
    in
    let rec skip_varint () =
      let b = byte d in
      if b land 0x80 <> 0 then
        skip_varint ()
      else
        ()
    in
    match kind with
    | Bits32 -> skip_len 4
    | Bits64 -> skip_len 8
    (* strings are always shorter than range of int *)
    | Bytes -> skip_len (int_as_varint d)
    | Varint -> skip_varint ()

  let map_entry d ~decode_key ~decode_value =
    let d = nested d in

    let key_v = ref None in
    let value_v = ref None in

    let rec loop () =
      match key d with
      | None -> ()
      | Some (1, _) ->
        key_v := Some (decode_key d);
        loop ()
      | Some (2, _) ->
        value_v := Some (decode_value d);
        loop ()
      | Some (_, pk) ->
        skip d pk;
        loop ()
    in
    loop ();
    match !key_v, !value_v with
    | Some key, Some value -> key, value
    | _ -> failwith "Missing key or value for map entry"

  let empty_nested d =
    let len = int_as_varint d in
    if len <> 0 then
      raise (Failure Incomplete)
    else
      ()

  let packed_fold f e0 d =
    let d' = nested d in
    let rec loop acc =
      if at_end d' then
        acc
      else
        loop (f acc d')
    in
    loop e0

  let[@inline] int_as_zigzag d = Int64.to_int @@ (zigzag [@inlined]) d
  let[@inline] int32_as_varint d = Int64.to_int32 ((varint [@inlined]) d)
  let[@inline] int32_as_zigzag d = Int64.to_int32 ((zigzag [@inlined]) d)
  let int64_as_varint = varint
  let int64_as_zigzag = zigzag
  let int32_as_bits32 = bits32
  let int64_as_bits64 = bits64
  let[@inline] uint32_as_varint d = `unsigned (int32_as_varint d)
  let[@inline] uint32_as_zigzag d = `unsigned (int32_as_zigzag d)
  let[@inline] uint64_as_varint d = `unsigned (varint d)
  let[@inline] uint64_as_zigzag d = `unsigned (zigzag d)
  let[@inline] uint32_as_bits32 d = `unsigned (bits32 d)
  let[@inline] uint64_as_bits64 d = `unsigned (bits64 d)
  let[@inline] bool d = bool_of_int64 "" ((varint [@inlined]) d)
  let[@inline] float_as_bits32 d = Int32.float_of_bits (bits32 d)
  let[@inline] float_as_bits64 d = Int64.float_of_bits (bits64 d)
  let[@inline] int_as_bits32 d = int_of_int32 "" (bits32 d)
  let[@inline] int_as_bits64 d = int_of_int64 "" (bits64 d)

  let string d =
    (* strings are always shorter than range of int *)
    let len = int_as_varint d in
    if d.offset + len > d.limit then raise (Failure Incomplete);
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
  type error = Overflow of string

  let error_to_string e =
    match e with
    | Overflow fld -> Printf.sprintf "Overflow(%S)" fld

  exception Failure of error

  let () =
    Printexc.register_printer (fun exn ->
        match exn with
        | Failure e ->
          Some
            (Printf.sprintf "Protobuf.Encoder.Failure(%s)" (error_to_string e))
        | _ -> None)

  type t = {
    mutable b: bytes;  (** Slice of bytes (already written: [start…]) *)
    mutable start: int;
        (** Start of the slice in which data have been written *)
    initial: bytes;  (** Initial buffer, for {!reset} *)
  }

  let create ?(size = 16) () =
    let len = max size 16 in
    let b = Bytes.create len in
    { b; start = len; initial = b }

  let[@inline] cap self = Bytes.length self.b
  let[@inline] clear self = self.start <- cap self

  let reset self =
    self.b <- self.initial;
    self.start <- cap self

  let[@inline] to_string self =
    Bytes.sub_string self.b self.start (cap self - self.start)

  let[@inline] to_bytes self =
    Bytes.sub self.b self.start (cap self - self.start)

  let[@inline] write_chunks w self : unit =
    w self.b self.start (cap self - self.start)

  let next_cap_ self =
    min Sys.max_string_length
      (let n = cap self in
       n + (n lsr 1))

  let[@inline never] grow_to_ self newcap =
    if newcap = cap self then
      raise (Failure (Overflow "encoder size reached its max"));
    let b' = Bytes.create newcap in
    let len = cap self - self.start in
    Bytes.blit self.b self.start b' (newcap - len) len;
    self.start <- newcap - len;
    self.b <- b'

  (** Grow to next size *)
  let[@inline never] grow_ self = grow_to_ self (next_cap_ self)

  (** Grow to get [n] free bytes *)
  let[@inline never] grow_reserve_n (self : t) n : unit =
    let newcap = max (cap self + n) (next_cap_ self) in
    grow_to_ self newcap;
    assert (self.start >= n)

  let[@inline] add_char self c =
    if self.start = 0 then grow_ self;
    self.start <- self.start - 1;
    Bytes.unsafe_set self.b self.start c

  (** Reserve [n] bytes, return the start offset of the newly allocated slice *)
  let[@inline] reserve_n (self : t) (n : int) : int =
    if self.start < n then grow_reserve_n self n;
    self.start <- self.start - n;
    self.start

  let add_bytes self b =
    let n = Bytes.length b in
    let start = reserve_n self n in
    Bytes.blit b 0 self.b start n

  external varint_size : (int64[@unboxed]) -> int
    = "caml_pbrt_varint_size_byte" "caml_pbrt_varint_size"
  [@@noalloc]
  (** Compute how many bytes this int would occupy as varint *)

  external varint_slice : bytes -> (int[@untagged]) -> (int64[@unboxed]) -> unit
    = "caml_pbrt_varint_byte" "caml_pbrt_varint"
  [@@noalloc]
  (** Write this int as varint into the given slice *)

  let[@inline] varint64 (i : int64) e =
    let n_bytes = varint_size i in
    let start = reserve_n e n_bytes in
    varint_slice e.b start i

  let int_as_varint i e =
    let i = Int64.of_int i in
    let n_bytes = varint_size i in
    let start = reserve_n e n_bytes in
    varint_slice e.b start i

  let zigzag i e =
    let i = Int64.of_int i in
    let i = Int64.(logxor (shift_left i 1) (shift_right i 63)) in
    let n_bytes = varint_size i in
    let start = reserve_n e n_bytes in
    varint_slice e.b start i

  let[@inline] zigzag64 i e =
    (varint64 [@inlined]) Int64.(logxor (shift_left i 1) (shift_right i 63)) e

  let[@inline] bits32 i e =
    let start = reserve_n e 4 in
    Bytes.set_int32_le e.b start i

  let[@inline] bits64 i e =
    let start = reserve_n e 8 in
    Bytes.set_int64_le e.b start i

  let bytes b e =
    add_bytes e b;
    int_as_varint (Bytes.length b) e

  let[@inline] nested f x e =
    (* compute length because it's not affected by a resize during
       the call to [f] *)
    let old_len = cap e - e.start in
    f x e;
    let new_len = cap e - e.start in
    let size = new_len - old_len in
    int_as_varint size e

  let[@inline] key k pk e =
    let pk' =
      match pk with
      | Varint -> 0
      | Bits64 -> 1
      | Bytes -> 2
      | Bits32 -> 5
    in
    int_as_varint (pk' lor (k lsl 3)) e

  let map_entry ~encode_key ~encode_value kv t =
    nested
      (fun kv t ->
        let (key_value, key_pk), (value_value, value_pk) = kv in
        encode_value value_value t;
        key 2 value_pk t;
        encode_key key_value t;
        key 1 key_pk t)
      kv t

  let empty_nested e = add_char e (Char.unsafe_chr 0)
  let int_as_zigzag = zigzag
  let int32_as_varint i e = (varint64 [@inlined]) (Int64.of_int32 i) e
  let int32_as_zigzag i e = (zigzag64 [@inlined]) (Int64.of_int32 i) e
  let int64_as_varint = varint64
  let int64_as_zigzag = zigzag64
  let int32_as_bits32 = bits32
  let int64_as_bits64 = bits64

  let uint32_as_varint = function
    | `unsigned d -> int32_as_varint d

  let uint32_as_zigzag = function
    | `unsigned d -> int32_as_zigzag d

  let uint64_as_varint = function
    | `unsigned d -> varint64 d

  let uint64_as_zigzag = function
    | `unsigned d -> zigzag64 d

  let uint32_as_bits32 = function
    | `unsigned x -> bits32 x

  let uint64_as_bits64 = function
    | `unsigned x -> bits64 x

  let[@inline] bool b e =
    add_char e
      (Char.unsafe_chr
         (if b then
            1
          else
            0))

  let[@inline] float_as_bits32 f e = bits32 (Int32.bits_of_float f) e
  let[@inline] float_as_bits64 f e = bits64 (Int64.bits_of_float f) e
  let[@inline] int_as_bits32 i e = bits32 (Int32.of_int i) e
  let[@inline] int_as_bits64 i e = bits64 (Int64.of_int i) e

  let[@inline] string s e =
    (* safe: we're not going to modify the bytes, and [s] will
       not change. *)
    bytes (Bytes.unsafe_of_string s) e

  let wrapper_double_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some f -> float_as_bits64 f e);
        key 1 Bits64 e)
      v e

  let wrapper_float_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some f -> float_as_bits32 f e);
        key 1 Bits32 e)
      v e

  let wrapper_int64_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some i -> int64_as_varint i e);
        key 1 Varint e)
      v e

  let wrapper_int32_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some i -> int32_as_varint i e);
        key 1 Varint e)
      v e

  let wrapper_bool_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some b -> bool b e);
        key 1 Varint e)
      v e

  let wrapper_string_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some s -> string s e);
        key 1 Bytes e)
      v e

  let wrapper_bytes_value v e =
    nested
      (fun v e ->
        (match v with
        | None -> ()
        | Some b -> bytes b e);
        key 1 Bytes e)
      v e
end

module List_util = struct
  let rev_iter_with f l st =
    let rec iter_ f l st =
      match l with
      | [] -> ()
      | x :: tl ->
        f x st;
        iter_ f tl st
    in
    let rec direct i f l st =
      match l with
      | [] -> ()
      | [ x ] -> f x st
      | [ x; y ] ->
        f y st;
        f x st
      | _ when i = 0 -> iter_ f (List.rev l) st
      | x :: y :: tl ->
        direct (i - 1) f tl st;
        f y st;
        f x st
    in

    match l with
    | [] -> ()
    | [ x ] -> f x st
    | [ x; y ] ->
      f y st;
      f x st
    | x :: y :: tl ->
      direct 200 f tl st;
      f y st;
      f x st
end

module Repeated_field = struct
  type 'a t = {
    mutable s: int; (* total size (allocated) of the partial array [a] *)
    mutable i: int; (* current number of inserted element in [a] *)
    mutable a: 'a array; (* partial array *)
    mutable l: 'a array list;
        (* previously filled array [List.hd l] is the last filled array *)
  }
  (** [t] is a container optimized for fast repeated inserts.

      It is made of a list of growing size array [l] as well as a current array
      [a] in which inserts are performed until [a] is full and appended to [l].

      The main growing logic is implemented in the [add] functions. *)

  let make v = { s = 16; i = 0; a = Array.make 16 v; l = [] }

  let of_array_no_copy a =
    {
      (* We intentionally don't put [a] argument in [l]
         directly since it would require the allocation of a new
         array and an initial value. Since [Array.length a] could be [0]
         we would not be able to get such a value from the [a] argument.

         Hence the transfer of [a] to [l] will be done in the subsequent
         [add v t] call in which [v] argument is used to initialize the new array.
      *)
      s = Array.length a;
      i = Array.length a;
      a;
      l = [];
    }

  let add v ({ s; i; a; l } as tmp) =
    match i with
    | i when i = s ->
      (* [1.3] is an emperical growth factor found to be
         a good balance for allocation of a new
         array.
      *)
      tmp.s <- int_of_float (float_of_int s *. 1.3);
      tmp.i <- 1;
      tmp.l <- a :: l;
      tmp.a <- Array.make tmp.s v
    | i ->
      Array.unsafe_set a i v;
      tmp.i <- i + 1

  let to_array { s; i; a; l } =
    let l =
      match i with
      | 0 -> l
      | i when i = s -> a :: l
      | i -> Array.sub a 0 i :: l
    in
    Array.concat (List.rev l)

  let iter f { i; a; l; _ } =
    List_util.rev_iter_with
      (fun a f ->
        let len = Array.length a - 1 in
        for j = 0 to len do
          f (Array.unsafe_get a j)
        done)
      l f;
    let len = i - 1 in
    for j = 0 to len do
      f (Array.unsafe_get a j)
    done

  let rec list_iter_with_ f l st =
    match l with
    | [] -> ()
    | x :: tl ->
      f x st;
      list_iter_with_ f tl st

  let rev_iter_with f (self : _ t) st =
    let len = self.i - 1 in
    for j = len downto 0 do
      f (Array.unsafe_get self.a j) st
    done;
    list_iter_with_
      (fun a st ->
        let len = Array.length a - 1 in
        for j = len downto 0 do
          f (Array.unsafe_get a j) st
        done)
      self.l st

  let iteri f { i; a; l; _ } =
    let counter = ref 0 in
    List_util.rev_iter_with
      (fun a f ->
        let len = Array.length a - 1 in
        for j = 0 to len do
          f !counter (Array.unsafe_get a j);
          incr counter
        done)
      l f;
    let len = i - 1 in
    for j = 0 to len do
      f !counter (Array.unsafe_get a j);
      incr counter
    done

  let fold_left f e0 t =
    let acc = ref e0 in
    iter (fun e -> acc := f !acc e) t;
    !acc

  let length { s = _; i; a = _; l } : int =
    let len = List.fold_left (fun len a -> len + Array.length a) 0 l in
    len + i

  let map_to_array f t =
    let len = length t in
    let dest = Array.make len (f @@ Array.unsafe_get t.a 0) in
    let index = ref 0 in

    iter
      (fun e ->
        Array.unsafe_set dest !index (f e);
        incr index)
      t;
    dest

  let map_to_list f { s = _; i; a; l } =
    let rec a_to_list a i res =
      if i < 0 then
        res
      else
        a_to_list a (i - 1) (f (Array.unsafe_get a i) :: res)
    in

    (* start with last (partial) array and its last index *)
    let res = a_to_list a (i - 1) [] in

    (* go over the filled array *)
    List.fold_left (fun acc a -> a_to_list a (Array.length a - 1) acc) res l

  external identity : 'a -> 'a = "%identity"

  let to_list t = map_to_list identity t
end

module Pp = struct
  module F = Format

  type formatter = F.formatter

  let pp_unit fmt () = F.pp_print_string fmt "()"
  let pp_int = F.pp_print_int
  let pp_float = F.pp_print_float
  let pp_bool = F.pp_print_bool
  let pp_int32 fmt i = F.pp_print_string fmt (Int32.to_string i)

  let pp_unsigned_of_int32 fmt = function
    | `unsigned i -> F.fprintf fmt "%lu" i

  let pp_int64 fmt i = F.pp_print_string fmt (Int64.to_string i)

  let pp_unsigned_of_int64 fmt = function
    | `unsigned i -> F.fprintf fmt "%Lu" i

  let pp_string fmt s = F.fprintf fmt "\"%a\"" F.pp_print_string s
  let pp_bytes fmt b = F.fprintf fmt "<bytes len=%d>" (Bytes.length b)

  let pp_option pp_f fmt = function
    | None -> F.fprintf fmt "@[None@]"
    | Some x -> F.fprintf fmt "@[<hv2>Some(@,%a)@]" pp_f x

  let pp_wrapper_float fmt v = pp_option pp_float fmt v
  let pp_wrapper_bool fmt v = pp_option pp_bool fmt v
  let pp_wrapper_int32 fmt v = pp_option pp_int32 fmt v
  let pp_wrapper_int64 fmt v = pp_option pp_int64 fmt v
  let pp_wrapper_string fmt v = pp_option pp_string fmt v
  let pp_wrapper_bytes fmt v = pp_option pp_bytes fmt v

  let pp_list pp_element fmt l =
    let rec pp_i fmt = function
      | [ h ] -> Format.fprintf fmt "%a" pp_element h
      | h :: t -> Format.fprintf fmt "%a;@,%a" pp_element h pp_i t
      | [] -> ()
    in
    F.fprintf fmt "[@[<hv>%a@,@]]" pp_i l

  let pp_associative_list pp_key pp_value fmt l =
    let pp_element fmt (k, v) =
      F.fprintf fmt "(@[%a,@ %a@])" pp_key k pp_value v
    in
    pp_list pp_element fmt l

  let pp_hastable pp_key pp_value fmt h =
    let l = Hashtbl.fold (fun a b l -> (a, b) :: l) h [] in
    pp_associative_list pp_key pp_value fmt l

  let pp_record_field ?(first = false) field_name pp_val fmt val_ =
    if not first then F.fprintf fmt "@ ";
    F.fprintf fmt "@[<hv2>%s =@ %a;@]" field_name pp_val val_

  let pp_brk pp_record (fmt : F.formatter) r : unit =
    F.fprintf fmt "@[<hv2>{ %a@;<1 -2>@]}" pp_record r
end
