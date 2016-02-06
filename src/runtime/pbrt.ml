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

  let at_end d =
    d.limit = d.offset

  let byte d =
    if d.offset >= d.limit then
      raise (Failure Incomplete);
    let byte = int_of_char (Bytes.get d.source d.offset) in
    d.offset <- d.offset + 1;
    byte
  
  let rec read_int s d =
    let b = byte d in
    if b land 0x80 <> 0 
    then ((b land 0x7f) lsl s) lor (read_int (s + 7) d)
    else if s < 56 || (b land 0x7f) <= 1 
    then b lsl s
    else raise (Failure Overlong_varint)

  let int_as_varint d =
    read_int 0 d

  let int64_as_varint d =
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
  
  let int32_as_varint d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int32.(logor (shift_left (logand (of_int b) 0x7fl) s) (read (s + 7)))
      else if s < 24 || (b land 0x7f) <= 1 then
        Int32.(shift_left (of_int b) s)
      else
        raise (Failure Overlong_varint)
    in
    read 0

  let int64_as_zigzag d =
    let v = int64_as_varint d in
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))
  
  let int_as_zigzag d =
    Int64.to_int (int64_as_zigzag d) 

  let int32_as_zigzag d =
    let v = int32_as_varint d in
    Int32.(logxor (shift_right v 1) (neg (logand v Int32.one)))

  let int32_as_bits32 d =
    let b1 = byte d in
    let b2 = byte d in
    let b3 = byte d in
    let b4 = byte d in
    Int32.(add (shift_left (of_int b4) 24)
           (add (shift_left (of_int b3) 16)
            (add (shift_left (of_int b2) 8)
             (of_int b1))))

  let int64_as_bits64 d =
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
    let len = int_as_varint d in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let str = Bytes.sub d.source d.offset len in
    d.offset <- d.offset + len;
    str

  let nested d =
    let len = int_as_varint d in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let d' = { d with limit = d.offset + len; } in
    d.offset <- d.offset + len;
    d'

  let empty_nested d =
    let len = int_as_varint d in
    if len <> 0 
    then raise (Failure Incomplete) 
    else () 
  
  let bool d = 
    if int_as_varint d = 0 then false else true

  let float_as_bits32 d = 
    Int32.float_of_bits @@ int32_as_bits32 d
  
  let float_as_bits64 d = 
    Int64.float_of_bits @@ int64_as_bits64 d
  
  let string d = 
    let len = int_as_varint d in
    if d.offset + len > d.limit then
      raise (Failure Incomplete);
    let str = Bytes.sub_string d.source d.offset len in
    d.offset <- d.offset + len;
    str
  
  let int_as_bits32 d = 
    Int32.to_int @@ int32_as_bits32 d 
    (* TODO this could be faster by implementing it directly *)
  
  let int_as_bits64 d = 
    Int64.to_int @@ int64_as_bits64 d 
    (* TODO this could be faster by implementing it directly *)
  
  let packed f d =
    let ({limit;_ } as d') = nested d in 
    let rec loop acc = 
      if d'.offset = limit  
      then acc 
      else 
        let acc = (f d')::acc in 
        loop acc 
    in 
    loop []
  
  let packed_fold f e0 d =
    let ({limit;_ } as d') = nested d in 
    let rec loop acc = 
      if d'.offset = limit  
      then acc 
      else loop (f acc d')
    in 
    loop e0 
  
  let key d =
    if d.offset = d.limit
    then None
    else
      (* keys are always in the range of int, but prefix might only fit into int32 *)
      let prefix  = int_as_varint d in
      let key, ty = (prefix lsr 3), 0x7 land prefix in
      match ty with
      | 0 -> Some (key, Varint)
      | 1 -> Some (key, Bits64)
      | 2 -> Some (key, Bytes)
      | 5 -> Some (key, Bits32)
      | _  -> raise (Failure Malformed_field)

  let skip_len n d =
    if d.offset + n > d.limit then
      raise (Failure Incomplete);
    d.offset <- d.offset + n
  
  let rec skip_varint d =
    let b = byte d in
    if b land 0x80 <> 0 then skip_varint d else ()
  
  let skip d kind =
    match kind with
    | Bits32 -> skip_len 4 d 
    | Bits64 -> skip_len 8 d
    | Bytes  -> skip_len (int_as_varint d) d
    | Varint -> skip_varint d 
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

  let int_as_varint i e =
    let rec write i =
      if (i land (lnot 0x7f)) = 0 then
        Buffer.add_char e (char_of_int (0x7f land i))
      else begin
        Buffer.add_char e (char_of_int (0x80 lor (0x7f land i)));
        write (i lsr 7)
      end
    in
    write i
  
  let int64_as_varint i e =
    let rec write i =
      if Int64.(logand i (lognot 0x7fL)) = Int64.zero then
        Buffer.add_char e (char_of_int Int64.(to_int (logand 0x7fL i)))
      else begin
        Buffer.add_char e (char_of_int Int64.(to_int (logor 0x80L (logand 0x7fL i))));
        write (Int64.shift_right_logical i 7)
      end
    in
    write i
  
  let int32_as_varint i e =
    let rec write i =
      if Int32.(logand i (lognot 0x7fl)) = Int32.zero then
        Buffer.add_char e (char_of_int Int32.(to_int (logand 0x7fl i)))
      else begin
        Buffer.add_char e (char_of_int Int32.(to_int (logor 0x80l (logand 0x7fl i))));
        write (Int32.shift_right_logical i 7)
      end
    in
    write i

  let smallint i e =
    int_as_varint i e
  
  let int64_as_zigzag i e =
    int64_as_varint Int64.(logxor (shift_left i 1) (shift_right i 63)) e
  
  let int_as_zigzag i e =
    int64_as_zigzag  (Int64.of_int i) e 
  
  let int32_as_zigzag i e =
    int32_as_varint Int32.(logxor (shift_left i 1) (shift_right i 31)) e

  let int32_as_bits32 i e =
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl i)));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Buffer.add_char e (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))

  let int64_as_bits64 i e =
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL i)));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 8))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 16))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 24))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 32))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 40))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 48))));
    Buffer.add_char e (char_of_int Int64.(to_int (logand 0xffL (shift_right i 56))))
  
  let int_as_bits32 i e =  
    (* TODO do safe mode *) 
    int32_as_bits32 (Int32.of_int i) e

  let int_as_bits64 i e = 
    int64_as_bits64 (Int64.of_int i) e 

  let float_as_bits32 v e = 
    int32_as_bits32 (Int32.bits_of_float v) e 
  
  let float_as_bits64 v e = 
    int64_as_bits64 (Int64.bits_of_float v) e 

  let bytes b e =
    smallint (Bytes.length b) e;
    Buffer.add_bytes e b
  
  let string s e =
    let b = Bytes.of_string s in 
    bytes b e 
    (* TODO this could be implemented natively *)

  let bool b e = 
    smallint (if b then 1 else 0) e 

  let nested f e =
    let e' = Buffer.create 16 in
    f e';
    smallint (Buffer.length e') e;
    Buffer.add_buffer e e'
  
  let packed l f e = 
    nested (fun e -> 
      List.iter (fun i -> 
        f i e
      ) l 
    ) e 

  let empty_nested e  = 
    smallint 0 e 

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

  let iter f {s; i; a; l} = 
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
  
  let iteri f {s; i; a; l} = 
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

  let map_to_array f ({s; i; a; l} as t) = 
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
      
end (* Array *) 
