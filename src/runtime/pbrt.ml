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

type payload_kind = Protobuf.payload_kind = 
  | Varint
  | Bits32
  | Bits64
  | Bytes 

module Decoder = struct 

  type t = Protobuf.Decoder.t 

  let of_bytes = Protobuf.Decoder.of_bytes 
  
  let key = Protobuf.Decoder.key
  
  let skip = Protobuf.Decoder.skip 

  let nested = Protobuf.Decoder.nested 

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
    let len = Protobuf.Decoder.varint d in
    if len <> 0L
    then raise (Protobuf.Decoder.Failure Protobuf.Decoder.Incomplete) 
    else () 

  let packed_fold f e0 d =
    let d' = nested d in 
    let rec loop acc = 
      if Protobuf.Decoder.at_end d'
      then acc 
      else loop (f acc d')
    in 
    loop e0 

  let int_as_varint d = 
    Int64.to_int @@ Protobuf.Decoder.varint d
    
  let int_as_zigzag d = 
    Int64.to_int @@ Protobuf.Decoder.zigzag d 

  let int32_as_varint d = 
    Int64.to_int32 (Protobuf.Decoder.varint d) 

  let int32_as_zigzag d =
    Int64.to_int32 (Protobuf.Decoder.zigzag d)

  let int64_as_varint = Protobuf.Decoder.varint 

  let int64_as_zigzag  = Protobuf.Decoder.zigzag 

  let int32_as_bits32  = Protobuf.Decoder.bits32

  let int64_as_bits64  = Protobuf.Decoder.bits64

  let bool d = 
    Protobuf.Decoder.bool_of_int64 "" (Protobuf.Decoder.varint d)

  let float_as_bits32 d = 
    Int32.float_of_bits (Protobuf.Decoder.bits32 d)

  let float_as_bits64 d = 
    Int64.float_of_bits (Protobuf.Decoder.bits64 d) 

  let int_as_bits32 d = 
    Protobuf.Decoder.int_of_int32 "" (Protobuf.Decoder.bits32 d) 

  let int_as_bits64 d = 
    Protobuf.Decoder.int_of_int64 "" (Protobuf.Decoder.bits64 d) 

  let string d = Bytes.to_string (Protobuf.Decoder.bytes d) 

  let bytes  = Protobuf.Decoder.bytes

end 

module Encoder = struct 

  type t = Protobuf.Encoder.t 

  let create = Protobuf.Encoder.create 

  let to_bytes = Protobuf.Encoder.to_bytes  
  
  let key = Protobuf.Encoder.key

  let nested = Protobuf.Encoder.nested
  
  let map_entry ~encode_key ~encode_value ((key_value, key_pk), (value_value, value_pk)) t = 
    nested (fun t -> 
      key (1, key_pk) t; 
      encode_key key_value t;
      key (2, value_pk) t; 
      encode_value value_value t; 
    ) t 
  
  let empty_nested e = 
    Protobuf.Encoder.varint 0L e

  let int_as_varint i e = 
    Protobuf.Encoder.varint (Int64.of_int i) e 

  let int_as_zigzag i e = 
    Protobuf.Encoder.zigzag (Int64.of_int i) e 

  let int32_as_varint i e = 
    Protobuf.Encoder.varint (Int64.of_int32 i) e 

  let int32_as_zigzag i e = 
    Protobuf.Encoder.zigzag (Int64.of_int32 i) e 

  let int64_as_varint = Protobuf.Encoder.varint 

  let int64_as_zigzag = Protobuf.Encoder.zigzag 

  let int32_as_bits32 = Protobuf.Encoder.bits32

  let int64_as_bits64 = Protobuf.Encoder.bits64

  let bool b e = 
    Protobuf.Encoder.varint (if b then 1L else 0L) e

  let float_as_bits32 f e = 
    Protobuf.Encoder.bits32 (Int32.bits_of_float f) e 

  let float_as_bits64 f e =
    Protobuf.Encoder.bits64 (Int64.bits_of_float f) e 

  let int_as_bits32 i e = 
    Protobuf.Encoder.bits32 (Int32.of_int i) e 
  
  let int_as_bits64 i e =
    Protobuf.Encoder.bits64 (Int64.of_int i) e 

  let string s e = Protobuf.Encoder.bytes (Bytes.of_string s) e 

  let bytes = Protobuf.Encoder.bytes 
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
