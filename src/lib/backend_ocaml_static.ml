
let prefix_payload_to_ocaml_t  = {|

module P  = Printf
  
let add_indentation n s = 
  Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  

module Pc = Protobuf_codec 

let decode_varint_as_int decoder = 
  (*Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder*)
  Int64.to_int @@ Pc.Decoder.varint decoder

let decode_varint_zigzag_as_int decoder = 
  (* Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder *) 
  Int64.to_int @@ Pc.Decoder.zigzag decoder

let encode_int_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int v) encoder  

let encode_int_as_varint_zigzag v encoder = 
  Pc.Encoder.zigzag (Int64.of_int v) encoder  

let decode_bits32_as_int decoder = 
  (* Pc.Decoder.int_of_int32 "" @@ Pc.Decoder.bits32 decoder *) 
  Int32.to_int @@ Pc.Decoder.bits32 decoder

let encode_int_as_bits32 v encoder = 
  Pc.Encoder.bits32 (Pc.Encoder.int32_of_int "" v) encoder

let decode_bits64_as_int decoder = 
  (* Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.bits64 decoder *)
  Int64.to_int @@ Pc.Decoder.bits64 decoder 

let encode_int_as_bits64 v encoder = 
  Pc.Encoder.bits64 (Int64.of_int v) encoder

let decode_varint_as_bool decoder = 
  Pc.Decoder.bool_of_int64 "" @@ Pc.Decoder.varint decoder

let encode_bool_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int @@ if v  then 1 else 0) encoder 

let decode_bits32_as_float decoder = 
  Int32.float_of_bits @@ Pc.Decoder.bits32 decoder 

let encode_float_as_bits32 v encoder =
  Pc.Encoder.bits32 (Int32.bits_of_float v) encoder 
  
let decode_bits64_as_float decoder = 
  Int64.float_of_bits @@ Pc.Decoder.bits64 decoder 

let encode_float_as_bits64 v encoder =
  Pc.Encoder.bits64 (Int64.bits_of_float v) encoder

let decode_bytes_as_string decoder = 
  Bytes.to_string @@ Pc.Decoder.bytes decoder 

let encode_string_as_bytes v encoder = 
  Pc.Encoder.bytes (Bytes.of_string v) encoder 

let decode_bytes_as_bytes  = Pc.Decoder.bytes 

let encode_bytes_as_bytes  = Pc.Encoder.bytes

|}

let prefix_decode_f = {|

let decode decoder mappings values = 
  let insert number v = 
    let v' = Array.unsafe_get values number in 
    Array.unsafe_set values number (v::v')
  in 

  let continue = ref true in 
  while !continue do 
    match Pc.Decoder.key decoder with 
    | None -> continue := false
    | Some (number, payload_kind) -> (
      try 
        let mapping = List.assoc number mappings in 
        insert number (mapping decoder);
      with 
      | Not_found ->  (
        Pc.Decoder.skip decoder payload_kind; 
      )
    )
  done;
  values

let required number a f = 
  match Array.unsafe_get a number with 
  | []     -> failwith (P.sprintf "field %i missing" number)
  | hd::_  -> f hd
  (* TODO Improve *) 

let optional number a f = 
  match Array.unsafe_get a number with 
  | []     -> None
  | hd::_ -> Some (f hd)
  (* TODO Improve *) 

let list_ number a f = 
  List.rev_map f @@ Array.unsafe_get a number

external identity: 'a -> 'a = "%identity"

let oneof numbers a = 
  let ret = List.fold_left (fun x number -> 
    match x with 
    | Some _ -> x 
    | None   -> optional number a identity  
 ) None numbers in 
 match ret with 
 | Some x -> x 
 | None -> failwith "None of oneof value could be found." 



let e () = failwith "programmatic error" 
|}
