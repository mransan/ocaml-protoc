
let prefix_payload_to_ocaml_t  = {|

module P  = Printf
  
let add_indentation n s = 
  Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  

let decode_varint_as_int = Pbrt.Decoder.varint

let decode_varint_as_int64 = Pbrt.Decoder.varint_64 

let decode_varint_as_int32 = Pbrt.Decoder.varint_32 

let decode_varint_zigzag_as_int = Pbrt.Decoder.zigzag  

let decode_varint_zigzag_as_int64 = Pbrt.Decoder.zigzag_64  

let encode_int_as_varint = Pbrt.Encoder.varint 

let encode_int64_as_varint = Pbrt.Encoder.varint_64 

let encode_int32_as_varint = Pbrt.Encoder.varint_32 

let encode_int_as_varint_zigzag = Pbrt.Encoder.zigzag 

let encode_int64_as_varint_zigzag = Pbrt.Encoder.zigzag_64 

let decode_bits32_as_int decoder = 
  (* Pbrt.Decoder.int_of_int32 "" @@ Pbrt.Decoder.bits32 decoder *) 
  Int32.to_int @@ Pbrt.Decoder.bits32 decoder

let decode_bits32_as_int32 = Pbrt.Decoder.bits32 

let encode_int_as_bits32 v encoder = 
  Pbrt.Encoder.bits32 (Pbrt.Encoder.int32_of_int "" v) encoder

let encode_int32_as_bits32 = Pbrt.Encoder.bits32 

let decode_bits64_as_int decoder = 
  (* Pbrt.Decoder.int_of_int64 "" @@ Pbrt.Decoder.bits64 decoder *)
  Int64.to_int @@ Pbrt.Decoder.bits64 decoder 

let decode_bits64_as_int64 = Pbrt.Decoder.bits64

let encode_int_as_bits64 v encoder = 
  Pbrt.Encoder.bits64 (Int64.of_int v) encoder

let encode_int64_as_bits64 = Pbrt.Encoder.bits64 

let decode_varint_as_bool decoder = 
  Pbrt.Decoder.bool_of_int64 "" @@ Pbrt.Decoder.varint_64 decoder

let encode_bool_as_varint v encoder = 
  Pbrt.Encoder.varint_64  (Int64.of_int @@ if v  then 1 else 0) encoder 

let decode_bits32_as_float decoder = 
  Int32.float_of_bits @@ Pbrt.Decoder.bits32 decoder 

let encode_float_as_bits32 v encoder =
  Pbrt.Encoder.bits32 (Int32.bits_of_float v) encoder 
  
let decode_bits64_as_float decoder = 
  Int64.float_of_bits @@ Pbrt.Decoder.bits64 decoder 

let encode_float_as_bits64 v encoder =
  Pbrt.Encoder.bits64 (Int64.bits_of_float v) encoder

let decode_bytes_as_string decoder = 
  Bytes.to_string @@ Pbrt.Decoder.bytes decoder 

let encode_string_as_bytes v encoder = 
  Pbrt.Encoder.bytes (Bytes.of_string v) encoder 

let decode_bytes_as_bytes  = Pbrt.Decoder.bytes 

let encode_bytes_as_bytes  = Pbrt.Encoder.bytes

|}

let prefix_decode_f = {|

let decode decoder mappings values = 

  let continue = ref true in 
  while !continue do 
    match Pbrt.Decoder.key decoder with 
    | None -> continue := false
    | Some (number, payload_kind) -> (
      try 
        let v' = Array.unsafe_get values number in 
        let v = mappings decoder (number, v') in 
        Array.unsafe_set values number v
      with 
      | Not_found ->  (
        Pbrt.Decoder.skip decoder payload_kind; 
      )
    )
  done;
  values

let required number a f = 
  match f @@ Array.unsafe_get a number with 
  | []     -> failwith (P.sprintf "field %i missing" number)
  | hd::_  -> hd
  (* TODO Improve *) 

let e i = failwith @@ Printf.sprintf "programmatic error for field %i" i 

let optional number a f = 
  match Array.unsafe_get a number with 
  | `Default  -> None
  | v         -> (match f v with
    | hd::_ -> Some hd
    | _     -> e number   
  )
  (* TODO Improve *) 

let list_ number a f = 
  List.rev @@ f @@ Array.unsafe_get a number

external identity: 'a -> 'a = "%identity"

let oneof numbers a f = 
  let ret = List.fold_left (fun x number -> 
    match x with 
    | Some _ -> x 
    | None   -> optional number a f
  ) None numbers in 
  match ret with 
  | Some x -> x 
  | None -> failwith "None of oneof value could be found." 

|}
