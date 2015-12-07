let prefix_payload_to_ocaml_t  = {|

module P  = Printf
  
let add_indentation n s = 
  Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  

|}

let runtime_function = function 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int   -> "Pbrt.Decoder.int_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int   -> "Pbrt.Decoder.int_as_zigzag" 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int32 -> "Pbrt.Decoder.int32_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int32 -> "Pbrt.Decoder.int32_as_zigzag" 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int64 -> "Pbrt.Decoder.int64_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int64 -> "Pbrt.Decoder.int64_as_zigzag" 
  | `Decode , Encoding_util.Bits32, Ocaml_types.Int32 -> "Pbrt.Decoder.bits32" 
  | `Decode , Encoding_util.Bits64, Ocaml_types.Int64 -> "Pbrt.Decoder.bits64" 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Bool -> "Pbrt.Decoder.bool" 
  | `Decode , Encoding_util.Bits32, Ocaml_types.Float -> "Pbrt.Decoder.float_as_bits32" 
  | `Decode , Encoding_util.Bits64, Ocaml_types.Float -> "Pbrt.Decoder.float_as_bits64" 
  | `Decode , Encoding_util.Bits32, Ocaml_types.Int -> "Pbrt.Decoder.int_as_bits32" 
  | `Decode , Encoding_util.Bits64, Ocaml_types.Int -> "Pbrt.Decoder.int_as_bits64" 
  | `Decode , Encoding_util.Bytes, Ocaml_types.String -> "Pbrt.Decoder.string" 
  | `Decode , Encoding_util.Bytes, Ocaml_types.Bytes -> "Pbrt.Decoder.bytes" 
  | `Encode , Encoding_util.Varint false, Ocaml_types.Int   -> "Pbrt.Encoder.int_as_varint" 
  | `Encode , Encoding_util.Varint true , Ocaml_types.Int   -> "Pbrt.Encoder.int_as_zigzag" 
  | `Encode , Encoding_util.Varint false, Ocaml_types.Int32 -> "Pbrt.Encoder.int32_as_varint" 
  | `Encode , Encoding_util.Varint true , Ocaml_types.Int32 -> "Pbrt.Encoder.int32_as_zigzag" 
  | `Encode , Encoding_util.Varint false, Ocaml_types.Int64 -> "Pbrt.Encoder.int64_as_varint" 
  | `Encode , Encoding_util.Varint true , Ocaml_types.Int64 -> "Pbrt.Encoder.int64_as_zigzag" 
  | `Encode , Encoding_util.Bits32, Ocaml_types.Int32 -> "Pbrt.Encoder.bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Int64 -> "Pbrt.Encoder.bits64" 
  | `Encode , Encoding_util.Varint false, Ocaml_types.Bool -> "Pbrt.Encoder.bool" 
  | `Encode , Encoding_util.Bits32, Ocaml_types.Float -> "Pbrt.Encoder.float_as_bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Float -> "Pbrt.Encoder.float_as_bits64" 
  | `Encode , Encoding_util.Bits32, Ocaml_types.Int -> "Pbrt.Encoder.int_as_bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Int -> "Pbrt.Encoder.int_as_bits64" 
  | `Encode , Encoding_util.Bytes, Ocaml_types.String -> "Pbrt.Encoder.string" 
  | `Encode , Encoding_util.Bytes, Ocaml_types.Bytes -> "Pbrt.Encoder.bytes" 
  | _ -> failwith "Invalid encoding/OCaml type combination"
