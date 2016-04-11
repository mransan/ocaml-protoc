(*
  The MIT License (MIT)
  
  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

let runtime_function = function 
  | `Decode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int   -> "Pbrt.Decoder.int_as_varint" 
  | `Decode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int   -> "Pbrt.Decoder.int_as_zigzag" 
  | `Decode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int32 -> "Pbrt.Decoder.int32_as_varint" 
  | `Decode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int32 -> "Pbrt.Decoder.int32_as_zigzag" 
  | `Decode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int64 -> "Pbrt.Decoder.int64_as_varint" 
  | `Decode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int64 -> "Pbrt.Decoder.int64_as_zigzag" 
  | `Decode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_int32 -> "Pbrt.Decoder.int32_as_bits32" 
  | `Decode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_int64 -> "Pbrt.Decoder.int64_as_bits64" 
  | `Decode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_bool -> "Pbrt.Decoder.bool" 
  | `Decode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_float -> "Pbrt.Decoder.float_as_bits32" 
  | `Decode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_float -> "Pbrt.Decoder.float_as_bits64" 
  | `Decode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_int -> "Pbrt.Decoder.int_as_bits32" 
  | `Decode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_int -> "Pbrt.Decoder.int_as_bits64" 
  | `Decode , Ocaml_types.Pk_bytes, Ocaml_types.Bt_string -> "Pbrt.Decoder.string" 
  | `Decode , Ocaml_types.Pk_bytes, Ocaml_types.Bt_bytes -> "Pbrt.Decoder.bytes" 
  | `Encode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int   -> "Pbrt.Encoder.int_as_varint" 
  | `Encode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int   -> "Pbrt.Encoder.int_as_zigzag" 
  | `Encode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int32 -> "Pbrt.Encoder.int32_as_varint" 
  | `Encode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int32 -> "Pbrt.Encoder.int32_as_zigzag" 
  | `Encode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_int64 -> "Pbrt.Encoder.int64_as_varint" 
  | `Encode , Ocaml_types.Pk_varint true , Ocaml_types.Bt_int64 -> "Pbrt.Encoder.int64_as_zigzag" 
  | `Encode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_int32 -> "Pbrt.Encoder.int32_as_bits32" 
  | `Encode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_int64 -> "Pbrt.Encoder.int64_as_bits64" 
  | `Encode , Ocaml_types.Pk_varint false, Ocaml_types.Bt_bool -> "Pbrt.Encoder.bool" 
  | `Encode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_float -> "Pbrt.Encoder.float_as_bits32" 
  | `Encode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_float -> "Pbrt.Encoder.float_as_bits64" 
  | `Encode , Ocaml_types.Pk_bits32, Ocaml_types.Bt_int -> "Pbrt.Encoder.int_as_bits32" 
  | `Encode , Ocaml_types.Pk_bits64, Ocaml_types.Bt_int -> "Pbrt.Encoder.int_as_bits64" 
  | `Encode , Ocaml_types.Pk_bytes, Ocaml_types.Bt_string -> "Pbrt.Encoder.string" 
  | `Encode , Ocaml_types.Pk_bytes, Ocaml_types.Bt_bytes -> "Pbrt.Encoder.bytes" 
  | _ -> failwith "Invalid encoding/OCaml type combination"
