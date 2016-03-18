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
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int   -> "Pbrt.Decoder.int_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int   -> "Pbrt.Decoder.int_as_zigzag" 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int32 -> "Pbrt.Decoder.int32_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int32 -> "Pbrt.Decoder.int32_as_zigzag" 
  | `Decode , Encoding_util.Varint false, Ocaml_types.Int64 -> "Pbrt.Decoder.int64_as_varint" 
  | `Decode , Encoding_util.Varint true , Ocaml_types.Int64 -> "Pbrt.Decoder.int64_as_zigzag" 
  | `Decode , Encoding_util.Bits32, Ocaml_types.Int32 -> "Pbrt.Decoder.int32_as_bits32" 
  | `Decode , Encoding_util.Bits64, Ocaml_types.Int64 -> "Pbrt.Decoder.int64_as_bits64" 
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
  | `Encode , Encoding_util.Bits32, Ocaml_types.Int32 -> "Pbrt.Encoder.int32_as_bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Int64 -> "Pbrt.Encoder.int64_as_bits64" 
  | `Encode , Encoding_util.Varint false, Ocaml_types.Bool -> "Pbrt.Encoder.bool" 
  | `Encode , Encoding_util.Bits32, Ocaml_types.Float -> "Pbrt.Encoder.float_as_bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Float -> "Pbrt.Encoder.float_as_bits64" 
  | `Encode , Encoding_util.Bits32, Ocaml_types.Int -> "Pbrt.Encoder.int_as_bits32" 
  | `Encode , Encoding_util.Bits64, Ocaml_types.Int -> "Pbrt.Encoder.int_as_bits64" 
  | `Encode , Encoding_util.Bytes, Ocaml_types.String -> "Pbrt.Encoder.string" 
  | `Encode , Encoding_util.Bytes, Ocaml_types.Bytes -> "Pbrt.Encoder.bytes" 
  | _ -> failwith "Invalid encoding/OCaml type combination"
