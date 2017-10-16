(* unit tests for the wrapper specific encoding *)

let do_test encode decode v =
  let e = Pbrt.Encoder.create () in
  encode (Some v) e;
  let b = Pbrt.Encoder.to_bytes e in
  let d = Pbrt.Decoder.of_bytes b in
  begin match decode d with
  | Some x when x = v -> ()
  | _ -> assert(false)
  end

let round32 v =
  let open Int32 in
  v |> bits_of_float |> float_of_bits

let () =
  let open Pbrt in
  do_test Encoder.wrapper_double_value Decoder.wrapper_double_value 1.23;
  do_test
    Encoder.wrapper_float_value
    Decoder.wrapper_float_value
    (round32 1.23);
  do_test Encoder.wrapper_int64_value Decoder.wrapper_int64_value 123L;
  do_test Encoder.wrapper_int32_value Decoder.wrapper_int32_value 123l;
  do_test Encoder.wrapper_bool_value Decoder.wrapper_bool_value true;
  do_test Encoder.wrapper_bool_value Decoder.wrapper_bool_value false;
  do_test Encoder.wrapper_string_value Decoder.wrapper_string_value "";
  do_test Encoder.wrapper_string_value Decoder.wrapper_string_value "abc";
  do_test
    Encoder.wrapper_bytes_value
    Decoder.wrapper_bytes_value
    (Bytes.of_string "");
  do_test
    Encoder.wrapper_bytes_value
    Decoder.wrapper_bytes_value
    (Bytes.of_string "abc");
  ()
