(* unit tests for the wrapper specific encoding *)

let spf = Printf.sprintf

let do_test pp encode decode v =
  let e = Pbrt.Encoder.create () in
  encode (Some v) e;
  let b = Pbrt.Encoder.to_bytes e in
  (* Printf.printf "encoded: %S for %s\n" (Bytes.unsafe_to_string b) (pp v); *)
  let d = Pbrt.Decoder.of_bytes b in
  match decode d with
  | Some x when x = v -> ()
  | Some y ->
    Printf.eprintf "expected %s, got %s\n%!" (pp v) (pp y);
    assert false
  | _ -> assert false

let round32 v =
  let open Int32 in
  v |> bits_of_float |> float_of_bits

let pp_bytes b = spf "%S" (Bytes.unsafe_to_string b)

let () =
  let open Pbrt in
  do_test (spf "%f") Encoder.wrapper_double_value Decoder.wrapper_double_value
    1.23;
  do_test (spf "%f") Encoder.wrapper_float_value Decoder.wrapper_float_value
    (round32 1.23);
  do_test (spf "%LdL") Encoder.wrapper_int64_value Decoder.wrapper_int64_value
    123L;
  do_test (spf "%ldl") Encoder.wrapper_int32_value Decoder.wrapper_int32_value
    123l;
  do_test (spf "%b") Encoder.wrapper_bool_value Decoder.wrapper_bool_value true;
  do_test (spf "%b") Encoder.wrapper_bool_value Decoder.wrapper_bool_value false;
  do_test (spf "%S") Encoder.wrapper_string_value Decoder.wrapper_string_value
    "";
  do_test (spf "%S") Encoder.wrapper_string_value Decoder.wrapper_string_value
    "abc";
  do_test pp_bytes Encoder.wrapper_bytes_value Decoder.wrapper_bytes_value
    (Bytes.of_string "");
  do_test pp_bytes Encoder.wrapper_bytes_value Decoder.wrapper_bytes_value
    (Bytes.of_string "abc");
  ()
