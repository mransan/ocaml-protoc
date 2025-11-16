module T = Test19

let decode_pb_ref_data () =
  let hash = Hashtbl.create 3 in
  Hashtbl.add hash "one" "1";
  Hashtbl.add hash "two" "2";
  Hashtbl.add hash "three" "3";

  let m = T.default_hash () in
  T.hash_set_t m hash;
  m

let test_contain_data { T.t } =
  assert (Hashtbl.mem t "one");
  assert (Hashtbl.mem t "two");
  assert (Hashtbl.mem t "three");
  ()

let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    let buffer, _ = Test_util.get_binary_file_content "test19.c2ml.data" in
    let decoder = Pbrt.Decoder.of_bytes buffer in
    let hash = T.decode_pb_hash decoder in
    let () = test_contain_data hash in
    print_endline "ML: -- Good --"
  | Test_util.Encode ->
    Test_util.encode "test19.ml2c.data" T.encode_pb_hash (decode_pb_ref_data ())
