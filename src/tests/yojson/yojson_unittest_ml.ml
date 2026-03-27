let test =
  let open Yojson_unittest in
  let all_basic_types =
    make_all_basic_types ~field01:1.2000001 ~field02:1.2 ~field03:0xEFFFFFFFl
      ~field04:0xEBABABABABABABABL ~field05:0x7FFFFFFFl
      ~field06:0x7BABABABABABABABL ~field07:0xEFFFFFFFl
      ~field08:0xEBABABABABABABABL ~field09:0xEFFFFFFFl
      ~field10:0xEBABABABABABABABL ~field13:true
      ~field14:"This is a test \"string\"" ~repeated01:[ 1.2000001 ]
      ~repeated02:[ 1.2 ] ~repeated03:[ 0xEFFFFFFFl ]
      ~repeated04:[ 0xEBABABABABABABABL ] ~repeated05:[ 0x7FFFFFFFl ]
      ~repeated06:[ 0x7BABABABABABABABL ] ~repeated07:[ 0xEFFFFFFFl ]
      ~repeated08:[ 0xEBABABABABABABABL ] ~repeated09:[ 0xEFFFFFFFl ]
      ~repeated10:[ 0xEBABABABABABABABL ] ~repeated13:[ true ]
      ~repeated14:[ "This is a test \"string\"" ]
      ()
  in

  let small_message =
    make_small_message ~sm_string:"This \"IS\" a small string" ()
  in

  let test_enum0 = Value0 in
  let test_enum1 = Value1 in
  let test_enum2 = Value_two in

  let single_one_of_string : single_one_of =
    String_value "This is the single one \"string\""
  in

  let single_one_of_int : single_one_of = Int_value 0xEFABABABl in

  let single_one_of_enum : single_one_of = Enum_value test_enum0 in

  let single_one_of_small_message : single_one_of =
    Small_message small_message
  in

  let single_one_of_recursive : single_one_of =
    Recursive_value single_one_of_small_message
  in

  let basic0 : abasic_message = make_abasic_message ~name:"basic0" () in

  make_test ~all_basic_types ~test_enum0 ~test_enum1 ~test_enum2
    ~single_one_of_string ~single_one_of_int ~single_one_of_enum
    ~single_one_of_small_message ~single_one_of_recursive
    ~repeated_enum:[ test_enum0; test_enum1; test_enum2 ]
    ~basic:[ basic0 ] ()

let () =
  let json_str =
    Yojson_unittest.encode_json_test test |> Yojson.Basic.to_string
  in

  print_endline json_str;

  let test' =
    json_str |> Yojson.Basic.from_string |> Yojson_unittest.decode_json_test
  in

  assert (test = test');
  ()

let () = print_endline "\nConsistency tests... Ok"

let () =
  let ic =
    let filename = "yojson.data" in
    open_in filename
  in

  let buffer_len = 1024 * 1024 in
  let buffer = Bytes.create buffer_len in
  let buffer_len =
    match input ic buffer 0 buffer_len with
    | i when i < buffer_len -> i
    | _ -> assert false
  in

  let test' =
    Bytes.sub_string buffer 0 buffer_len
    |> Yojson.Basic.from_string |> Yojson_unittest.decode_json_test
  in

  let open Yojson_unittest in
  assert (test'.all_basic_types = test.all_basic_types);
  assert (test'.test_enum0 = test.test_enum0);
  assert (test'.test_enum1 = test.test_enum1);
  assert (test'.test_enum2 = test.test_enum2);
  assert (test'.single_one_of_string = test.single_one_of_string);
  assert (test'.single_one_of_int = test.single_one_of_int);
  assert (test'.single_one_of_enum = test.single_one_of_enum);
  assert (test'.single_one_of_small_message = test.single_one_of_small_message);
  assert (test'.single_one_of_recursive = test.single_one_of_recursive);

  print_endline "\nConformance tests ... Ok";

  ()

let () =
  let open Yojson_unittest in
  let get_field key json =
    match json with
    | `Assoc fields -> List.assoc key fields
    | _ -> failwith "expected assoc"
  in
  (* double (field01) special values: must encode as JSON strings "NaN", "Infinity", "-Infinity" *)
  let check_double v expected_token =
    let t = make_all_basic_types ~field01:v () in
    let json = encode_json_all_basic_types t in
    assert (get_field "field01" json = expected_token);
    decode_json_all_basic_types json
  in
  let t' = check_double Float.nan (`String "NaN") in
  assert (Float.is_nan t'.field01);
  let t' = check_double Float.infinity (`String "Infinity") in
  assert (t'.field01 = Float.infinity);
  let t' = check_double Float.neg_infinity (`String "-Infinity") in
  assert (t'.field01 = Float.neg_infinity);

  (* float (field02, Pk_bits32) special values *)
  let check_float v expected_token =
    let t = make_all_basic_types ~field02:v () in
    let json = encode_json_all_basic_types t in
    assert (get_field "field02" json = expected_token);
    decode_json_all_basic_types json
  in
  let t' = check_float Float.nan (`String "NaN") in
  assert (Float.is_nan t'.field02);
  let t' = check_float Float.infinity (`String "Infinity") in
  assert (t'.field02 = Float.infinity);
  let t' = check_float Float.neg_infinity (`String "-Infinity") in
  assert (t'.field02 = Float.neg_infinity);

  print_endline "\nFloat special values ... Ok"

let () =
  let open Yojson_unittest in
  (* Decoder must accept proto field names (snake_case) alongside camelCase *)
  let json = `Assoc [ ("sm_string", `String "hello proto name") ] in
  let msg = decode_json_small_message json in
  assert (msg.sm_string = "hello proto name");

  (* Also verify camelCase still works *)
  let json2 = `Assoc [ ("smString", `String "hello camel case") ] in
  let msg2 = decode_json_small_message json2 in
  assert (msg2.sm_string = "hello camel case");

  print_endline "\nProto field name decode ... Ok"
