let test = 
  let open Yojson_unittest_types in 
  let all_basic_types = {
    field01 = 1.2000001;
    field02 = 1.2;
    field03 = 0xEFFFFFFFl;
    field04 = 0xEBABABABABABABABL;
    field05 = 0x7FFFFFFFl;
    field06 = 0x7BABABABABABABABL;
    field07 = 0xEFFFFFFFl;
    field08 = 0xEBABABABABABABABL;
    field09 = 0xEFFFFFFFl;
    field10 = 0xEBABABABABABABABL;
    field13 = true;
    field14 = "This is a test \"string\"";
    repeated01 = [1.2000001;                    ]; 
    repeated02 = [1.2;               ]; 
    repeated03 = [0xEFFFFFFFl;                 ]; 
    repeated04 = [0xEBABABABABABABABL;         ]; 
    repeated05 = [0x7FFFFFFFl;                 ]; 
    repeated06 = [0x7BABABABABABABABL;         ]; 
    repeated07 = [0xEFFFFFFFl;                 ]; 
    repeated08 = [0xEBABABABABABABABL;         ]; 
    repeated09 = [0xEFFFFFFFl;                 ]; 
    repeated10 = [0xEBABABABABABABABL;         ]; 
    repeated13 = [true;                       ]; 
    repeated14 = ["This is a test \"string\"";]; 
  } in  

  let small_message = {
    sm_string = "This \"IS\" a small string";
  } in 

  let test_enum0 = Value0 in 
  let test_enum1 = Value1 in 
  let test_enum2 = Value_two in 

  let single_one_of_string : single_one_of = 
    String_value "This is the single one \"string\""
  in 
  
  let single_one_of_int : single_one_of = 
    Int_value 0xEFABABABl
  in 

  let single_one_of_enum : single_one_of = 
    Enum_value test_enum0
  in 

  let single_one_of_small_message : single_one_of = 
    Small_message small_message
  in 

  let single_one_of_recursive : single_one_of = 
    Recursive_value single_one_of_small_message
  in 

  {
    all_basic_types = Some all_basic_types;
    test_enum0;
    test_enum1;
    test_enum2;
    single_one_of_string = Some single_one_of_string;
    single_one_of_int = Some single_one_of_int;
    single_one_of_enum = Some single_one_of_enum;
    single_one_of_small_message = Some single_one_of_small_message;
    single_one_of_recursive = Some single_one_of_recursive;
    repeated_enum = [test_enum0; test_enum1; test_enum2];
  }

let () = 

  let json_str = 
    Yojson_unittest_yojson.encode_test test
    |> Yojson.Basic.to_string 
  in

  print_endline json_str;

  let test'= 
    json_str 
    |> Yojson.Basic.from_string 
    |> Yojson_unittest_yojson.decode_test  
  in 

  assert(test = test');
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
    | _ -> assert(false) 
  in
    
  let test' = 
    Bytes.sub_string buffer 0 buffer_len
    |> Yojson.Basic.from_string
    |> Yojson_unittest_yojson.decode_test 
  in 

  let open Yojson_unittest_types in

  assert(test'.all_basic_types             = test.all_basic_types);
  assert(test'.test_enum0                  = test.test_enum0);
  assert(test'.test_enum1                  = test.test_enum1);
  assert(test'.test_enum2                  = test.test_enum2);
  assert(test'.single_one_of_string        = test.single_one_of_string);
  assert(test'.single_one_of_int           = test.single_one_of_int);
  assert(test'.single_one_of_enum          = test.single_one_of_enum);
  assert(test'.single_one_of_small_message = test.single_one_of_small_message);
  assert(test'.single_one_of_recursive     = test.single_one_of_recursive);

  print_endline "\nConformance tests ... Ok";

  ()
