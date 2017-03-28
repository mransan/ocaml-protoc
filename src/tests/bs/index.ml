let () = 
  let open Unittest_pb in 
  let all_basic_types = {
    field01 = 1.20001;
    field02 = 1.2000000001;
    field03 = 0xEFFFFFFFl;
    field04 = 0xEBABABABABABABABL;
    field05 = 0xEFFFFFFFl;
    field06 = 0xEBABABABABABABABL;
    field07 = 0xEFFFFFFFl;
    field08 = 0xEBABABABABABABABL;
    field09 = 0xEFFFFFFFl;
    field10 = 0xEBABABABABABABABL;
    field13 = true;
    field14 = "This is a test \"string\"";
    repeated01 = [1.20001;                    ]; 
    repeated02 = [1.2000000001;               ]; 
    repeated03 = [0xEFFFFFFFl;                 ]; 
    repeated04 = [0xEBABABABABABABABL;         ]; 
    repeated05 = [0xEFFFFFFFl;                 ]; 
    repeated06 = [0xEBABABABABABABABL;         ]; 
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

  let single_one_of_string = 
    String_value "This is the single one \"string\""
  in 
  
  let single_one_of_int = 
    Int_value 0xEFABABABl
  in 

  let single_one_of_enum = 
    Enum_value test_enum0
  in 

  let single_one_of_small_message = 
    Small_message small_message
  in 

  let single_one_of_recursive = 
    Recursive_value single_one_of_small_message
  in 

  let test = {
    all_basic_types = Some all_basic_types;
    test_enum0;
    test_enum1;
    test_enum2;
    single_one_of_string = Some single_one_of_string;
    single_one_of_int = Some single_one_of_int;
    single_one_of_enum = Some single_one_of_enum;
    single_one_of_small_message = Some single_one_of_small_message;
    single_one_of_recursive = Some single_one_of_recursive;
  } in 

  let json_str = 
    let dict = Js_dict.empty () in 
    Unittest_pb.encode_test test dict; 
    Js.log dict;
    Js_json.stringify (Js_json.object_ dict)
  in

  let test'= 
     match Js_json.decodeObject (Js_json.parse json_str) with
     | Some dict -> Unittest_pb.decode_test dict 
     | None -> assert(false)
  in 

  assert(test = test');
  ()

let () = print_endline "\nTests... Ok"
