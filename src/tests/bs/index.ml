let () = 
  let open Bs_unittest_types in 
  let all_basic_types = {
    field01 = 1.20001;
    field02 = 1.2000000001;
    field03 = 0xEFFFFFFFl;
    field04 = 0xEBABABABABABABABL;
    field05 = 0xEFFFFFFFl;
    field06 = 0xEBABABABABABABABL;
    field07 = 0xEFFFFFFFl;
    field08 = 0xEBABABABABABABABL;
    field09 = 0x7FFFFFFFl;
    field10 = 0xEBABABABABABABABL;
    field11 = 0xEFFFFFFFl;
    field12 = 0xEBABABABABABABABL;
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
    repeated09 = [0x7FFFFFFFl;                 ]; 
    repeated10 = [0xEBABABABABABABABL;         ]; 
    repeated11 = [0xEFFFFFFFl;                 ]; 
    repeated12 = [0xEBABABABABABABABL;         ]; 
    repeated13 = [true;                       ]; 
    repeated14 = ["This is a test \"string\"";]; 
    empty = [];
    int32_wrapped_value= Some(123l);
    int32_wrapped_none = None;
    int64_wrapped_value= Some(123L);
    int64_wrapped_none = None;
    float_wrapped_value= Some(123.456);
    float_wrapped_none = None;
    double_wrapped_value= Some(123.456);
    double_wrapped_none = None;
    string_wrapped_value= Some("This is an optional test string");
    string_wrapped_none = None;
    bool_wrapped_value= Some(false);
    bool_wrapped_none = None;
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
    repeated_small_message = [
      {sm_string = "sm1"};
      {sm_string = "sm2"};
      {sm_string = "sm3"};
    ];
    repeated_small_message_empty = [];
  } in 

  let json_str = 
    let dict = Bs_unittest_bs.encode_test test in
    Js.log dict;
    Js.Json.stringify (Js.Json.object_ dict)
  in

  let test'= 
     match Js.Json.decodeObject (Js.Json.parseExn json_str) with
     | Some dict -> Bs_unittest_bs.decode_test dict 
     | None -> assert(false)
  in 

  assert(test = test');
  ()

let () = print_endline "\nTests... Ok"
