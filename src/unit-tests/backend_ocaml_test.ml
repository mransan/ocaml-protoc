
let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () = 
  let message_scope = [] in 
  let oneof_name = "test" in 
  assert ("test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = [] in 
  let oneof_name = "TEST" in 
  assert ("test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = [] in 
  let oneof_name = "tEST" in 
  assert ("test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = [] in 
  let oneof_name = "tEST_Max" in 
  assert ("test_max" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = ["a"] in 
  let oneof_name = "test" in 
  assert ("a_test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = ["abc"] in 
  let oneof_name = "test" in 
  assert ("abc_test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = ["abc"; "def"] in 
  let oneof_name = "test" in 
  assert ("abc_def_test" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 

let () = 
  let s = "test" in 
  assert("Test" = Backend_ocaml.constructor_name s);
  ()
 
let () = 
  let s = "testBlah" in 
  assert("Testblah" = Backend_ocaml.constructor_name s);
  ()
  (* TODO this could really be improved to be Test_blah *)
 
let () = 
  let message_scope = {
    Pbtt.namespaces = ["ab"; "cd"]; 
    Pbtt.message_names = ["foo"; "bar"] 
  } in 
  let message_name = "test" in 
  assert("Ab.Cd.foo_bar_test" 
          = Backend_ocaml.type_name_of_message Pbtt_util.empty_scope message_scope message_name);
  ()
 
let () = 
  let message_scope = { 
    Pbtt.namespaces = ["ab"; "cd"]; 
    Pbtt.message_names = []; 
  } in 
  let message_name = "test" in 
  assert("Ab.Cd.test" 
         = Backend_ocaml.type_name_of_message Pbtt_util.empty_scope message_scope message_name);
  ()
 
let () = 
  let message_scope = {
    Pbtt.message_names  = ["foo";"bar";]; 
    Pbtt.namespaces = [];
  } in 
  let message_name = "test" in 
  assert("foo_bar_test" 
         = Backend_ocaml.type_name_of_message Pbtt_util.empty_scope message_scope message_name);
  ()
 
(** TODO: Add test cases for when the field_message_scope is NOT empty
 *)
module BO = Backend_ocaml

let compile_to_ocaml s = 
  let ast = parse Parser.message_ s in 
  let all_types = Pbtt_util.compile_message_p1 Pbtt_util.empty_scope ast in 
  let all_types = List.map (fun t -> 
    Pbtt_util.compile_type_p2 all_types t
  ) all_types in 
  List.flatten @@ List.map (fun t ->
    BO.compile all_types t 
  ) all_types
 
let () = 
  let s = "
  message M {
    required uint32 v1 = 1; 
    required string v2 = 2; 
    optional bool   v3 = 3; 
    optional float  v4 = 4; 
    optional double v5 = 5; 
    required bytes  v6 = 6; 
  }"
  in 
  let ocaml_types = compile_to_ocaml s in 
  assert(1 = List.length ocaml_types); 
  assert(Ocaml_types.(Record {
    record_name = "m"; 
    fields = [
      {field_type = Int; field_name = "v1"; type_qualifier = No_qualifier;
      encoding_type = Regular_field {field_number = 1; nested = false;  payload_kind = Encoding_util.Varint false}};
      {field_type = String; field_name = "v2"; type_qualifier = No_qualifier;
      encoding_type = Regular_field {field_number = 2; nested = false; payload_kind = Encoding_util.Bytes}};
      {field_type = Bool; field_name = "v3"; type_qualifier = Option; 
      encoding_type = Regular_field {field_number = 3; nested = false; payload_kind = Encoding_util.Varint false}};
      {field_type = Float; field_name = "v4"; type_qualifier = Option;
      encoding_type = Regular_field {field_number = 4; nested = false; payload_kind = Encoding_util.Bits32}};
      {field_type = Float; field_name = "v5"; type_qualifier = Option;
      encoding_type = Regular_field {field_number = 5; nested = false; payload_kind = Encoding_util.Bits64}};
      {field_type = Bytes; field_name = "v6"; type_qualifier = No_qualifier;
      encoding_type = Regular_field {field_number = 6; nested = false; payload_kind = Encoding_util.Bytes}};
    ];
  }) = List.hd ocaml_types);
  () 
 
let () = 
  let s = "
  message M1 {
    required uint32 m11 = 1; 
    message M2 {
      required uint32 m21 = 1; 
    }
    required M2 sub = 2;
  }
  "
  in 
  let ocaml_types = compile_to_ocaml s in 
  assert(2 = List.length ocaml_types); 
  assert(
    Ocaml_types.(Record {
      record_name = "m1_m2"; 
      fields = [
        {field_type = Int; field_name = "m21"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}};
      ];
    }) = List.nth ocaml_types 0);
  assert(
    Ocaml_types.(Record {
      record_name = "m1"; 
      fields = [
        {field_type = Int; field_name = "m11"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}};
        {field_type = User_defined_type "m1_m2"; field_name = "sub"; type_qualifier = No_qualifier;
        encoding_type = Regular_field {field_number = 2; nested = true; payload_kind = Encoding_util.Bytes}};
      ];
    }) = List.nth ocaml_types 1);
  () 
 
let () = 
  let s = "
  message M1 {
    oneof o1 { 
      uint32 intv    = 1; 
      string stringv = 2; 
    }
    required uint32 v1 = 3;
  }
  "
  in 
  let ocaml_types = compile_to_ocaml s in 
  assert(2 = List.length ocaml_types); 
  
  let variant = Ocaml_types.({
      variant_name  = "m1_o1"; 
      constructors = [
        {field_type = Int; field_name = "Intv"; type_qualifier = No_qualifier;
         encoding_type = {Encoding_util.field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}};
        {field_type = String; field_name = "Stringv"; type_qualifier = No_qualifier;
         encoding_type = {field_number = 2; nested = false; payload_kind = Encoding_util.Bytes}};
      ];
    }) in
  assert(Ocaml_types.Variant variant = List.nth ocaml_types 0);
  assert(
    Ocaml_types.(Record {
      record_name = "m1"; 
      fields = [
        {field_type = User_defined_type "m1_o1"; field_name = "o1"; type_qualifier = No_qualifier;
        encoding_type = One_of variant};
        {field_type = Int; field_name = "v1"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {Encoding_util.field_number = 3; nested = false; payload_kind = Encoding_util.Varint false}};
      ];
    }) = List.nth ocaml_types 1);
  () 

let () = 
  print_endline "Backend OCaml Tests ... Ok"
