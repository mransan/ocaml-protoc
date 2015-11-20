
let file_name = "a.proto"

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
 
 
 
(** TODO: Add test cases for when the field_message_scope is NOT empty
 *)
module BO = Backend_ocaml

let compile_to_ocaml ?all_pbtt_types:(all_pbtt_types = []) ?file_name:(file_name = file_name) s = 
  let ast = parse Parser.message_ s in 
  let all_pbtt_types' = Pbtt_util.compile_message_p1 file_name Pbtt_util.empty_scope ast in 
  let all_pbtt_types' = all_pbtt_types' @ all_pbtt_types in 
  let all_pbtt_types'' = List.map (fun t -> 
    Pbtt_util.compile_type_p2 all_pbtt_types' t
  ) all_pbtt_types' in 

  let all_caml_types = List.flatten @@ List.map (fun t ->
    BO.compile all_pbtt_types'' t 
  ) all_pbtt_types'' in 
  (all_pbtt_types', all_caml_types) 
 
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
  let _, ocaml_types = compile_to_ocaml s in 
  assert(1 = List.length ocaml_types); 
  assert(Ocaml_types.({
    module_ = "A";
    spec    = Record {
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
  }}) = List.hd ocaml_types);
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
  let _, ocaml_types = compile_to_ocaml s in 
  assert(2 = List.length ocaml_types); 
  assert(
    Ocaml_types.({
      module_ = "A"; 
      spec    = Record {
      record_name = "m1_m2"; 
      fields = [
        {field_type = Int; field_name = "m21"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}};
      ];
    }}) = List.nth ocaml_types 0);
  assert(
    Ocaml_types.({
      module_ = "A";
      spec    = Record {
      record_name = "m1"; 
      fields = [
        {field_type = Int; field_name = "m11"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}};
        {field_type = User_defined_type "m1_m2"; field_name = "sub"; type_qualifier = No_qualifier;
        encoding_type = Regular_field {field_number = 2; nested = true; payload_kind = Encoding_util.Bytes}};
      ];
    }}) = List.nth ocaml_types 1);
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
  let _, ocaml_types = compile_to_ocaml s in 
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
  assert(Ocaml_types.({module_ = "A"; spec = Variant variant}) = List.nth ocaml_types 0);
  assert(
    Ocaml_types.({
      module_ = "A"; 
      spec    = Record {
      record_name = "m1"; 
      fields = [
        {field_type = User_defined_type "m1_o1"; field_name = "o1"; type_qualifier = No_qualifier;
        encoding_type = One_of variant};
        {field_type = Int; field_name = "v1"; type_qualifier = No_qualifier;
         encoding_type = Regular_field {Encoding_util.field_number = 3; nested = false; payload_kind = Encoding_util.Varint false}};
      ];
  }}) = List.nth ocaml_types 1);
  () 

let () = 
  (** This tests verifies that a field which type is defined in another 
      file will have the appropriate type name (ie prefix with the module
      name corresponding to that file). 
   *)

  (* Fist type defined in a.proto *) 

  let s = "
  message M1 {
    required uint32 field_1 = 1;
  }
  "
  in 
  let pbtt_types, ocaml_types = compile_to_ocaml s in 
  assert(1 = List.length ocaml_types); 
  assert(1 = List.length pbtt_types); 
  
  (* Second type defined in b.proto *) 
  
  let s = "
  message M2 {
    required M1 field_m1= 1;
  }
  " in
  let pbtt_types, ocaml_types = compile_to_ocaml ~all_pbtt_types:pbtt_types ~file_name:"b.proto" s in 
  
  assert(2 = List.length ocaml_types); 
  assert(2 = List.length pbtt_types); 

  (* Compilation successfull now let's check that the field_m1 in M2 as the correct 
     type name A.m1 
   *)

  let fields_of_m2 = List.fold_left (fun acc {Ocaml_types.spec; _ } -> 
    match spec with
    | Ocaml_types.Record { Ocaml_types.record_name = "m2"; fields; } -> Some fields 
    | _ -> acc 
  ) None ocaml_types in 

  begin 
    match fields_of_m2 with
    | Some ({Ocaml_types.field_type = (User_defined_type name) ; _ }::[])  -> ( 
        print_endline name;
        assert("A.m1" = name)
    )
    | _ -> assert(false) 
    end;
  () 


let () = 
  print_endline "Backend OCaml Tests ... Ok"
