
(* TODO Resurect or Delete *) 


(* 
 
let file_options = []

let file_name = "a.proto"

let parse f s  = 
  f Pblexer.lexer (Lexing.from_string s)

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
  assert ("t_est" = Backend_ocaml.type_name message_scope oneof_name);
  ()
 
let () = 
  let message_scope = [] in 
  let oneof_name = "tEST_Max" in 
  assert ("t_est_max" = Backend_ocaml.type_name message_scope oneof_name);
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
  assert("Test_blah" = Backend_ocaml.constructor_name s);
  ()
  (* TODO this could really be improved to be Test_blah *)
 
 
 
(** TODO: Add test cases for when the field_message_scope is NOT empty
 *)
module BO = Backend_ocaml

let compile_to_ocaml ?all_pbtt_types:(all_pbtt_types = []) ?file_name:(file_name = file_name) s = 
  let ast = parse Pbparser.message_ s in 
  let all_pbtt_types' = Pbtt_util.compile_message_p1 file_name file_options Pbtt_util.empty_scope ast in 
  let all_pbtt_types' = all_pbtt_types' @ all_pbtt_types in 
  let all_pbtt_types'' = List.map (fun t -> 
    Pbtt_util.compile_proto_p2 all_pbtt_types' t
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
    module_ = "A_pb";
    spec    = Record {
    record_name = "m"; 
    fields = [
      {mutable_ = false; field_type = Int32; field_name = "v1"; type_qualifier = No_qualifier;
      encoding = Regular_field { packed = false; default = None; field_number = 1; nested = false;  payload_kind = Varint false}};
      {mutable_ = false; field_type = String; field_name = "v2"; type_qualifier = No_qualifier;
      encoding = Regular_field { packed = false; default = None; field_number = 2; nested = false; payload_kind = Bytes}};
      {mutable_ = false; field_type = Bool; field_name = "v3"; type_qualifier = Option; 
      encoding = Regular_field { packed = false; default = None; field_number = 3; nested = false; payload_kind = Varint false}};
      {mutable_ = false; field_type = Float; field_name = "v4"; type_qualifier = Option;
      encoding = Regular_field { packed = false; default = None; field_number = 4; nested = false; payload_kind = Bits32}};
      {mutable_ = false; field_type = Float; field_name = "v5"; type_qualifier = Option;
      encoding = Regular_field { packed = false; default = None; field_number = 5; nested = false; payload_kind = Bits64}};
      {mutable_ = false; field_type = Bytes; field_name = "v6"; type_qualifier = No_qualifier;
      encoding = Regular_field { packed = false; default = None; field_number = 6; nested = false; payload_kind = Bytes}};
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
      module_ = "A_pb"; 
      spec    = Record {
      record_name = "m1_m2"; 
      fields = [
        {mutable_ = false; field_type = Int32; field_name = "m21"; type_qualifier = No_qualifier;
         encoding = Regular_field { packed = false; default = None; field_number = 1; nested = false; payload_kind = Varint false}};
      ];
    }}) = List.nth ocaml_types 0);
  assert(
    Ocaml_types.({
      module_ = "A_pb";
      spec    = Record {
      record_name = "m1"; 
      fields = [
        {mutable_ = false; field_type = Int32; field_name = "m11"; type_qualifier = No_qualifier;
         encoding = Regular_field { packed = false; default = None; field_number = 1; nested = false; payload_kind = Varint false}};
         {mutable_ = false; field_type = User_defined_type {type_name = "m1_m2"; module_ = None; } ; field_name = "sub"; type_qualifier = No_qualifier;
        encoding = Regular_field { packed = false; default = None; field_number = 2; nested = true; payload_kind = Bytes}};
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
      variant_constructors = [
        {mutable_ = false; field_type = Int32; field_name = "Intv"; type_qualifier = No_qualifier;
        encoding = { packed = false; default = None; field_number = 1; nested = false; payload_kind = Varint false}};
        {mutable_ = false; field_type = String; field_name = "Stringv"; type_qualifier = No_qualifier;
         encoding = { packed = false; default = None; field_number = 2; nested = false; payload_kind = Bytes}};
      ];
      variant_encoding = Inlined_within_message;
    }) in
  assert(Ocaml_types.({module_ = "A_pb"; spec = Variant variant}) = List.nth ocaml_types 0);
  assert(
    Ocaml_types.({
      module_ = "A_pb"; 
      spec    = Record {
      record_name = "m1"; 
      fields = [
        {mutable_ = false; field_type = User_defined_type {type_name = "m1_o1"; module_ = None; } ; field_name = "o1"; type_qualifier = No_qualifier;
        encoding = One_of variant};
        {mutable_ = false; field_type = Int32; field_name = "v1"; type_qualifier = No_qualifier;
         encoding = Regular_field { packed = false; default = None; field_number = 3; nested = false; payload_kind = Varint false}};
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
    | Some ({Ocaml_types.field_type = (Ocaml_types.User_defined_type {Ocaml_types.type_name; Ocaml_types.module_ }) ; _ }::[])  -> ( 
        assert("m1" = type_name);
        assert(Some "A_pb" = module_)
    )
    | _ -> assert(false) 
    end;
  () 

(* Test for mutable field option *)

let () = 
  
  let s = "
  message m {
    required int32 f1 = 1 [(ocaml_mutable) = true];
  }
  " in

  let pbtt_types, ocaml_types = compile_to_ocaml ~all_pbtt_types:[] ~file_name:"b.proto" s in 
  
  assert(1 = List.length ocaml_types); 
  assert(1 = List.length pbtt_types); 

  let open Ocaml_types in

  begin
    match ocaml_types with 
    | {spec = Record {record_name; fields;} } :: [] -> (
      match fields with 
      | {field_name; mutable_; _ } :: [] -> assert(mutable_)
      | _                                -> assert(false)
    )
    | _ -> assert(false)
  end;
  () 

let () = 
  assert (["blah"] = Backend_ocaml.rev_split_by_naming_convention "blah"); 
  assert (["foo";"bar"] = Backend_ocaml.rev_split_by_naming_convention "bar_foo");
  assert (["Foo";"bar"] = Backend_ocaml.rev_split_by_naming_convention "barFoo");
  assert (["F"] = Backend_ocaml.rev_split_by_naming_convention "F");
  assert (["BF"] = Backend_ocaml.rev_split_by_naming_convention "BF");
  assert (["BAR"; "foo"] = Backend_ocaml.rev_split_by_naming_convention "fooBAR");
  assert (["BAr"; "foo"] = Backend_ocaml.rev_split_by_naming_convention "fooBAr");
  assert (["BAR"; "BAr"; "Bar"; "foo"] = Backend_ocaml.rev_split_by_naming_convention "fooBarBArBAR");
  assert (["BAR"; "BAr"; "Bar"; "foo"] = Backend_ocaml.rev_split_by_naming_convention "fooBarBArBAR");
  ()

let () = 
  print_endline "Backend OCaml Tests ... Ok"

*)
