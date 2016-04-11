(* TODO Resurect or Delete *)

(* 

let () = 
  let r = Ocaml_types.({
    record_name = "test";
    fields = [ {
      mutable_ = false;
      field_type = Int; 
      field_name = "v1"; 
      type_qualifier = No_qualifier;
      encoding = Regular_field ({ 
        packed = false; default = None; field_number = 1; nested = false; payload_kind = Varint false}
      )
    }; {
      mutable_ = false;
      field_type = String; 
      field_name = "v2"; 
      type_qualifier = Option;
      encoding = Regular_field  ({ 
        packed = false; default = None; field_number = 2; nested = false; payload_kind = Bytes}
      )
    };{
      (* Make sure the mutable_ is respected in the OCaml 
         codegen
       *)
      mutable_ = true;
      field_type = User_defined_type {type_name = "other"; module_ = None; }; 
      field_name = "v3"; 
      type_qualifier = No_qualifier;
      encoding = Regular_field ({ 
        packed = false; default = None; field_number = 3; nested = true ; payload_kind = Bytes}
      )
    };];
  }) in

  let s = {|type test = {
  v1 : int;
  v2 : string option;
  mutable v3 : other;
}

and test_mutable = {
  mutable v1 : int;
  mutable v2 : string option;
  mutable v3 : other;
}|} in 

  let sc = Fmt.empty_scope () in 
  ignore @@ Codegen_type.gen_struct (Ocaml_types.{module_ = "A"; spec = Record r}) sc; 
  print_endline @@ Fmt.print sc;
  assert(s = (Fmt.print sc));
  ()

let () = 
  print_endline "OCaml Codegen Test ... Ok"

*)
