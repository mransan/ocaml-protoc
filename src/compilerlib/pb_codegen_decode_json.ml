module Ot = Pb_codegen_ocaml_type 
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

(* Create the string for the exception raised expression in the case
   a JSON value type is not matching the protobuf definition. (For instance
   getting a JSON number when the protobuf field type is a string) *)
let unpexected_payload r_name rf_label = 
  sp "(Pbrt_js.unexpected_json_type \"%s\" \"%s\")"  r_name rf_label 

(* Function which returns all the possible cases for reading a JSON 
   value into an OCaml value. The protobuf JSON encoding rules 
   are defined here:
   https://developers.google.com/protocol-buffers/docs/proto3#json *)
let field_decode_cases ~r_name ~rf_label field_type = 
  let basic_type helper_fun = 
    (
      "json_value", 
      sp "Helper.%s json_value \"%s\" \"%s\"" helper_fun r_name rf_label
    ) :: []
  in
  match field_type with 
  | Ot.Ft_basic_type Ot.Bt_string -> basic_type "string"
  | Ot.Ft_basic_type Ot.Bt_float -> basic_type "float"
  | Ot.Ft_basic_type Ot.Bt_int -> basic_type "int" 
  | Ot.Ft_basic_type Ot.Bt_int32 -> basic_type "int32" 
  | Ot.Ft_basic_type Ot.Bt_int64 -> basic_type "int64" 
  | Ot.Ft_basic_type Ot.Bt_bool -> basic_type "bool" 
  | Ot.Ft_basic_type Ot.Bt_bytes -> basic_type "bytes" 
  | Ot.Ft_unit -> [] 
  | Ot.Ft_user_defined_type udt -> 
    let value_expression = 
      "(" ^ 
      Pb_codegen_util.function_name_of_user_defined "decode" udt ^
      " o)"
    in
    ("Decoder.Object o", value_expression)::
    ("_", unpexected_payload r_name rf_label)::[]
    (* TODO: This will not work for user defined types from other 
       modules *)

let gen_decode_repeated_field sc ~r_name ~rf_label repeated_field =
  let (repeated_type, field_type, _, _, _) = repeated_field in
  let json_label = Pb_codegen_util.caml_case_of_label rf_label in 
  let cases = field_decode_cases ~r_name ~rf_label field_type in 
  F.line sc @@ sp "| Some (\"%s\", Decoder.Array_as_array a) -> begin" 
               json_label;
  F.scope sc (fun sc -> 
    F.line sc @@ sp "v.%s <- Array.map (function" rf_label;
    List.iter (fun (json_type, value_expression) -> 
      F.line sc @@ sp "  | %s -> %s" json_type value_expression;
    ) cases; 
    F.line sc ") a |> Array.to_list;";
  ); 
  F.line sc "end"

let gen_decode_record ?and_  {Ot.r_name; r_fields} sc = 

  let mutable_record_name = Pb_codegen_util.mutable_record_name r_name in 

  F.line sc @@ 
    sp "%s decode_%s d =" (Pb_codegen_util.let_decl_of_and and_) r_name; 

  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" mutable_record_name;
    F.line sc @@ "let continue = ref true in"; 
    
    F.line sc "while !continue do"; 
    F.scope sc (fun sc -> 
      F.line sc "match Decoder.key d with";

      (* termination condition *)
      F.line sc "| None -> continue := false ";
      List.iter (fun {Ot.rf_label; rf_field_type; _ } -> 

        match rf_field_type with
        | Ot.Rft_nolabel (field_type, _, _) -> begin 
          let json_label = Pb_codegen_util.caml_case_of_label rf_label in 
          List.iter (fun (json_type, value_expression) ->
            F.line sc @@ sp "| Some (\"%s\", %s) -> " json_label json_type; 
            F.line sc @@ sp "  v.%s <- %s" rf_label value_expression
          ) (field_decode_cases ~r_name ~rf_label field_type);
        end 

        | Ot.Rft_repeated_field repeated_field -> 
          gen_decode_repeated_field sc ~r_name ~rf_label repeated_field

        | _ -> assert(false)
      ) r_fields;

      (* Extra unknown fields are simply ignored *)
      F.empty_line sc; 
      F.line sc "| Some (_, _) -> () (*Unknown fields are ignored*)";
    ); 
    F.line sc "done;"; 
    
    F.line sc "({"; 
    F.scope sc (fun sc -> 
      List.iter (fun {Ot.rf_label;_} -> 
        F.line sc @@ sp "%s = v.%s;" rf_label rf_label; 
      ) r_fields;
    ); 
    F.line sc @@ sp "} : %s)" r_name;
  )

let gen_struct ?and_ t sc = 
  let (), has_encoded =  match t with 
    | {Ot.spec = Ot.Record r; _ }  -> 
      gen_decode_record ?and_ r sc, true
    | _ -> assert(false) (* TODO *)
(*    | {Ot.spec = Ot.Variant v; _ } -> 
      gen_decode_variant ?and_ v sc, true
    | {Ot.spec = Ot.Const_variant v; _ } -> 
      gen_decode_const_variant ?and_ v sc, true*)
  in
  has_encoded

let gen_sig ?and_ t sc = 

  let _ = and_ in

  let f type_name = 
    F.line sc @@ sp "val decode_%s : Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp ("(** [decode_%s decoder] decodes a " ^^ 
                     "[%s] value from [decoder] *)") type_name type_name; 
  in 

  let (), has_encoded = 
    match t with 
    | {Ot.spec = Ot.Record {Ot.r_name; _ }; _} -> f r_name, true
    | _ -> assert(false) (* TODO *)
(*    | {Ot.spec = Ot.Variant {Ot.v_name; _ }; _ } -> f v_name, true 
    | {Ot.spec = Ot.Const_variant {Ot.cv_name; _ }; _ } -> f cv_name, true*)
  in
  has_encoded

let ocamldoc_title = "JSON Decoding"
