module Ot = Pb_codegen_ocaml_type 
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

(* Create the string for the exception raised expression in the case
   a JSON value type is not matching the protobuf definition. (For instance
   getting a JSON number when the protobuf field type is a string) *)
let unpexected_payload r_name rf_label = 
  sp "(Pbrt_js.unexpected_json_type \"%s\" \"%s\")"  r_name rf_label 

(* Function which returns all the possible pattern match for reading a JSON 
   value into an OCaml value. The protobuf JSON encoding rules 
   are defined here:
   https://developers.google.com/protocol-buffers/docs/proto3#json *)
let field_pattern_matches ~r_name ~rf_label field_type = 
   
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
    let {Ot.udt_nested; _} = udt in 
    let f_name = Pb_codegen_util.function_name_of_user_defined "decode" udt in 
    if udt_nested
    then 
      let value_expression = "(" ^ f_name ^ " o)" in
      ("Decoder.Object o", value_expression)::
      ("_", unpexected_payload r_name rf_label)::[]
      (* TODO: This will not work for user defined types from other 
         modules *)
    else 
      let value_expression = "(" ^ f_name ^ " json_value)" in
      ("json_value", value_expression):: []



(* Generate all the pattern matches for a record field *)
let gen_rft_nolabel sc ~r_name ~rf_label (field_type, _, _) = 
   
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in 
  
  let pattern_matches = field_pattern_matches ~r_name ~rf_label field_type in

  List.iter (fun (json_type, value_expression) ->
    F.line sc @@ sp "| Some (\"%s\", %s) -> " json_label json_type; 
    F.line sc @@ sp "  v.%s <- %s" rf_label value_expression
  ) pattern_matches 

(* Generate all the pattern matches for a repeated field *)
let gen_rft_repeated_field sc ~r_name ~rf_label repeated_field =
  let (_, field_type, _, _, _) = repeated_field in

  let json_label = Pb_codegen_util.camel_case_of_label rf_label in 

  F.line sc @@ sp "| Some (\"%s\", Decoder.Array_as_array a) -> begin" 
               json_label;

  F.scope sc (fun sc -> 
    F.line sc @@ sp "v.%s <- Array.map (function" rf_label;

    let pattern_matches = field_pattern_matches ~r_name ~rf_label field_type in

    List.iter (fun (json_type, value_expression) -> 
      F.line sc @@ sp "  | %s -> %s" json_type value_expression;
    ) pattern_matches; 
    F.line sc ") a |> Array.to_list;";
  ); 

  F.line sc "end"

(* Generate pattern match for a variant field *)
let gen_rft_variant_field sc ~r_name ~rf_label {Ot.v_constructors; _} = 

  List.iter (fun {Ot.vc_constructor; vc_field_type; _} -> 

    let json_label = 
      Pb_codegen_util.camel_case_of_constructor vc_constructor 
    in

    match vc_field_type with
    | Ot.Vct_nullary -> begin 
      F.line sc @@ sp "| Some (\"%s\", _) -> v.%s <- %s"
                   json_label rf_label vc_constructor
    end

    | Ot.Vct_non_nullary_constructor field_type ->
      let pattern_matches = 
        field_pattern_matches ~r_name ~rf_label field_type
      in

      List.iter (fun (json_type, value_expression) -> 
        F.line sc @@ sp "| Some (\"%s\", %s) -> "
          json_label json_type;
        F.line sc @@ sp "  v.%s <- %s (%s)" 
          rf_label vc_constructor value_expression;
      ) pattern_matches 
    
  ) v_constructors

(* Generate decode function for a record *)
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
      
      (* Termination condition *)
      F.line sc "| None -> continue := false ";

      (* Generate pattern match for all the possible message field *)
      List.iter (fun {Ot.rf_label; rf_field_type; _ } -> 

        match rf_field_type with
        | Ot.Rft_nolabel nolabel_field  ->
          gen_rft_nolabel sc ~r_name ~rf_label nolabel_field

        | Ot.Rft_repeated_field repeated_field -> 
          gen_rft_repeated_field sc ~r_name ~rf_label repeated_field

        | Ot.Rft_variant_field variant_field -> 
          gen_rft_variant_field sc ~r_name ~rf_label variant_field

        | _ -> assert(false)
      ) r_fields;

      (* Unknown fields are simply ignored *)
      F.empty_line sc; 
      F.line sc "| Some (_, _) -> () (*Unknown fields are ignored*)";
    ); 
    F.line sc "done;"; 
    
    (* Transform the mutable record in an immutable one *)
    F.line sc "({"; 
    F.scope sc (fun sc -> 
      List.iter (fun {Ot.rf_label;_} -> 
        F.line sc @@ sp "%s = v.%s;" rf_label rf_label; 
      ) r_fields;
    ); 
    F.line sc @@ sp "} : %s)" r_name;
  )

(* Generate decode function for a variant type *)
let gen_decode_variant ?and_ {Ot.v_name; v_constructors} sc = 

  (* helper function for each constructor case *)
  let process_v_constructor sc {Ot.vc_constructor; vc_field_type; _} = 

    let json_label = Pb_codegen_util.camel_case_of_constructor vc_constructor in

    match vc_field_type with
    | Ot.Vct_nullary -> 
      F.line sc @@ sp "| Some (\"%s\", _) -> %s" json_label vc_constructor 

    | Ot.Vct_non_nullary_constructor field_type ->
      let pattern_matches = 
        let r_name = v_name and rf_label = vc_constructor in 
        field_pattern_matches ~r_name ~rf_label field_type
      in

      List.iter (fun (json_type, value_expression) -> 
        F.line sc @@ sp "| Some (\"%s\", %s) -> "
          json_label json_type;
        F.line sc @@ sp "  %s (%s)" vc_constructor value_expression;
      ) pattern_matches
  in

  F.line sc @@ 
    sp "%s decode_%s d =" (Pb_codegen_util.let_decl_of_and and_) v_name; 

  F.scope sc (fun sc -> 
    (* even though a variant should be an object with a single field, 
     * it is possible other fields are present in the JSON object. Therefore
     * we still need a loop to iterate over the key/value, even if in 99.99% 
     * of the cases it will be a single iteration *)
    F.line sc "let rec loop () ="; 
    F.scope sc (fun sc -> 
      F.line sc "match Decoder.key d with";

      (* termination condition *)
      F.line sc @@ sp "| None -> Pbrt.Decoder.malformed_variant \"%s\"" 
        v_name; 
        
      List.iter (process_v_constructor sc) v_constructors; 

      F.empty_line sc; 
      F.line sc "| Some (_, _) -> loop ()";
    );
    F.line sc "in"; 
    F.line sc "loop ()";
  ) 

let gen_decode_const_variant ?and_ {Ot.cv_name; cv_constructors} sc = 
  F.line sc @@ sp "%s decode_%s (value:Decoder.value) =" 
    (Pb_codegen_util.let_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match value with"; 
    List.iter (fun (constructor, _) -> 
      F.line sc @@ sp "| Decoder.String \"%s\" -> %s"
        (String.uppercase constructor) constructor
    ) cv_constructors;  
    F.line sc @@ sp "| _ -> Pbrt.Decoder.malformed_variant \"%s\"" cv_name;  
  ) 

let gen_struct ?and_ t sc = 
  let (), has_encoded =  match t with 
    | {Ot.spec = Ot.Record r; _ }  -> 
      gen_decode_record ?and_ r sc, true
    | {Ot.spec = Ot.Variant v; _ } -> 
      gen_decode_variant ?and_ v sc, true
    | {Ot.spec = Ot.Const_variant v; _ } -> 
      gen_decode_const_variant ?and_ v sc, true
  in
  has_encoded

let gen_sig ?and_ t sc = 
  let _ = and_ in

  let f type_name = 
    F.line sc @@ sp "val decode_%s : Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp ("(** [decode_%s decoder] decodes a " ^^ 
                     "[%s] value from [decoder] *)") type_name type_name; 
  in 

  match t with 
  | {Ot.spec = Ot.Record {Ot.r_name; _ }; _} -> f r_name; true
  | {Ot.spec = Ot.Variant {Ot.v_name; _ }; _ } -> f v_name; true 
  | {Ot.spec = Ot.Const_variant {Ot.cv_name; _ }; _ } -> 
    F.line sc @@ sp "val decode_%s : Decoder.value -> %s" cv_name cv_name ; 
    F.line sc @@ sp "(** [decode_%s value] decodes a [%s] from a Json value*)"
      cv_name cv_name;
    true

let ocamldoc_title = "JSON Decoding"
