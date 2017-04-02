module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting 

let sp = Pb_codegen_util.sp

let file_suffix = Pb_codegen_decode_yojson.file_suffix 

let unsupported json_label = 
  failwith (sp "Unsupported field type for field: %s" json_label) 

let setter_of_basic_type json_label basic_type pk = 
  match basic_type, pk with
  (* String *)
  | Ot.Bt_string, _ ->
    ("make_string", None) 

  (* Float *)
  | Ot.Bt_float, Ot.Pk_bits32 ->
    ("make_float", None)
  | Ot.Bt_float, Ot.Pk_bits64 -> 
    ("make_string", Some "string_of_float") 

  (* Int32 *)
  | Ot.Bt_int32, Ot.Pk_varint _ 
  | Ot.Bt_int32, Ot.Pk_bits32 ->
    ("make_int", Some ("Int32.to_int"))
  
  (* Int64 *)
  | Ot.Bt_int64, Ot.Pk_varint _ 
  | Ot.Bt_int64, Ot.Pk_bits64 ->
    ("make_string", Some ("Int64.to_string"))
    (* 64 bit integer are always encoded as string since 
       only support up to 51 bits integer. An improvement
       could be to check for value > 2^51 and use int *)

  (* int *)
  | Ot.Bt_int, Ot.Pk_bits32 ->
    ("make_int", None)

  | Ot.Bt_int, Ot.Pk_varint _ 
  | Ot.Bt_int, Ot.Pk_bits64 ->
    ("make_string", Some ("string_of_int")) 

  (* bool *)
  | Ot.Bt_bool, Ot.Pk_varint _ ->
    ("make_bool", None)

  (* bytes *)
  | Ot.Bt_bytes, Ot.Pk_bytes -> unsupported json_label
  | _ -> unsupported json_label

let gen_field var_name json_label field_type pk = 

  match field_type, pk with
  | Ot.Ft_unit, _ -> 
    None

  (* Basic types *)
  | Ot.Ft_basic_type basic_type, _ ->
    let setter, map_function = setter_of_basic_type json_label basic_type pk in
    begin match map_function with
    | None -> 
      Some (sp "(\"%s\", Pbrt_json.%s %s)"
                   json_label setter var_name)
    | Some map_function ->
      Some (sp "(\"%s\", Pbrt_json.%s (%s %s))"
                   json_label setter map_function var_name)
    end
  
  (* User defined *)
  | Ot.Ft_user_defined_type udt, _ -> 
    let f_name = 
      let function_prefix = "encode" in 
      let module_suffix = file_suffix in 
      Pb_codegen_util.function_name_of_user_defined 
        ~function_prefix ~module_suffix udt 
    in
    Some (sp "(\"%s\", %s %s)" json_label f_name var_name)

let gen_rft_nolabel sc rf_label (field_type, _, pk) = 
  let var_name = sp "v.%s" rf_label in 
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in  
  match gen_field var_name json_label field_type pk with
  | None -> () 
  | Some exp -> 
     F.line sc @@ sp "let assoc = %s :: assoc in" exp 

let gen_rft_optional_field sc rf_label (field_type, _, pk, _) = 
  F.line sc @@ sp "let assoc = match v.%s with" rf_label; 
  F.scope sc (fun sc ->
    F.line sc "| None -> assoc";
    let json_label = Pb_codegen_util.camel_case_of_label rf_label in  
    match gen_field "v" json_label field_type pk with 
    | None -> 
      F.line sc "| Some v -> assoc";
    | Some exp -> 
      F.line sc @@ sp "| Some v -> %s :: assoc" exp;
  ); 
  F.line sc "in"

let gen_rft_repeated_field sc rf_label repeated_field = 
  let (repeated_type, field_type, _, pk, _) = repeated_field in
  begin match repeated_type with
  | Ot.Rt_list -> () 
  | Ot.Rt_repeated_field -> 
    (sp "Pbrt.Repeated_field is not supported with JSON (field: %s)" rf_label) 
    |> failwith    
  end; 

  let var_name = sp "v.%s" rf_label in 
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in  

  F.line sc "let assoc ="; 
  F.scope sc (fun sc ->

    begin match field_type, pk with
    | Ot.Ft_unit, _ -> 
      unsupported json_label

    | Ot.Ft_basic_type basic_type, _ ->
      let setter, map_function = 
        setter_of_basic_type json_label basic_type pk 
      in 
      begin match map_function with
      | None ->
        F.line sc @@ sp "let l = %s |> List.map Pbrt_json.%s in" var_name setter
      | Some map_function ->
        F.line sc @@ sp "let l = %s |> List.map %s |> List.map Pbrt_json.%s in " 
          var_name map_function setter
      end 
    
    (* User defined *)
    | Ot.Ft_user_defined_type udt, Ot.Pk_bytes -> 
      let f_name = 
        let function_prefix = "encode" in 
        let module_suffix = file_suffix in 
        Pb_codegen_util.function_name_of_user_defined 
          ~function_prefix ~module_suffix udt 
      in
      F.line sc @@ sp "let l = %s |> List.map %s in" 
          var_name f_name 

    | _ -> unsupported json_label
    end;
    F.line sc @@ sp "(\"%s\", `List l) :: assoc " json_label
  );
  F.line sc "in"
        
let gen_rft_variant_field sc rf_label {Ot.v_constructors; _} = 
  F.line sc @@ sp "let assoc = match v.%s with" rf_label;
  F.scope sc (fun sc -> 
    List.iter (fun {Ot.vc_constructor; vc_field_type; vc_payload_kind; _} ->
      let var_name = "v" in 
      let json_label = 
        Pb_codegen_util.camel_case_of_constructor vc_constructor 
      in  
      F.line sc @@ sp "| %s v ->" vc_constructor; 
      F.scope sc (fun sc ->  
        match vc_field_type with
        | Ot.Vct_nullary -> 
          F.line sc @@ sp "(\"%s\", `Null) :: assoc" json_label
        | Ot.Vct_non_nullary_constructor field_type -> 
          match gen_field var_name json_label field_type vc_payload_kind with
          | None -> 
            F.line sc @@ sp "(\"%s\", `Null) :: assoc" json_label
          | Some exp -> 
            F.line sc @@ sp "%s :: assoc " exp
       )
    ) v_constructors;
  ); 
  F.line sc @@ sp "in (* match v.%s *)" rf_label

let gen_encode_record ?and_ module_ {Ot.r_name; r_fields } sc = 
  let rn = r_name in 
  F.line sc @@ sp "%s encode_%s (v:%s_types.%s) = " 
      (Pb_codegen_util.let_decl_of_and and_) rn module_ rn;
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let open !%s_types in" module_;
    F.line sc "let assoc = [] in ";
    List.iter (fun record_field -> 
      let {Ot.rf_label; rf_field_type; _ } = record_field in  

      match rf_field_type with 
      | Ot.Rft_nolabel nolabel_field  ->
        gen_rft_nolabel sc rf_label nolabel_field
     
      | Ot.Rft_repeated_field repeated_field  -> 
        gen_rft_repeated_field sc rf_label repeated_field  

      | Ot.Rft_variant_field variant_field -> 
        gen_rft_variant_field sc rf_label variant_field 
      
      | Ot.Rft_optional optional_field ->
        gen_rft_optional_field sc rf_label optional_field 

      | Ot.Rft_required _ ->
        Printf.eprintf "Only proto3 syntax supported in JSON encoding";
        exit 1

      | Ot.Rft_associative_field _ -> 
        Printf.eprintf "Map field are not currently supported for JSON";
        exit 1
        
    ) r_fields (* List.iter *); 
    F.line sc "`Assoc assoc"
  )

let gen_encode_variant ?and_ {Ot.v_name; v_constructors} sc = 

  let process_v_constructor sc v_constructor = 
    let {
      Ot.vc_constructor; 
      Ot.vc_field_type; 
      Ot.vc_payload_kind; _} = v_constructor in 

    let json_label = Pb_codegen_util.camel_case_of_constructor vc_constructor in

    F.scope sc (fun sc -> 
      match vc_field_type with 
      | Ot.Vct_nullary -> 
        F.line sc @@ sp "| %s -> `Assoc [(\"%s\", `Null)]" vc_constructor json_label 

      | Ot.Vct_non_nullary_constructor field_type -> 
        match gen_field "v" json_label field_type vc_payload_kind with
        | None -> 
          F.line sc @@ sp "| %s -> `Assoc [(\"%s\", `Null)]" vc_constructor json_label 
        | Some exp -> 
          F.line sc @@ sp "| %s -> `Assoc [%s]" vc_constructor exp
    )
  in 

  F.line sc @@ sp "%s encode_%s (v:%s) = " 
      (Pb_codegen_util.let_decl_of_and and_) v_name v_name;
  F.scope sc (fun sc -> 
    F.line sc "begin match v with";
    List.iter (process_v_constructor sc) v_constructors;
    F.line sc "end";
  ) 

let gen_encode_const_variant ?and_ {Ot.cv_name; Ot.cv_constructors} sc = 
  F.line sc @@ sp "%s encode_%s (v:%s) = " 
      (Pb_codegen_util.let_decl_of_and and_) cv_name cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (constructor, _) -> 
      let json_value = String.uppercase constructor in 
      F.line sc @@ sp "| %s -> `String \"%s\"" constructor json_value
    ) cv_constructors
  ) 

let gen_struct ?and_ t sc  = 
  let (), has_encoded = 
    match t with 
    | {Ot.spec = Ot.Record r; module_; _ } -> 
      gen_encode_record  ?and_ module_ r sc, true
    | {Ot.spec = Ot.Variant v; _ } -> 
      gen_encode_variant ?and_ v sc, true 
    | {Ot.spec = Ot.Const_variant v; _ } ->
      gen_encode_const_variant ?and_ v sc, true
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let _ = and_ in
  let {Ot.module_ ; _ } = t in 
  let f type_name = 
    F.line sc @@ sp "val encode_%s : %s_types.%s -> Yojson.Basic.json" 
                 type_name module_ type_name;
    F.line sc @@ sp ("(** [encode_%s v encoder] encodes [v] to " ^^ 
                     "to json*)") type_name; 
  in 
  match t with 
  | {Ot.spec = Ot.Record {Ot.r_name; _ }; _}-> f r_name; true
  | {Ot.spec = Ot.Variant v; _ } -> f v.Ot.v_name; true 
  | {Ot.spec = Ot.Const_variant {Ot.cv_name; _ }; _ } -> f cv_name; true 

let ocamldoc_title = "Protobuf JSON Encoding"

