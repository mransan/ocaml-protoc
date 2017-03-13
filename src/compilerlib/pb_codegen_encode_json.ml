module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting 

let sp = Pb_codegen_util.sp

let gen_rft_nolabel sc rf_label pk field_type = 
  let var_name = sp "v.%s" rf_label in 
  let json_label = Pb_codegen_util.caml_case_of_label rf_label in  
  match field_type, pk with
  | Ot.Ft_unit, _ -> 
    F.line sc "(* unit type -> encode nothing *)" 

  | Ot.Ft_basic_type Ot.Bt_string, _ ->
    F.line sc @@ sp "Encoder.set_string encoder \"%s\" %s;" 
                 json_label var_name

  | Ot.Ft_basic_type Ot.Bt_int32, Ot.Pk_varint _ ->
    (* TODO: this could be more than 51 bit !! *)
    F.line sc @@ sp "Encoder.set_int encoder \"%s\" (Int32.to_int %s);"
                 json_label var_name 

  | Ot.Ft_basic_type Ot.Bt_bool, Ot.Pk_varint _ ->
    F.line sc @@ sp "Encoder.set_bool encoder \"%s\" %s;"
                 json_label var_name 

  | Ot.Ft_user_defined_type udt, Ot.Pk_bytes -> 
    F.line sc @@ sp "begin (* %s field *)" rf_label;
    F.scope sc (fun sc -> 
      F.line sc "let encoder' = Encoder.empty () in"; 
      F.line sc @@ sp "%s %s encoder';"
                   (Pb_codegen_util.function_name_of_user_defined "encode" udt)
                   var_name;
      F.line sc @@ sp "Encoder.set_object encoder \"%s\" encoder';" json_label;
    ); 
    F.line sc "end;";
    ()

let gen_encode_record ?and_ {Ot.r_name; r_fields } sc = 
  let rn = r_name in 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " 
      (Pb_codegen_util.let_decl_of_and and_) rn rn;
  F.scope sc (fun sc -> 
    List.iter (fun record_field -> 
      let {Ot.rf_label; rf_field_type; _ } = record_field in  

      match rf_field_type with 
      | Ot.Rft_nolabel (field_type, _, pk) ->
        gen_rft_nolabel sc rf_label pk field_type 
        
    ) r_fields (* List.iter *); 
    F.line sc "()"
  )

let gen_struct ?and_ t sc  = 
  let (), has_encoded = 
    match t with 
    | {Ot.spec = Ot.Record r; _ } -> gen_encode_record  ?and_ r sc, true
(*    | {Ot.spec = Ot.Variant v; _ } -> gen_encode_variant ?and_ v sc, true 
    | {Ot.spec = Ot.Const_variant v; _ } ->
      gen_encode_const_variant ?and_ v sc, true*)
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let _ = and_ in
  let f type_name = 
    F.line sc @@ sp "val encode_%s : %s -> Encoder.t -> unit" 
                 type_name type_name;
    F.line sc @@ sp ("(** [encode_%s v encoder] encodes [v] with the " ^^ 
                     "given [encoder] *)") type_name; 
  in 
  let (), has_encoded = 
    match t with 
    | {Ot.spec = Ot.Record {Ot.r_name; _ }; _}-> f r_name, true
(*    | {Ot.spec = Ot.Variant v; _ } -> f v.Ot.v_name, true 
    | {Ot.spec = Ot.Const_variant {Ot.cv_name; _ }; _ } -> f cv_name, true*)
  in
  has_encoded

let ocamldoc_title = "Protobuf JSON Encoding"
