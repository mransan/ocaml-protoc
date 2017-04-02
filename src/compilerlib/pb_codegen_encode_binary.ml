module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting 
module L = Pb_logger

let file_suffix = Pb_codegen_decode_binary.file_suffix

let constructor_name s = 
  (String.capitalize @@ String.lowercase s) [@@ocaml.warning "-3"] 

let sp = Pb_codegen_util.sp

let gen_encode_field_key sc number pk is_packed = 
  F.linep sc "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
    number (constructor_name @@ Pb_codegen_util.string_of_payload_kind pk is_packed)
  
let encode_basic_type bt pk = 
  Pb_codegen_util.runtime_function (`Encode, pk, bt) 

let gen_encode_field_type 
      ?with_key sc var_name encoding_number pk is_packed field_type = 

  let encode_key sc = 
    match with_key with
    | Some () -> gen_encode_field_key sc encoding_number pk is_packed 
    | None -> ()
  in 

  match field_type with 
  | Ot.Ft_user_defined_type udt ->
    encode_key sc;
    let f_name = 
      let function_prefix = "encode" in 
      let module_suffix = file_suffix in 
      Pb_codegen_util.function_name_of_user_defined 
        ~function_prefix ~module_suffix udt
    in 
    begin match udt.Ot.udt_type with
    | `Message -> 
      F.linep sc "Pbrt.Encoder.nested (%s %s) encoder;" f_name var_name
    | `Enum ->
      F.linep sc "%s %s encoder;" f_name var_name
    end
  
  | Ot.Ft_unit ->
    encode_key sc;
    F.line sc "Pbrt.Encoder.empty_nested encoder;" 
  
  | Ot.Ft_basic_type bt ->
    encode_key sc;
    let rt = encode_basic_type bt pk in 
    F.linep sc "%s %s encoder;" rt var_name


let gen_rft_nolabel sc var_name (field_type, encoding_number, pk) = 
  gen_encode_field_type ~with_key:() sc var_name encoding_number pk 
    false (* packed *) field_type

let gen_rft_required sc var_name (field_type, encoding_number, pk, _) = 
  gen_encode_field_type ~with_key:() sc var_name encoding_number pk 
    false (* packed *) field_type

let gen_rft_optional sc var_name (field_type, encoding_number, pk, _) = 
  F.linep sc "begin match %s with" var_name;
  F.linep sc "| Some x -> ";
  F.scope sc (fun sc ->
    gen_encode_field_type 
      ~with_key:() sc "x" encoding_number pk false field_type;
  ); 
  F.line sc "| None -> ();";
  F.line sc "end;"

let gen_rft_repeated sc var_name repeated_field = 
  let (rt, field_type, encoding_number, pk, is_packed) = repeated_field in 

  match rt, is_packed with
  | Ot.Rt_list, false -> (
    F.line sc "List.iter (fun x -> ";
    F.scope sc (fun sc -> 
      gen_encode_field_type 
        ~with_key:() sc "x" encoding_number pk is_packed field_type;
    );
    F.linep sc ") %s;" var_name;
  ) 

  | Ot.Rt_repeated_field, false -> ( 
    F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
    F.scope sc (fun sc -> 
      gen_encode_field_type 
        ~with_key:() sc "x" encoding_number pk is_packed field_type;
    );
    F.linep sc ") %s;" var_name;
  ) 

  | Ot.Rt_list, true -> (
    gen_encode_field_key sc encoding_number pk is_packed;
      (* When packed the key is encoded once. 
       *)
    F.line sc "Pbrt.Encoder.nested (fun encoder ->";
    F.scope sc (fun sc -> 
      F.line sc "List.iter (fun x -> ";
      F.scope sc (fun sc -> 
        gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
      );
      F.linep sc ") %s;" var_name;
    );
    F.line sc ") encoder;";
  ) 

  | Ot.Rt_repeated_field, true -> (
    gen_encode_field_key sc encoding_number pk is_packed;
      (* When packed the key is encoded once. 
       *)
    F.line sc "Pbrt.Encoder.nested (fun encoder ->";
    F.scope sc (fun sc -> 
      F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
      F.scope sc (fun sc -> 
        gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
      );
      F.linep sc ") %s;" var_name; 
    );
    F.line sc") encoder;";
  ) 

let gen_rft_variant sc module_ var_name {Ot.v_constructors; _} =

  F.linep sc "begin match %s with" var_name;
  List.iter (fun constructor  -> 

    let {
      Ot.vc_constructor; 
      vc_field_type; 
      vc_encoding_number; 
      vc_payload_kind} = constructor
    in 

    begin match vc_field_type with
    | Ot.Vct_nullary -> ( 
      F.linep sc "| %s_types.%s ->" module_ vc_constructor; 
      F.scope sc (fun sc -> 
        gen_encode_field_key sc vc_encoding_number vc_payload_kind false;
        F.line sc "Pbrt.Encoder.empty_nested encoder";
      );
    )
    | Ot.Vct_non_nullary_constructor field_type -> (
      F.linep sc "| %s_types.%s x ->" module_ vc_constructor;
      F.scope sc (fun sc -> 
        gen_encode_field_type 
          sc ~with_key:() "x" vc_encoding_number vc_payload_kind 
          false field_type  
      ); 
    )
    end;
  ) v_constructors;
  F.line sc "end;"

let gen_rft_associative sc var_name associative_field = 
  let (
    at, 
    encoding_number, 
    (key_type, key_pk), 
    (value_type, value_pk)
  ) = associative_field in

  F.linep sc "let encode_key = %s in" (encode_basic_type key_type key_pk);
  F.line sc "let encode_value = (fun x encoder ->";
  F.scope sc (fun sc -> 
    gen_encode_field_type sc "x" (-1 (* TODO *)) value_pk false value_type;
  );
  F.line sc ") in";

  begin match at with
  | Ot.At_list -> (
     F.line sc "List.iter (fun (k, v) ->"; 
  )
  | Ot.At_hashtable -> (
    F.line sc "Hashtbl.iter (fun k v ->";
  )
  end; 
  F.scope sc (fun sc ->
    gen_encode_field_key sc encoding_number Ot.Pk_bytes false;
    F.linep sc "let map_entry = (k, Pbrt.%s), (v, Pbrt.%s) in"
      (Pb_codegen_util.string_of_payload_kind ~capitalize:() key_pk false) 
      (Pb_codegen_util.string_of_payload_kind ~capitalize:() value_pk false); 
    F.line sc 
        ("Pbrt.Encoder.map_entry ~encode_key " ^ 
         "~encode_value map_entry encoder")
  );
  F.linep sc ") %s;" var_name

let gen_record ?and_ module_ {Ot.r_name; r_fields } sc = 
  let rn = r_name in 
  F.linep sc "%s encode_%s (v:%s_types.%s) encoder = " 
      (Pb_codegen_util.let_decl_of_and and_) rn module_ rn;

  F.scope sc (fun sc -> 
    List.iter (fun record_field -> 
      let {Ot.rf_label; rf_field_type; _ } = record_field in  

      let var_name = sp "v.%s_types.%s" module_ rf_label in  
      match rf_field_type with 
      | Ot.Rft_nolabel x -> 
        gen_rft_nolabel sc var_name x 

      | Ot.Rft_required x ->
        gen_rft_required sc var_name x 

      | Ot.Rft_optional x -> 
        gen_rft_optional sc var_name x 

      | Ot.Rft_repeated x ->
        gen_rft_repeated sc var_name x 

      | Ot.Rft_variant x -> 
        gen_rft_variant sc module_ var_name x 

      | Ot.Rft_associative x -> 
        gen_rft_associative sc var_name x 
      
    ) r_fields (* List.iter *); 
    F.line sc "()";
  ) (* encode function *)

let gen_variant ?and_ module_ variant sc = 
  let {Ot.v_name; Ot.v_constructors} = variant in 
  let vn = v_name in  
  F.linep sc "%s encode_%s (v:%s_types.%s) encoder = " 
      (Pb_codegen_util.let_decl_of_and and_) vn module_ vn;
  F.scope sc (fun sc -> 
    F.line sc "begin match v with";
    List.iter (fun variant_constructor -> 
      let {
        Ot.vc_constructor; 
        vc_field_type; 
        vc_encoding_number; 
        vc_payload_kind} = variant_constructor in 
      begin match vc_field_type with
      | Ot.Vct_nullary -> ( 
        F.linep sc "| %s_types.%s ->" module_ vc_constructor; 
        F.scope sc (fun sc -> 
          gen_encode_field_key sc vc_encoding_number vc_payload_kind false;
          F.line sc "Pbrt.Encoder.empty_nested encoder";
        );
      )
      | Ot.Vct_non_nullary_constructor field_type -> (
        F.linep sc "| %s_types.%s x ->" module_ vc_constructor;
        F.scope sc (fun sc -> 
          gen_encode_field_type 
              sc ~with_key:() "x" 
              vc_encoding_number vc_payload_kind false field_type  
        ); 
      )
      end;
    ) v_constructors;
    F.line sc "end" 
  )

let gen_const_variant ?and_ module_ cv sc = 

  let {Ot.cv_name; cv_constructors} = cv in 

  F.linep sc "%s encode_%s (v:%s_types.%s) encoder =" 
      (Pb_codegen_util.let_decl_of_and and_) cv_name module_ cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, value) -> 
      F.line sc (
        if value > 0 
        then 
          sp "| %s_types.%s -> Pbrt.Encoder.int_as_varint %i encoder" 
            module_ name value
        else 
          sp "| %s_types.%s -> Pbrt.Encoder.int_as_varint (%i) encoder" 
            module_ name value
      )
    ) cv_constructors; 
  )

let gen_struct ?and_ t sc = 
  let {Ot.module_; spec; _} = t in
  let has_encoded = 
    match spec with 
    | Ot.Record r  -> gen_record  ?and_ module_ r sc; true
    | Ot.Variant v -> gen_variant ?and_ module_ v sc; true 
    | Ot.Const_variant v ->
      gen_const_variant ?and_ module_ v sc; true
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let _ = and_ in
  let {Ot.module_; spec; _} = t in 
  let f type_name = 
    F.linep sc "val encode_%s : %s_types.%s -> Pbrt.Encoder.t -> unit"
      type_name module_ type_name;
    F.linep sc 
      "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" 
      type_name; 
  in 
  let has_encoded = 
    match spec with 
    | Ot.Record {Ot.r_name; _ } -> f r_name; true
    | Ot.Variant v -> f v.Ot.v_name; true 
    | Ot.Const_variant {Ot.cv_name; _ } -> f cv_name; true
  in
  has_encoded

let ocamldoc_title = "Protobuf Encoding"
