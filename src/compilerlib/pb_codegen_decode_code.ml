module Ot = Pb_codegen_ocaml_type 
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

let decode_basic_type bt pk = 
  Pb_codegen_util.runtime_function (`Decode, pk, bt)
  
let decode_field_f field_type pk = 
  match field_type with 
  | Ot.Ft_user_defined_type t -> 
    let f_name = Pb_codegen_util.function_name_of_user_defined "decode" t in
    if t.Ot.udt_nested 
    then (f_name ^ " (Pbrt.Decoder.nested d)")
    else (f_name ^ " d") 
  | Ot.Ft_unit -> 
      "Pbrt.Decoder.empty_nested d"
  | Ot.Ft_basic_type bt -> (decode_basic_type bt pk) ^ " d" 

let gen_decode_record ?and_ {Ot.r_name; r_fields} sc = 

  (* return the variable name used for keeping track if a required 
   * field has been set during decoding.  *)
  let is_set_variable_name rf_label = 
    sp "%s_is_set" rf_label 
  in

  let pbrt_payload_kind payload_kind is_packed = 
    if is_packed 
    then "Bytes"
    else 
      Pb_codegen_util.string_of_payload_kind ~capitalize:() payload_kind false 
  in  

  let process_field_common sc encoding_number payload_kind 
                           ?(is_packed = false) f = 
    F.line sc @@ sp "| Some (%i, Pbrt.%s) -> ("   
                    encoding_number (pbrt_payload_kind payload_kind is_packed);
    F.scope sc (fun sc -> 
      f sc;
    );
    F.line sc ")";
    F.line sc @@ sp "| Some (%i, pk) -> " encoding_number;
    F.line sc @@ sp "  Pbrt.Decoder.unexpected_payload \"%s\" pk"
     (sp "Message(%s), field(%i)" r_name encoding_number);
  in
  
  let process_nolabel_field sc rf_label (field_type, encoding_number, pk) = 
    process_field_common sc encoding_number pk (fun sc -> 
      F.line sc @@ sp "v.%s <- %s;" 
        rf_label (decode_field_f field_type pk); 
    ) 
  in

  let process_required_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number pk (fun sc -> 
      F.line sc @@ sp "v.%s <- %s; %s := true;" 
                      rf_label (decode_field_f field_type pk) 
                       (is_set_variable_name rf_label); 
    ) 
  in

  let process_optional_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number pk (fun sc -> 
      F.line sc @@ sp "v.%s <- Some (%s);" 
                      rf_label (decode_field_f field_type pk); 
    )
  in

  let process_repeated_field sc rf_label repeated_field = 
    let (rt, field_type, encoding_number, pk, is_packed) = repeated_field in
    match rt, is_packed with
    | Ot.Rt_list, false -> 
      process_field_common sc encoding_number pk ~is_packed (fun sc -> 
        F.line sc @@ sp "v.%s <- (%s) :: v.%s;" 
                         rf_label (decode_field_f field_type pk) rf_label; 
      ) 
    | Ot.Rt_repeated_field, false -> (
      process_field_common sc encoding_number pk ~is_packed (fun sc -> 
        F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s; " 
                        (decode_field_f field_type pk) rf_label; 
      ) 
    ) 
    | Ot.Rt_list, true -> (
      process_field_common sc encoding_number pk ~is_packed (fun sc -> 
        F.line sc @@ sp 
          "v.%s <- Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d;" 
          rf_label (decode_field_f field_type pk) 
      ) 
    ) 
    | Ot.Rt_repeated_field, true -> (
      process_field_common sc encoding_number pk ~is_packed (fun sc -> 
        F.line sc "Pbrt.Decoder.packed_fold (fun () d -> ";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s;" 
                          (decode_field_f field_type pk) rf_label;
        );
        F.line sc ") () d;";
      ) 
    ) 
  in

  let process_associative_field sc rf_label associative_field =   
  
    let (
      at, 
      encoding_number, 
      (key_type, key_pk), 
      (value_type, value_pk)) = associative_field in  

    let decode_key_f   = decode_basic_type key_type key_pk in 
      (* Because key can never be nested we can assign the decoding function 
       * directly rather wrapping up in a closure like for the value 
       * below
       *)
       (* TODO enhancement
        * For the value decoding function passed as an argument to 
        * [Pbrt.Decoder.map_entry] it's not always the case that it would 
        * require nesting. In the case it does not neeed a nested decoder 
        * we can avoid creating a closure and therefore improving 
        * the performance. 
        *)
    process_field_common sc encoding_number Ot.Pk_bytes (fun sc -> 
      F.line sc "let decode_value = (fun d ->";
      F.scope sc (fun sc ->
        F.line sc @@ decode_field_f value_type value_pk;
      ); 
      F.line sc ") in"; 
      let decode_expression = sp 
          "(Pbrt.Decoder.map_entry d ~decode_key:%s ~decode_value)" 
           decode_key_f 
      in

      begin match at with
      | Ot.At_list -> ( 
        F.line sc @@ sp "v.%s <- (" rf_label; 
        F.scope sc (fun sc -> 
          F.line sc @@ sp "%s::v.%s;"
            decode_expression rf_label
        ); 
        F.line sc ");" 
      )
      | Ot.At_hashtable -> (
        F.line sc @@ sp "let a, b = %s in" decode_expression;
        F.line sc @@ sp "Hashtbl.add v.%s a b;" rf_label;
      )
      end;
    )
  in

  let process_variant_field sc rf_label {Ot.v_constructors; _} = 
    List.iter (fun variant_constructor  -> 

      let {
        Ot.vc_constructor; 
        vc_field_type; 
        vc_encoding_number; 
        vc_payload_kind = pk; } = variant_constructor in 

      process_field_common sc vc_encoding_number pk (fun sc->  
        match vc_field_type  with
        | Ot.Vct_nullary -> (
          F.line sc "Pbrt.Decoder.empty_nested d;";
          F.line sc @@ sp "v.%s <- %s;" rf_label vc_constructor; 
        ) 
        | Ot.Vct_non_nullary_constructor field_type -> (
          F.line sc @@ sp "v.%s <- %s (%s);" 
            rf_label vc_constructor (decode_field_f field_type pk)
        )
      )
    ) v_constructors
  in 
  
  (* list fields have a special treatement when decoding since each new element
   * of a repeated field is appended to the front of the list. In order
   * to retreive the right order efficiently we reverse all the repeated field
   * lists values when the message is done being decoded.  *) 
  let all_lists = List.fold_left (fun acc {Ot.rf_label; rf_field_type; _ } -> 
    match rf_field_type with
    | Ot.Rft_repeated_field (Ot.Rt_list, _, _ , _, _ ) -> rf_label :: acc 
    | Ot.Rft_associative_field (Ot.At_list, _, _, _) -> rf_label :: acc
    | _ -> acc  
  ) [] r_fields in  

  let all_required_rf_labels = 
    List.fold_left (fun acc {Ot.rf_label; rf_field_type; _} -> 
      match rf_field_type with
      | Ot.Rft_required _ -> rf_label ::  acc 
      | _ -> acc 
    ) [] r_fields 
  in  

  let mutable_record_name = Pb_codegen_util.mutable_record_name r_name in 

  F.line sc @@ sp 
          "%s decode_%s d =" (Pb_codegen_util.let_decl_of_and and_) r_name; 
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" mutable_record_name;
    F.line sc "let continue__= ref true in";

    (* Add the is_set_<field_name> boolean variable which keeps track 
     * of whether a required field is set during the decoding.  *)
    List.iter (fun rf_label -> 
      F.line sc @@ sp "let %s = ref false in" (is_set_variable_name rf_label)
    ) all_required_rf_labels; 

    (* Decoding is done with recursively (tail - recursive). The 
     * function loop iterate over all fields returned by the Protobuf 
     * runtime. *)
    F.line sc "while !continue__ do";
    F.scope sc (fun sc -> 
      F.line sc "match Pbrt.Decoder.key d with";

      (* termination condition *)
      F.line sc "| None -> (";
      F.scope sc (fun sc -> 
        List.iter (fun field_name -> 
          F.line sc @@ sp "v.%s <- List.rev v.%s;" field_name field_name
        ) all_lists;   
      );
      F.line sc "); continue__ := false";

      (* compare the decoded field with the one defined in the 
       * .proto file. Unknown fields are ignored. *)
      List.iter (fun {Ot.rf_label; rf_field_type; _ } -> 
        match rf_field_type with
        | Ot.Rft_nolabel x -> process_nolabel_field sc rf_label x 
        | Ot.Rft_required x -> process_required_field sc rf_label x 
        | Ot.Rft_optional x -> process_optional_field sc rf_label x 
        | Ot.Rft_repeated_field x -> process_repeated_field sc rf_label x 
        | Ot.Rft_associative_field x-> process_associative_field sc rf_label x 
        | Ot.Rft_variant_field x -> process_variant_field sc rf_label x 
      ) r_fields; 
      F.line sc ("| Some (_, payload_kind) -> " ^ 
                 "Pbrt.Decoder.skip d payload_kind");
    ); 
    F.line sc "done;"; 

    (* Add the check to see if all required fields are set if not 
     * a Protobuf.Decoder.Failure exception is raised *) 
    List.iter (fun rf_label -> 
      F.line sc @@ sp 
        "begin if not !%s then Pbrt.Decoder.missing_field \"%s\" end;"
        (is_set_variable_name rf_label) rf_label
    ) all_required_rf_labels ; 

    F.line sc "({"; 
    F.scope sc (fun sc -> 
      List.iter (fun {Ot.rf_label;_} -> 
        F.line sc @@ sp "%s = v.%s;" rf_label rf_label; 
      ) r_fields;
    ); 
    F.line sc @@ sp "} : %s)" r_name;
  )

let gen_decode_variant ?and_ {Ot.v_name; v_constructors;} sc = 

  let process_ctor sc variant_constructor = 

    let {
      Ot.vc_constructor; 
      vc_field_type; 
      vc_encoding_number; 
      vc_payload_kind = pk;} = variant_constructor in  

    match vc_field_type with 
    | Ot.Vct_nullary -> (
      F.line sc @@ sp "| Some (%i, _) -> (Pbrt.Decoder.empty_nested d ; %s)" 
        vc_encoding_number vc_constructor
    ) 
    | Ot.Vct_non_nullary_constructor field_type -> 
      F.line sc @@ sp "| Some (%i, _) -> %s (%s)" 
        vc_encoding_number vc_constructor (decode_field_f field_type pk) 
  in 

  F.line sc @@ sp "%s decode_%s d = " 
    (Pb_codegen_util.let_decl_of_and and_) v_name;
  F.scope sc (fun sc ->
    F.line sc @@ sp "let rec loop () = "; 
    F.scope sc (fun sc ->
      F.line sc @@ sp "let ret:%s = match Pbrt.Decoder.key d with" v_name;
      F.scope sc (fun sc -> 
        F.line sc @@ sp "| None -> Pbrt.Decoder.malformed_variant \"%s\""
          v_name; 
        List.iter (fun ctor -> process_ctor sc ctor) v_constructors; 
        F.line sc "| Some (n, payload_kind) -> (";
        F.line sc "  Pbrt.Decoder.skip d payload_kind; ";
        F.line sc "  loop () ";
        F.line sc ")";
      );
      F.line sc "in";
      F.line sc "ret";
    ); 
    F.line sc "in"; 
    F.line sc "loop ()";
  )

let gen_decode_const_variant ?and_ {Ot.cv_name; cv_constructors; } sc = 

  F.line sc @@ sp "%s decode_%s d = " 
                  (Pb_codegen_util.let_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun (name, value) -> 
      F.line sc @@ sp "| %i -> (%s:%s)" value name cv_name
    ) cv_constructors; 
    F.line sc @@ sp "| _ -> Pbrt.Decoder.malformed_variant \"%s\"" cv_name
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
    F.line sc @@ sp "val decode_%s : Pbrt.Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp ("(** [decode_%s decoder] decodes a " ^^ 
                     "[%s] value from [decoder] *)") type_name type_name; 
  in 

  let (), has_encoded = 
    match t with 
    | {Ot.spec = Ot.Record {Ot.r_name; _ }; _} -> f r_name, true
    | {Ot.spec = Ot.Variant {Ot.v_name; _ }; _ } -> f v_name, true 
    | {Ot.spec = Ot.Const_variant {Ot.cv_name; _ }; _ } -> f cv_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Decoding"
