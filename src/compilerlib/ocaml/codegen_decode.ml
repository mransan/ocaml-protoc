module T   = Ocaml_types 
module E   = Exception 
module F   = Fmt

let sp = Codegen_util.sp

let decode_basic_type bt pk = 
  Backend_ocaml_static.runtime_function (`Decode, pk, bt)
  
let decode_field_f field_type pk = 
  match field_type with 
  | T.Ft_user_defined_type t -> 
    let f_name = Codegen_util.function_name_of_user_defined "decode" t in
    if t.T.udt_nested 
    then (f_name ^ " (Pbrt.Decoder.nested d)")
    else (f_name ^ " d") 
  | T.Ft_unit -> 
      "Pbrt.Decoder.empty_nested d"
  | T.Ft_basic_type bt -> (decode_basic_type bt pk) ^ " d" 

let gen_decode_record ?and_ {T.r_name; r_fields} sc = 

  let string_of_nonpacked_pk pk = 
    Codegen_util.string_of_payload_kind ~capitalize:() pk false 
  in 

  (* return the variable name used for keeping track if a required 
   * field has been set during decoding.  *)
  let is_set_variable_name rf_label = 
    sp "%s_is_set" rf_label 
  in

  let process_field_common sc encoding_number pk_as_string f = 
    F.line sc @@ sp "| Some (%i, Pbrt.%s) -> (" encoding_number pk_as_string; 
    F.scope sc (fun sc -> 
      f sc;
      F.line sc "loop ()";
    );
    F.line sc ")";
    F.line sc @@ sp "| Some (%i, pk) -> raise (" encoding_number;
    F.scope sc (fun sc ->
      F.line sc @@ sp 
        ("Protobuf.Decoder.Failure " ^^ 
         "(Protobuf.Decoder.Unexpected_payload (%s, pk))") 
        (sp "\"Message(%s), field(%i)\"" r_name encoding_number)
    );
    F.line sc ")"
  in
  
  let process_nolabel_field sc rf_label (field_type, encoding_number, pk) = 
    process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
      F.line sc @@ sp "v.%s <- %s;" 
        rf_label (decode_field_f field_type pk); 
    ) 
  in

  let process_required_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
      F.line sc @@ sp "v.%s <- %s; %s := true;" 
        rf_label (decode_field_f field_type pk) (is_set_variable_name rf_label); 
    ) 
  in

  let process_optional_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
      F.line sc @@ sp "v.%s <- Some (%s);" rf_label (decode_field_f field_type pk); 
    )
  in

  let process_repeated_field sc rf_label (rt, field_type, encoding_number, pk, is_packed) = 
    match rt, is_packed with
    | T.Rt_list, false -> 
      process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
        F.line sc @@ sp "v.%s <- (%s) :: v.%s;" rf_label (decode_field_f field_type pk) rf_label; 
      ) 
    | T.Rt_repeated_field, false -> (
      process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
        F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s; " (decode_field_f field_type pk) rf_label; 
      ) 
    ) 
    | T.Rt_list, true -> (
      process_field_common sc encoding_number "Bytes" (fun sc -> 
        F.line sc @@ sp "v.%s <- Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d;" 
          rf_label (decode_field_f field_type pk) 
      ) 
    ) 
    | T.Rt_repeated_field, true -> (
      process_field_common sc encoding_number "Bytes" (fun sc -> 
        F.line sc "Pbrt.Decoder.packed_fold (fun () d -> ";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s;" (decode_field_f field_type pk) rf_label;
        );
        F.line sc       ") () d;";
      ) 
    ) 
  in

  let process_associative_field sc rf_label (at, encoding_number, (key_type, key_pk), (value_type, value_pk)) = 
    let decode_key_f   = decode_basic_type key_type key_pk in 
      (* Because key can never be nested we can assign the decoding function 
       * directly rather wrapping up in a closure like for the value 
       * below
       *)
       (* TODO enhancement
        * For the value decoding function passed as an argument to [Pbrt.Decoder.map_entry] 
        * it's not always the case that it would require nesting. In the 
        * case it does not neeed a nested decoder we can avoid creating 
        * a closure and therefore improving the performance. 
        *)
    process_field_common sc encoding_number "Bytes" (fun sc -> 
      F.line sc "let decode_value = (fun d ->";
      F.scope sc (fun sc ->
        F.line sc @@ decode_field_f value_type value_pk;
      ); 
      F.line sc ") in"; 
      let decode_expression = 
        sp "(Pbrt.Decoder.map_entry d ~decode_key:%s ~decode_value)" decode_key_f in

      begin match at with
      | T.At_list -> ( 
        F.line sc @@ sp "v.%s <- (" rf_label; 
        F.scope sc (fun sc -> 
          F.line sc @@ sp "%s::v.%s;"
            decode_expression rf_label
        ); 
        F.line sc ");" 
      )
      | T.At_hashtable -> (
        F.line sc @@ sp "let a, b = %s in" decode_expression;
        F.line sc @@ sp "Hashtbl.add v.%s a b;" rf_label;
      )
      end;
    )
  in

  let process_variant_field sc rf_label {T.v_constructors; _} = 
    List.iter (fun {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind = pk; } -> 
      process_field_common sc vc_encoding_number (string_of_nonpacked_pk pk) (fun sc->  
        match vc_field_type  with
        | T.Vct_nullary -> (
          F.line sc "Pbrt.Decoder.empty_nested d;";
          F.line sc @@ sp "v.%s <- %s;" rf_label vc_constructor; 
        ) 
        | T.Vct_non_nullary_constructor field_type -> (
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
  let all_lists = List.fold_left (fun acc {T.rf_label; rf_field_type; _ } -> 
    match rf_field_type with
    | T.Rft_repeated_field (T.Rt_list, _, _ , _, _ ) -> rf_label :: acc 
    | T.Rft_associative_field (T.At_list, _, _, _) -> rf_label :: acc
    | _ -> acc  
  ) [] r_fields in  

  let all_required_rf_labels = 
    List.fold_left (fun acc {T.rf_label; rf_field_type; _} -> 
      match rf_field_type with
      | T.Rft_required _ -> rf_label ::  acc 
      | _ -> acc 
    ) [] r_fields 
  in  

  let mutable_record_name = Codegen_util.mutable_record_name r_name in 

  F.line sc @@ sp "%s decode_%s d =" (Codegen_util.let_decl_of_and and_) r_name; 
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" mutable_record_name;

    (* Add the is_set_<field_name> boolean variable which keeps track 
     * of whether a required field is set during the decoding.  *)
    List.iter (fun rf_label -> 
      F.line sc @@ sp "let %s = ref false in" (is_set_variable_name rf_label)
    ) all_required_rf_labels; 

    (* Decoding is done with recursively (tail - recursive). The 
     * function loop iterate over all fields returned by the Protobuf 
     * runtime. *)
    F.line sc "let rec loop () = "; 
    F.scope sc (fun sc -> 
      F.line sc "match Pbrt.Decoder.key d with";

      (* termination condition *)
      F.line sc "| None -> (";
      F.scope sc (fun sc -> 
        List.iter (fun field_name -> 
          F.line sc @@ sp "v.%s <- List.rev v.%s;" field_name field_name
        ) all_lists;   
      );
      F.line sc ")";

      (* compare the decoded field with the one defined in the 
       * .proto file. Unknown fields are ignored. *)
      List.iter (fun {T.rf_label; rf_field_type; _ } -> 
        match rf_field_type with
        | T.Rft_nolabel x -> process_nolabel_field sc rf_label x 
        | T.Rft_required x -> process_required_field sc rf_label x 
        | T.Rft_optional x -> process_optional_field sc rf_label x 
        | T.Rft_repeated_field x -> process_repeated_field sc rf_label x 
        | T.Rft_associative_field x-> process_associative_field sc rf_label x 
        | T.Rft_variant_field x -> process_variant_field sc rf_label x 
      ) r_fields; 
      F.line sc "| Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()";
    ); 
    F.line sc "in"; 
    F.line sc "loop ();";

    (* Add the check to see if all required fields are set if not 
     * a Protobuf.Decoder.Failure exception is raised *) 
    List.iter (fun rf_label -> 
      F.line sc @@ sp 
        ("begin if not !%s then raise Protobuf.Decoder." ^^ 
         "(Failure (Missing_field \"%s\")) end;") 
        (is_set_variable_name rf_label) rf_label
    ) all_required_rf_labels ; 

    (* Rely on Obj.magic invariant to change the type to the 
     * non-mutable record type. 
     * Using Obj.magic avoids an additional allocation of the record 
     *)
    F.line sc @@ sp "let v:%s = Obj.magic v in" r_name; 
    F.line sc "v";
  )

let gen_decode_variant ?and_ {T.v_name; v_constructors;} sc = 

  let process_ctor sc {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind = pk ; } = 
    match vc_field_type with 
    | T.Vct_nullary -> (
      F.line sc @@ sp "| Some (%i, _) -> (Pbrt.Decoder.empty_nested d ; %s)" 
        vc_encoding_number vc_constructor
    ) 
    | T.Vct_non_nullary_constructor field_type -> 
      F.line sc @@ sp "| Some (%i, _) -> %s (%s)" 
        vc_encoding_number vc_constructor (decode_field_f field_type pk) 
  in 

  F.line sc @@ sp "%s decode_%s d = " (Codegen_util.let_decl_of_and and_) v_name;
  F.scope sc (fun sc ->
    F.line sc @@ sp "let rec loop () = "; 
    F.scope sc (fun sc ->
      F.line sc @@ sp "let ret:%s = match Pbrt.Decoder.key d with" v_name;
      F.scope sc (fun sc -> 
        F.line sc "| None -> failwith \"None of the known key is found\"";
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

let gen_decode_const_variant ?and_ {T.cv_name; cv_constructors; } sc = 

  F.line sc @@ sp "%s decode_%s d = " (Codegen_util.let_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun (name, value) -> 
      F.line sc @@ sp "| %i -> (%s:%s)" value name cv_name
    ) cv_constructors; 
    F.line sc @@ sp "| _ -> failwith \"Unknown value for enum %s\"" cv_name
  )

let gen_struct ?and_ t sc = 
  let (), has_encoded =  match t with 
    | {T.spec = T.Record r; _ }  -> gen_decode_record ?and_ r sc, true
    | {T.spec = T.Variant v; _ } -> gen_decode_variant ?and_ v sc, true
    | {T.spec = T.Const_variant v; _ } -> gen_decode_const_variant ?and_ v sc, true
  in
  has_encoded

let gen_sig ?and_ t sc = 

  let _ = and_ in

  let f type_name = 
    F.line sc @@ sp "val decode_%s : Pbrt.Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)" type_name type_name; 
  in 

  let (), has_encoded = 
    match t with 
    | {T.spec = T.Record {T.r_name; _ }; _} -> f r_name, true
    | {T.spec = T.Variant {T.v_name; _ }; _ } -> f v_name, true 
    | {T.spec = T.Const_variant {T.cv_name; _ }; _ } -> f cv_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Decoding"
