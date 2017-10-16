module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

let file_suffix = "pb"

let runtime_function_for_basic_type bt pk =
  match pk, bt with
  | Ot.Pk_varint false, Ot.Bt_int   -> "Pbrt.Decoder.int_as_varint"
  | Ot.Pk_varint true , Ot.Bt_int   -> "Pbrt.Decoder.int_as_zigzag"
  | Ot.Pk_varint false, Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_varint"
  | Ot.Pk_varint true , Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_zigzag"
  | Ot.Pk_varint false, Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_varint"
  | Ot.Pk_varint true , Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_zigzag"
  | Ot.Pk_bits32, Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_bits32"
  | Ot.Pk_bits64, Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_bits64"
  | Ot.Pk_varint false, Ot.Bt_bool -> "Pbrt.Decoder.bool"
  | Ot.Pk_bits32, Ot.Bt_float -> "Pbrt.Decoder.float_as_bits32"
  | Ot.Pk_bits64, Ot.Bt_float -> "Pbrt.Decoder.float_as_bits64"
  | Ot.Pk_bits32, Ot.Bt_int -> "Pbrt.Decoder.int_as_bits32"
  | Ot.Pk_bits64, Ot.Bt_int -> "Pbrt.Decoder.int_as_bits64"
  | Ot.Pk_bytes, Ot.Bt_string -> "Pbrt.Decoder.string"
  | Ot.Pk_bytes, Ot.Bt_bytes -> "Pbrt.Decoder.bytes"
  | _ -> failwith "Invalid encoding/OCaml type combination"

let runtime_function_for_wrapper_type {Ot.wt_type; wt_pk} = 
  match wt_type, wt_pk with
  | Ot.Bt_float, Ot.Pk_bits64 -> "Pbrt.Decoder.wrapper_double_value"
  | Ot.Bt_float, Ot.Pk_bits32 -> "Pbrt.Decoder.wrapper_float_value"
  | Ot.Bt_int64, Ot.Pk_varint _ -> "Pbrt.Decoder.wrapper_int64_value"
  | Ot.Bt_int32, Ot.Pk_varint _ -> "Pbrt.Decoder.wrapper_int32_value"
  | Ot.Bt_bool, Ot.Pk_varint _ -> "Pbrt.Decoder.wrapper_bool_value"
  | Ot.Bt_string, Ot.Pk_bytes -> "Pbrt.Decoder.wrapper_string_value"
  | Ot.Bt_bytes, Ot.Pk_bytes -> "Pbrt.Decoder.wrapper_bytes_value"
  | _ -> assert(false)

let decode_field_expression field_type pk =
  match field_type with
  | Ot.Ft_user_defined_type t ->
    let f_name =
      let function_prefix = "decode" in
      let module_suffix = file_suffix in
      Pb_codegen_util.function_name_of_user_defined
        ~function_prefix ~module_suffix t
    in
    begin match t.Ot.udt_type with
    | `Message -> (f_name ^ " (Pbrt.Decoder.nested d)")
    | `Enum -> (f_name ^ " d")
    end
  | Ot.Ft_unit ->
      "Pbrt.Decoder.empty_nested d"
  | Ot.Ft_basic_type bt -> (runtime_function_for_basic_type bt pk) ^ " d"
  | Ot.Ft_wrapper_type wt -> (runtime_function_for_wrapper_type wt) ^ " d"

let pbrt_payload_kind payload_kind is_packed =
  if is_packed
  then
    "Bytes"
  else
    Pb_codegen_util.string_of_payload_kind ~capitalize:() payload_kind false

let gen_field_common sc encoding_number payload_kind message_name
                         ?(is_packed = false) f =
  F.linep sc "| Some (%i, Pbrt.%s) -> begin"
    encoding_number (pbrt_payload_kind payload_kind is_packed);
  F.scope sc (fun sc ->
    f sc;
  );
  F.line sc "end";
  F.linep sc "| Some (%i, pk) -> " encoding_number;
  F.linep sc "  Pbrt.Decoder.unexpected_payload \"%s\" pk"
    (sp "Message(%s), field(%i)" message_name encoding_number)

let gen_rft_nolabel sc r_name rf_label (field_type, encoding_number, pk) =
  gen_field_common sc encoding_number pk r_name (fun sc ->
    F.linep sc "v.%s <- %s;" rf_label (decode_field_expression field_type pk);
  )

(* return the variable name used for keeping track if a required
 * field has been set during decoding.  *)
let is_set_variable_name rf_label =
  sp "%s_is_set" rf_label

let gen_rft_required sc r_name rf_label (field_type, encoding_number, pk, _) =
  gen_field_common sc encoding_number pk r_name (fun sc ->
    F.linep sc "v.%s <- %s; %s := true;"
        rf_label
        (decode_field_expression field_type pk)
        (is_set_variable_name rf_label);
  )

let gen_rft_optional sc r_name rf_label optional_field =
  let (field_type, encoding_number, pk, _) = optional_field in
  gen_field_common sc encoding_number pk r_name (fun sc ->
    F.linep sc "v.%s <- Some (%s);" rf_label
        (decode_field_expression field_type pk);
  )

let gen_rft_repeated sc r_name rf_label repeated_field =
  let (rt, field_type, encoding_number, pk, is_packed) = repeated_field in
  match rt, is_packed with
  | Ot.Rt_list, false ->
    gen_field_common sc encoding_number pk r_name ~is_packed (fun sc ->
      F.linep sc "v.%s <- (%s) :: v.%s;"
        rf_label (decode_field_expression field_type pk) rf_label;
    )
  | Ot.Rt_repeated_field, false -> (
    gen_field_common sc encoding_number pk r_name ~is_packed (fun sc ->
      F.linep sc "Pbrt.Repeated_field.add (%s) v.%s; "
        (decode_field_expression field_type pk) rf_label;
    )
  )
  | Ot.Rt_list, true -> (
    gen_field_common sc encoding_number pk r_name ~is_packed (fun sc ->
      F.linep sc
        "v.%s <- Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d;"
        rf_label (decode_field_expression field_type pk)
    )
  )
  | Ot.Rt_repeated_field, true -> (
    gen_field_common sc encoding_number pk r_name ~is_packed (fun sc ->
      F.line sc "Pbrt.Decoder.packed_fold (fun () d -> ";
      F.scope sc (fun sc ->
        F.linep sc "Pbrt.Repeated_field.add (%s) v.%s;"
            (decode_field_expression field_type pk) rf_label;
      );
      F.line sc ") () d;";
    )
  )

let gen_rft_associative sc r_name rf_label associative_field =

  let (
    at,
    encoding_number,
    (key_type, key_pk),
    (value_type, value_pk)) = associative_field in

  let decode_key_f   = runtime_function_for_basic_type key_type key_pk in
    (* Because key can never be nested we can assign the decoding function
     * directly rather wrapping up in a closure like for the value
     * below
     *)
     (* TODO enhancement
      * For the value decoding function passed as an argument to
      * [Pbrt.Decoder.map_entry] it's not always the case that it would
      * require nesting. In the case it does not neeed a nested decoder
      * we can avoid creating a closure and therefore improving
      * the performance.  *)

  gen_field_common sc encoding_number Ot.Pk_bytes r_name (fun sc ->
    F.line sc "let decode_value = (fun d ->";
    F.scope sc (fun sc ->
      F.line sc @@ decode_field_expression value_type value_pk;
    );
    F.line sc ") in";
    let decode_expression = sp
        "(Pbrt.Decoder.map_entry d ~decode_key:%s ~decode_value)" decode_key_f
    in

    begin match at with
    | Ot.At_list ->
      F.linep sc "v.%s <- (" rf_label;
      F.scope sc (fun sc ->
        F.linep sc "%s::v.%s;" decode_expression rf_label
      );
      F.line sc ");"

    | Ot.At_hashtable ->
      F.linep sc "let a, b = %s in" decode_expression;
      F.linep sc "Hashtbl.add v.%s a b;" rf_label;
    end;
  )

let gen_rft_variant sc module_prefix r_name rf_label {Ot.v_constructors; _} =
  List.iter (fun variant_constructor  ->

    let {
      Ot.vc_constructor;
      vc_field_type;
      vc_encoding_number;
      vc_payload_kind = pk; } = variant_constructor in

    gen_field_common sc vc_encoding_number pk r_name (fun sc->
      match vc_field_type  with
      | Ot.Vct_nullary -> (
        F.line sc "Pbrt.Decoder.empty_nested d;";
        F.linep sc "v.%s <- %s_types.%s;" rf_label module_prefix vc_constructor;
      )
      | Ot.Vct_non_nullary_constructor field_type -> (
        F.linep sc "v.%s <- %s_types.%s (%s);"
            rf_label
            module_prefix
            vc_constructor
            (decode_field_expression field_type pk)
      )
    )
  ) v_constructors

let gen_record ?and_ module_prefix {Ot.r_name; r_fields} sc =

  (* list fields have a special treatement when decoding since each new element
   * of a repeated field is appended to the front of the list. In order
   * to retreive the right order efficiently we reverse all the repeated field
   * lists values when the message is done being decoded.  *)
  let all_lists = List.fold_left (fun acc {Ot.rf_label; rf_field_type; _ } ->
    match rf_field_type with
    | Ot.Rft_repeated (Ot.Rt_list, _, _ , _, _ ) -> rf_label :: acc
    | Ot.Rft_associative (Ot.At_list, _, _, _) -> rf_label :: acc
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

  F.linep sc "%s decode_%s d =" (Pb_codegen_util.let_decl_of_and and_) r_name;
  F.scope sc (fun sc ->
    F.linep sc "let v = default_%s () in" mutable_record_name;
    F.line sc "let continue__= ref true in";

    (* Add the is_set_<field_name> boolean variable which keeps track
     * of whether a required field is set during the decoding.  *)
    List.iter (fun rf_label ->
      F.linep sc "let %s = ref false in" (is_set_variable_name rf_label)
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
          F.linep sc "v.%s <- List.rev v.%s;" field_name field_name
        ) all_lists;
      );
      F.line sc "); continue__ := false";

      (* compare the decoded field with the one defined in the
       * .proto file. Unknown fields are ignored. *)
      List.iter (fun {Ot.rf_label; rf_field_type; _ } ->
        match rf_field_type with
        | Ot.Rft_nolabel x ->
          gen_rft_nolabel sc r_name rf_label x
        | Ot.Rft_required x ->
          gen_rft_required sc r_name rf_label x
        | Ot.Rft_optional x ->
          gen_rft_optional sc r_name rf_label x
        | Ot.Rft_repeated x ->
          gen_rft_repeated sc r_name rf_label x
        | Ot.Rft_associative x->
          gen_rft_associative sc r_name rf_label x
        | Ot.Rft_variant x ->
          gen_rft_variant sc module_prefix r_name rf_label x
      ) r_fields;
      F.line sc ("| Some (_, payload_kind) -> " ^
                 "Pbrt.Decoder.skip d payload_kind");
    );
    F.line sc "done;";

    (* Add the check to see if all required fields are set if not
     * a Protobuf.Decoder.Failure exception is raised *)
    List.iter (fun rf_label ->
      F.linep sc "begin if not !%s then Pbrt.Decoder.missing_field \"%s\" end;"
        (is_set_variable_name rf_label) rf_label
    ) all_required_rf_labels ;

    F.line sc "({";
    F.scope sc (fun sc ->
      List.iter (fun {Ot.rf_label;_} ->
        F.linep sc "%s_types.%s = v.%s;" module_prefix rf_label rf_label;
      ) r_fields;
    );
    F.linep sc "} : %s_types.%s)" module_prefix r_name;
  )

let gen_variant ?and_ module_prefix {Ot.v_name; v_constructors;} sc =

  let process_ctor sc variant_constructor =

    let {
      Ot.vc_constructor;
      vc_field_type;
      vc_encoding_number;
      vc_payload_kind = pk;} = variant_constructor in

    match vc_field_type with
    | Ot.Vct_nullary -> (
      F.linep sc "| Some (%i, _) -> begin " vc_encoding_number;
      F.scope sc (fun sc ->
        F.line sc "Pbrt.Decoder.empty_nested d ;";
        F.linep sc "(%s_types.%s : %s_types.%s)" module_prefix vc_constructor
          module_prefix v_name;
      );
      F.line sc "end";
    )
    | Ot.Vct_non_nullary_constructor field_type ->
      F.linep sc "| Some (%i, _) -> (%s_types.%s (%s) : %s_types.%s) "
        vc_encoding_number
        module_prefix
        vc_constructor
        (decode_field_expression field_type pk)
        module_prefix
        v_name
  in

  F.linep sc "%s decode_%s d = "
    (Pb_codegen_util.let_decl_of_and and_) v_name;
  F.scope sc (fun sc ->
    F.linep sc "let rec loop () = ";
    F.scope sc (fun sc ->
      F.linep sc "let ret:%s_types.%s = match Pbrt.Decoder.key d with"
        module_prefix v_name;

      F.scope sc (fun sc ->
        F.linep sc "| None -> Pbrt.Decoder.malformed_variant \"%s\"" v_name;
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

let gen_const_variant ?and_ module_prefix {Ot.cv_name; cv_constructors; } sc =

  F.linep sc "%s decode_%s d = " (Pb_codegen_util.let_decl_of_and and_)
      cv_name;
  F.scope sc (fun sc ->
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun {Ot.cvc_name; cvc_binary_value; _} ->
      F.linep sc "| %i -> (%s_types.%s:%s_types.%s)"
        cvc_binary_value module_prefix cvc_name module_prefix cv_name
    ) cv_constructors;
    F.linep sc "| _ -> Pbrt.Decoder.malformed_variant \"%s\"" cv_name
  )

let gen_struct ?and_ t sc =
  let {Ot.module_prefix; spec; _} = t in

  let has_encoded =
    match spec with
    | Ot.Record r  -> gen_record ?and_ module_prefix r sc; true
    | Ot.Variant v -> gen_variant ?and_ module_prefix v sc; true
    | Ot.Const_variant v -> gen_const_variant ?and_ module_prefix v sc; true
  in

  has_encoded

let gen_sig ?and_ t sc =
  let _ = and_ in

  let {Ot.module_prefix; spec; _} = t in

  let f type_name =
    F.linep sc "val decode_%s : Pbrt.Decoder.t -> %s_types.%s"
      type_name module_prefix type_name ;
    F.linep sc ("(** [decode_%s decoder] decodes a " ^^
                     "[%s] value from [decoder] *)") type_name type_name;
  in

  let has_encoded =
    match spec with
    | Ot.Record {Ot.r_name; _ } -> f r_name; true
    | Ot.Variant {Ot.v_name; _ } -> f v_name; true
    | Ot.Const_variant {Ot.cv_name; _ } -> f cv_name; true
  in

  has_encoded

let ocamldoc_title = "Protobuf Decoding"
