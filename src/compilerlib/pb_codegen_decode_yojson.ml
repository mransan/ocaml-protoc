module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

(** Function which returns all the possible pattern match for reading a JSON
    value into an OCaml value. The protobuf JSON encoding rules are defined
    here: https://developers.google.com/protocol-buffers/docs/proto3#json *)
let field_pattern_match ~r_name ~rf_label field_type =
  match field_type with
  | Ot.Ft_basic_type bt ->
    let decode runtime_f =
      sp "Pbrt_yojson.%s json_value \"%s\" \"%s\"" runtime_f r_name rf_label
    in
    let exp =
      match bt with
      | Ot.Bt_string -> decode "string"
      | Ot.Bt_float -> decode "float"
      | Ot.Bt_int -> decode "int"
      | Ot.Bt_int32 -> decode "int32"
      | Ot.Bt_int64 -> decode "int64"
      | Ot.Bt_uint32 -> sp "`unsigned (%s)" (decode "int32")
      | Ot.Bt_uint64 -> sp "`unsigned (%s)" (decode "int64")
      | Ot.Bt_bool -> decode "bool"
      | Ot.Bt_bytes -> decode "bytes"
    in
    "json_value", exp
  | Ot.Ft_unit ->
    "json_value", sp "Pbrt_yojson.unit json_value \"%s\" \"%s\"" r_name rf_label
  (* TODO Wrapper: add similar one for wrapper type (with different
     runtime functions) *)
  | Ot.Ft_user_defined_type udt ->
    let f_name =
      let function_prefix = "decode_json" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    in
    let value_expression = "(" ^ f_name ^ " json_value)" in
    "json_value", value_expression
  | _ -> assert false

(* Generate all the pattern matches for a record field *)
let gen_rft_nolabel sc ~r_name ~rf_label (field_type, _, _) =
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in

  let match_variable_name, exp =
    field_pattern_match ~r_name ~rf_label field_type
  in
  F.linep sc "| (\"%s\", %s) -> " json_label match_variable_name;
  F.linep sc "  v.%s <- %s" rf_label exp

(* Generate all the pattern matches for a repeated field *)
let gen_rft_repeated_field sc ~r_name ~rf_label repeated_field =
  let _, field_type, _, _, _ = repeated_field in

  let json_label = Pb_codegen_util.camel_case_of_label rf_label in

  F.linep sc "| (\"%s\", `List l) -> begin" json_label;

  F.sub_scope sc (fun sc ->
      F.linep sc "v.%s <- List.map (function" rf_label;
      let match_variable_name, exp =
        field_pattern_match ~r_name ~rf_label field_type
      in
      F.linep sc "  | %s -> %s" match_variable_name exp;
      F.line sc ") l;");

  F.line sc "end"

let gen_rft_optional_field sc ~r_name ~rf_label optional_field =
  let field_type, _, _, _ = optional_field in

  let json_label = Pb_codegen_util.camel_case_of_label rf_label in

  let match_variable_name, exp =
    field_pattern_match ~r_name ~rf_label field_type
  in

  F.linep sc "| (\"%s\", %s) -> " json_label match_variable_name;
  F.linep sc "  v.%s <- Some (%s)" rf_label exp

(* Generate pattern match for a variant field *)
let gen_rft_variant_field sc ~r_name ~rf_label { Ot.v_constructors; _ } =
  List.iter
    (fun { Ot.vc_constructor; vc_field_type; _ } ->
      let json_label =
        Pb_codegen_util.camel_case_of_constructor vc_constructor
      in

      match vc_field_type with
      | Ot.Vct_nullary ->
        F.linep sc "| (\"%s\", _) -> v.%s <- Some %s" json_label rf_label
          vc_constructor
      | Ot.Vct_non_nullary_constructor field_type ->
        let match_variable_name, exp =
          field_pattern_match ~r_name ~rf_label field_type
        in
        F.linep sc "| (\"%s\", %s) -> " json_label match_variable_name;
        F.linep sc "  v.%s <- Some (%s (%s))" rf_label vc_constructor exp)
    v_constructors

let gen_rft_assoc_field sc ~r_name ~rf_label ~assoc_type ~key_type ~value_type =
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in
  F.linep sc "| (\"%s\", `Assoc assoc) ->" json_label;
  F.sub_scope sc (fun sc ->
      let value_name, value_exp =
        field_pattern_match ~r_name ~rf_label value_type
      in
      let key_name = "key" in
      let key_exp =
        match key_type with
        | Ot.Bt_string -> "key"
        | Ot.Bt_int -> "(Int.of_string key)"
        | Ot.Bt_int32 -> "(Int32.of_string key)"
        | Ot.Bt_int64 -> "(Int64.of_string key)"
        | Ot.Bt_uint32 -> "(`unsigned (Int32.of_string key))"
        | Ot.Bt_uint64 -> "(`unsigned (Int64.of_string key))"
        | Ot.Bt_bool -> "(Bool.of_string key)"
        | Ot.Bt_float ->
          Printf.eprintf "float cannot be used as a map key type";
          exit 1
        | Ot.Bt_bytes ->
          Printf.eprintf "bytes cannot be used as a map key type";
          exit 1
      in
      F.line sc "let assoc =";
      F.sub_scope sc (fun sc ->
          F.line sc "assoc";
          F.linep sc "|> List.map (fun (%s, %s) -> (%s, %s)) " key_name
            value_name key_exp value_exp;
          F.line sc "|> List.to_seq";
          (* Passing through [Hashtbl.of_seq] even in the [At_list] case ensures that if there
             is a repeated key we take the last value associated with it. *)
          F.line sc "|> Hashtbl.of_seq");
      F.line sc "in";
      let assoc_exp =
        match assoc_type with
        | Ot.At_hashtable -> "assoc"
        | Ot.At_list -> "assoc |> Hashtbl.to_seq |> List.of_seq"
      in
      F.linep sc "v.%s <- %s" rf_label assoc_exp)

(* Generate decode function for a record *)
let gen_record ?and_ { Ot.r_name; r_fields } sc =
  let mutable_record_name = Pb_codegen_util.mutable_record_name r_name in

  F.line sc
  @@ sp "%s decode_json_%s d =" (Pb_codegen_util.let_decl_of_and and_) r_name;

  F.sub_scope sc (fun sc ->
      F.linep sc "let v = default_%s () in" mutable_record_name;
      F.line sc @@ "let assoc = match d with";
      F.line sc @@ "  | `Assoc assoc -> assoc";
      F.line sc @@ "  | _ -> assert(false)";
      (* TODO raise E *)
      F.line sc @@ "in";

      F.line sc "List.iter (function ";
      F.sub_scope sc (fun sc ->
          (* Generate pattern match for all the possible message field *)
          List.iter
            (fun { Ot.rf_label; rf_field_type; _ } ->
              match rf_field_type with
              | Ot.Rft_nolabel nolabel_field ->
                gen_rft_nolabel sc ~r_name ~rf_label nolabel_field
              | Ot.Rft_repeated repeated_field ->
                gen_rft_repeated_field sc ~r_name ~rf_label repeated_field
              | Ot.Rft_variant variant_field ->
                gen_rft_variant_field sc ~r_name ~rf_label variant_field
              | Ot.Rft_optional optional_field ->
                gen_rft_optional_field sc ~r_name ~rf_label optional_field
              | Ot.Rft_required _ ->
                Printf.eprintf "Only proto3 syntax supported in JSON encoding";
                exit 1
              | Ot.Rft_associative
                  (assoc_type, _, (key_type, _), (value_type, _)) ->
                gen_rft_assoc_field sc ~r_name ~rf_label ~assoc_type ~key_type
                  ~value_type)
            r_fields;

          (* Unknown fields are simply ignored *)
          F.empty_line sc;
          F.line sc "| (_, _) -> () (*Unknown fields are ignored*)");
      F.line sc ") assoc;";

      let has_presence =
        List.exists
          (fun { Ot.rf_presence; _ } -> Ot.rfp_requires_bitfield rf_presence)
          r_fields
      in

      (* Transform the mutable record in an immutable one *)
      F.line sc "({";
      F.sub_scope sc (fun sc ->
          if has_presence then F.line sc "_presence = v._presence;";
          List.iter
            (fun { Ot.rf_label; _ } ->
              F.linep sc "%s = v.%s;" rf_label rf_label)
            r_fields);
      F.linep sc "} : %s)" r_name)

(* Generate decode function for an empty record *)
let gen_unit ?and_ { Ot.er_name } sc =
  F.line sc
  @@ sp "%s decode_json_%s d =" (Pb_codegen_util.let_decl_of_and and_) er_name;
  F.line sc (sp "Pbrt_yojson.unit d \"%s\" \"%s\"" er_name "empty record")

(* Generate decode function for a variant type *)
let gen_variant ?and_ { Ot.v_name; v_constructors } sc =
  (* helper function for each constructor case *)
  let process_v_constructor sc { Ot.vc_constructor; vc_field_type; _ } =
    let json_label = Pb_codegen_util.camel_case_of_constructor vc_constructor in

    match vc_field_type with
    | Ot.Vct_nullary ->
      F.linep sc "| (\"%s\", _)::_-> (%s : %s)" json_label vc_constructor v_name
    | Ot.Vct_non_nullary_constructor field_type ->
      let match_, exp =
        let r_name = v_name and rf_label = vc_constructor in
        field_pattern_match ~r_name ~rf_label field_type
      in

      F.linep sc "| (\"%s\", %s)::_ -> " json_label match_;
      F.linep sc "  (%s (%s) : %s)" vc_constructor exp v_name
  in

  F.linep sc "%s decode_json_%s json ="
    (Pb_codegen_util.let_decl_of_and and_)
    v_name;

  F.sub_scope sc (fun sc ->
      (* even though a variant should be an object with a single field,
       * it is possible other fields are present in the JSON object. Therefore
       * we still need a loop to iterate over the key/value, even if in 99.99%
       * of the cases it will be a single iteration *)
      F.line sc "let assoc = match json with";
      F.line sc "  | `Assoc assoc -> assoc";
      F.line sc "  | _ -> assert(false)";
      (* TODO raise E *)
      F.line sc "in";

      F.line sc "let rec loop = function";
      F.sub_scope sc (fun sc ->
          (* termination condition *)
          F.linep sc "| [] -> Pbrt_yojson.E.malformed_variant \"%s\"" v_name;

          List.iter (process_v_constructor sc) v_constructors;

          F.empty_line sc;
          F.line sc "| _ :: tl -> loop tl");
      F.line sc "in";
      F.line sc "loop assoc")

let gen_const_variant ?and_ { Ot.cv_name; cv_constructors } sc =
  F.linep sc "%s decode_json_%s json ="
    (Pb_codegen_util.let_decl_of_and and_)
    cv_name;

  F.sub_scope sc (fun sc ->
      F.line sc "match json with";
      List.iter
        (fun { Ot.cvc_name; cvc_string_value; _ } ->
          F.linep sc "| `String \"%s\" -> (%s : %s)" cvc_string_value cvc_name
            cv_name)
        cv_constructors;
      F.linep sc "| _ -> Pbrt_yojson.E.malformed_variant \"%s\"" cv_name)

let gen_struct ?and_ t sc =
  let { Ot.spec; _ } = t in
  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record ?and_ r sc;
      true
    | Ot.Variant v ->
      gen_variant ?and_ v sc;
      true
    | Ot.Const_variant v ->
      gen_const_variant ?and_ v sc;
      true
    | Ot.Unit u ->
      gen_unit ?and_ u sc;
      true
  in
  has_encoded

let gen_sig ?and_ t sc =
  let _ = and_ in
  let { Ot.spec; _ } = t in

  let f type_name =
    F.linep sc "val decode_json_%s : Yojson.Basic.t -> %s" type_name type_name;
    F.linep sc
      ("(** [decode_json_%s decoder] decodes a "
     ^^ "[%s] value from [decoder] *)")
      type_name type_name
  in

  match spec with
  | Ot.Record { Ot.r_name; _ } ->
    f r_name;
    true
  | Ot.Variant { Ot.v_name; _ } ->
    f v_name;
    true
  | Ot.Const_variant { Ot.cv_name; _ } ->
    f cv_name;
    true
  | Ot.Unit { Ot.er_name; _ } ->
    f er_name;
    true

let ocamldoc_title = "JSON Decoding"
let requires_mutable_records = true

let plugin : Pb_codegen_plugin.t =
  let module P = struct
    let gen_sig = gen_sig
    let gen_struct = gen_struct
    let ocamldoc_title = ocamldoc_title
    let requires_mutable_records = requires_mutable_records
  end in
  (module P)
