module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let sp = Pb_codegen_util.sp

let unsupported json_label =
  failwith (sp "Unsupported field type for field: %s" json_label)

let unsupported2 json_label x y =
  failwith
    (sp "Unsupported 2 field type for field: %s (%s, %s)" json_label
       (Pb_codegen_util.string_of_basic_type x)
       (Pb_codegen_util.string_of_payload_kind y false))

let setter_of_basic_type json_label basic_type pk =
  match basic_type, pk with
  (* String *)
  | Ot.Bt_string, _ -> "string", None
  (* Float *)
  | Ot.Bt_float, Ot.Pk_bits32 -> "number", None
  | Ot.Bt_float, Ot.Pk_bits64 -> "string", Some "Js.Float.toString"
  (* Int32 *)
  | Ot.Bt_int32, _ -> "number", Some "Int32.to_float"
  | Ot.Bt_uint32, _ -> "number", Some "Int32.to_float"
  (* Int64 *)
  | Ot.Bt_int64, Ot.Pk_varint _
  | Ot.Bt_int64, Ot.Pk_bits64
  | Ot.Bt_uint64, Ot.Pk_varint _
  | Ot.Bt_uint64, Ot.Pk_bits64 ->
    "string", Some "Int64.to_string"
  (* 64 bit integer are always encoded as string since
     only support up to 51 bits integer. An improvement
     could be to check for value > 2^51 and use int *)
  (* int *)
  | Ot.Bt_int, Ot.Pk_bits32 -> "number", Some "float_of_int"
  | Ot.Bt_int, Ot.Pk_varint _ | Ot.Bt_int, Ot.Pk_bits64 ->
    "string", Some "string_of_int"
  (* bool *)
  | Ot.Bt_bool, Ot.Pk_varint _ -> "boolean", None
  (* bytes *)
  | Ot.Bt_bytes, Ot.Pk_bytes -> unsupported json_label
  | x, y -> unsupported2 json_label x y

let gen_field sc var_name json_label field_type pk =
  (* reusable function for both basic type and wrapped type which
   * are both based upon basic type *)
  let basic_type_statement basic_type var_name pk =
    let setter, map_function = setter_of_basic_type json_label basic_type pk in
    match map_function with
    | None ->
      Printf.sprintf "Js.Dict.set json \"%s\" (Js.Json.%s %s)" json_label setter
        var_name
    | Some map_function ->
      Printf.sprintf "Js.Dict.set json \"%s\" (Js.Json.%s (%s %s))" json_label
        setter map_function var_name
  in

  match field_type, pk with
  | Ot.Ft_unit, _ -> F.line sc "(* unit type -> encode nothing *)"
  (* Basic types *)
  | Ot.Ft_basic_type basic_type, _ ->
    let statement = basic_type_statement basic_type var_name pk in
    F.linep sc "%s;" statement
  (* User defined *)
  | Ot.Ft_user_defined_type udt, _ ->
    let { Ot.udt_type; _ } = udt in
    let f_name =
      let function_prefix = "encode" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    in
    (match udt_type with
    | `Message ->
      F.linep sc "begin (* %s field *)" json_label;
      F.sub_scope sc (fun sc ->
          F.linep sc "let json' = %s %s in" f_name var_name;
          F.linep sc "Js.Dict.set json \"%s\" (Js.Json.object_ json');"
            json_label);
      F.line sc "end;"
    | `Enum ->
      F.linep sc "Js.Dict.set json \"%s\" (Js.Json.string (%s %s));" json_label
        f_name var_name)
  | Ot.Ft_wrapper_type wrapper_type, _ ->
    let { Ot.wt_type; Ot.wt_pk } = wrapper_type in
    F.line sc "begin";
    F.sub_scope sc (fun sc ->
        F.linep sc "match %s with" var_name;
        F.linep sc "| None -> Js.Dict.set json \"%s\" Js.Json.null" json_label;
        let statement = basic_type_statement wt_type "__x__" wt_pk in
        F.linep sc "| Some __x__ -> %s" statement);
    F.line sc "end;"

let gen_rft_nolabel sc var_name rf_label (field_type, _, pk) =
  let json_label = Pb_codegen_util.camel_case_of_label rf_label in
  gen_field sc var_name json_label field_type pk

let gen_rft_optional sc var_name rf_label (field_type, _, pk, _) =
  F.linep sc "begin match %s with" var_name;
  F.line sc "| None -> ()";
  F.line sc "| Some v ->";
  F.sub_scope sc (fun sc ->
      let json_label = Pb_codegen_util.camel_case_of_label rf_label in
      gen_field sc "v" json_label field_type pk);
  F.line sc "end;"

let gen_rft_repeated sc var_name rf_label repeated_field =
  let repeated_type, field_type, _, pk, _ = repeated_field in
  (match repeated_type with
  | Ot.Rt_list -> ()
  | Ot.Rt_repeated_field ->
    sp "Pbrt.Repeated_field is not supported with JSON (field: %s)" rf_label
    |> failwith);

  let json_label = Pb_codegen_util.camel_case_of_label rf_label in

  F.linep sc "begin match %s with" var_name;
  F.line sc "| [] -> ()";
  F.linep sc "| __x__ -> (* %s *)" json_label;
  F.sub_scope sc (fun sc ->
      match field_type, pk with
      | Ot.Ft_unit, _ -> unsupported json_label
      | Ot.Ft_basic_type basic_type, _ ->
        let setter, map_function =
          setter_of_basic_type json_label basic_type pk
        in
        (match map_function with
        | None ->
          F.linep sc "let a = __x__ |> Array.of_list |> Array.map Js.Json.%s in"
            setter
        | Some map_function ->
          F.line sc
          @@ sp
               ("let a = __x__ |> List.map %s |> Array.of_list "
              ^^ "|> Array.map Js.Json.%s in")
               map_function setter);
        F.linep sc "Js.Dict.set json \"%s\" (Js.Json.array a);" json_label
      (* User defined *)
      | Ot.Ft_user_defined_type udt, Ot.Pk_bytes ->
        let f_name =
          let function_prefix = "encode" in
          Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
        in
        F.linep sc "let (%s':Js.Json.t) =" rf_label;
        F.sub_scope sc (fun sc ->
            F.line sc "__x__";
            F.line sc "|> Array.of_list";
            F.linep sc "|> Array.map (fun v ->";
            F.sub_scope sc (fun sc ->
                F.linep sc "v |> %s |> Js.Json.object_" f_name);
            F.line sc ")";
            F.line sc "|> Js.Json.array");
        F.line sc "in";
        F.linep sc "Js.Dict.set json \"%s\" %s'" json_label rf_label
      | _ -> unsupported json_label);
  F.linep sc "end;"

let gen_rft_variant sc var_name rf_label { Ot.v_constructors; _ } =
  F.linep sc "begin match %s with" var_name;
  F.sub_scope sc (fun sc ->
      List.iter
        (fun { Ot.vc_constructor; vc_field_type; vc_payload_kind; _ } ->
          let var_name = "v" in
          let json_label =
            Pb_codegen_util.camel_case_of_constructor vc_constructor
          in
          F.linep sc "| Some (%s v) ->" vc_constructor;
          F.sub_scope sc (fun sc ->
              match vc_field_type with
              | Ot.Vct_nullary ->
                F.linep sc "Js.Dict.set json \"%s\" Js.Json.null" json_label
              | Ot.Vct_non_nullary_constructor field_type ->
                gen_field sc var_name json_label field_type vc_payload_kind))
        v_constructors;
      F.line sc "| None -> ()");
  F.linep sc "end; (* match v.%s *)" rf_label

let gen_record ?and_ { Ot.r_name; r_fields } sc =
  let rn = r_name in
  F.linep sc "%s encode_%s (v:%s) = "
    (Pb_codegen_util.let_decl_of_and and_)
    rn rn;
  F.sub_scope sc (fun sc ->
      F.line sc "let json = Js.Dict.empty () in";
      List.iter
        (fun record_field ->
          let { Ot.rf_label; rf_field_type; _ } = record_field in
          let var_name = sp "v.%s" rf_label in

          match rf_field_type with
          | Ot.Rft_nolabel nolabel_field ->
            gen_rft_nolabel sc var_name rf_label nolabel_field
          | Ot.Rft_repeated repeated_field ->
            gen_rft_repeated sc var_name rf_label repeated_field
          | Ot.Rft_variant variant_field ->
            gen_rft_variant sc var_name rf_label variant_field
          | Ot.Rft_optional optional_field ->
            gen_rft_optional sc var_name rf_label optional_field
          | Ot.Rft_required _ ->
            Printf.eprintf "Only proto3 syntax supported in JSON encoding";
            exit 1
          | Ot.Rft_associative _ ->
            Printf.eprintf "Map field are not currently supported for JSON";
            exit 1)
        r_fields (* List.iter *);
      F.line sc "json")

let gen_variant ?and_ { Ot.v_name; v_constructors } sc =
  let process_v_constructor sc v_constructor =
    let { Ot.vc_constructor; Ot.vc_field_type; Ot.vc_payload_kind; _ } =
      v_constructor
    in

    let json_label = Pb_codegen_util.camel_case_of_constructor vc_constructor in

    match vc_field_type with
    | Ot.Vct_nullary ->
      F.linep sc "| %s ->" vc_constructor;
      F.linep sc "  Js.Dict.set json \"%s\" Js.Json.null" json_label
    | Ot.Vct_non_nullary_constructor field_type ->
      F.linep sc "| %s v ->" vc_constructor;
      F.sub_scope sc (fun sc ->
          gen_field sc "v" json_label field_type vc_payload_kind)
  in

  F.linep sc "%s encode_%s (v:%s) = "
    (Pb_codegen_util.let_decl_of_and and_)
    v_name v_name;
  F.sub_scope sc (fun sc ->
      F.line sc "let json = Js.Dict.empty () in";
      F.line sc "begin match v with";
      List.iter (process_v_constructor sc) v_constructors;
      F.line sc "end;";
      F.line sc "json")

let gen_const_variant ?and_ { Ot.cv_name; Ot.cv_constructors } sc =
  F.linep sc "%s encode_%s (v:%s) : string = "
    (Pb_codegen_util.let_decl_of_and and_)
    cv_name cv_name;
  F.sub_scope sc (fun sc ->
      F.line sc "match v with";
      List.iter
        (fun { Ot.cvc_name; cvc_string_value; _ } ->
          F.linep sc "| %s -> \"%s\"" cvc_name cvc_string_value)
        cv_constructors)

let gen_unit ?and_ { Ot.er_name } sc =
  let rn = er_name in
  F.linep sc "%s encode_%s (_v:%s) = "
    (Pb_codegen_util.let_decl_of_and and_)
    rn rn;
  F.line sc "Js.Json.null"

let gen_struct ?and_ t sc : bool =
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
    | Ot.Unit v ->
      gen_unit ?and_ v sc;
      true
  in
  has_encoded

let gen_sig ?and_ t sc =
  let _ = and_ in
  let { Ot.spec; _ } = t in
  let f type_name =
    F.linep sc "val encode_%s : %s -> Js.Json.t Js.Dict.t" type_name type_name;
    F.linep sc
      ("(** [encode_%s v dict] encodes [v] int the " ^^ "given JSON [dict] *)")
      type_name
  in
  match spec with
  | Ot.Record { Ot.r_name; _ } ->
    f r_name;
    true
  | Ot.Variant v ->
    f v.Ot.v_name;
    true
  | Ot.Const_variant { Ot.cv_name; _ } ->
    F.linep sc "val encode_%s : %s -> string" cv_name cv_name;
    F.linep sc "(** [encode_%s v] returns JSON string *)" cv_name;
    true
  | Ot.Unit { Ot.er_name } ->
    F.linep sc "val encode_%s : %s -> Js.Json.t" er_name er_name;
    F.linep sc "(** [encode_%s v] returns JSON null *)" er_name;
    true

let ocamldoc_title = "Protobuf JSON Encoding"
let requires_mutable_records = false

let plugin : Pb_codegen_plugin.t =
  let module P = struct
    let gen_sig = gen_sig
    let gen_struct = gen_struct
    let ocamldoc_title = ocamldoc_title
    let requires_mutable_records = requires_mutable_records
  end in
  (module P)
