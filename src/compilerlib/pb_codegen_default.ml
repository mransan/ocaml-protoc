module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
module E = Pb_exception
open Pb_codegen_util

let default_value_of_basic_type ?field_name basic_type field_default =
  match basic_type, field_default with
  | Ot.Bt_string, None -> "\"\""
  | Ot.Bt_string, Some (Pb_option.Constant_string s) -> sp "\"%s\"" s
  | Ot.Bt_float, None -> "0."
  | Ot.Bt_float, Some (Pb_option.Constant_float f) -> string_of_float f
  | Ot.Bt_int, None -> "0"
  | Ot.Bt_int, Some (Pb_option.Constant_int i) -> string_of_int i
  | Ot.Bt_int32, None -> "0l"
  | Ot.Bt_int32, Some (Pb_option.Constant_int i) -> sp "%il" i
  | Ot.Bt_uint32, None -> "(`unsigned 0l)"
  | Ot.Bt_uint32, Some (Pb_option.Constant_int i) -> sp "(`unsigned %il)" i
  | Ot.Bt_int64, None -> "0L"
  | Ot.Bt_int64, Some (Pb_option.Constant_int i) -> sp "%iL" i
  | Ot.Bt_uint64, None -> "(`unsigned 0L)"
  | Ot.Bt_uint64, Some (Pb_option.Constant_int i) -> sp "(`unsigned %iL)" i
  | Ot.Bt_bytes, None -> "Bytes.create 0"
  | Ot.Bt_bytes, Some (Pb_option.Constant_string s) ->
    sp "Bytes.of_string \"%s\"" s
  | Ot.Bt_bool, None -> "false"
  | Ot.Bt_bool, Some (Pb_option.Constant_bool b) -> string_of_bool b
  | _ -> E.invalid_default_value ?field_name ~info:"invalid default type" ()

(* Generate the string which is the default value for a given field
   type and default information. *)
let default_value_of_field_type ?field_name field_type field_default =
  match field_type with
  | Ot.Ft_user_defined_type udt ->
    let function_prefix = "default" in
    function_name_of_user_defined ~function_prefix udt ^ " ()"
  | Ot.Ft_unit -> "()"
  | Ot.Ft_basic_type bt ->
    default_value_of_basic_type ?field_name bt field_default
  | Ot.Ft_wrapper_type _ -> "None"

type default_info = {
  fname: string;
  ftype: string;
  default_value: string;  (** Default value if not provided *)
  requires_presence: bool;
  presence_idx: int;
  optional: bool;  (** Can actually skip providing this? *)
}

(** This function returns [(field_name, field_default_value, field_type)] for a
    record field. *)
let record_field_default_info (record_field : Ot.record_field) : default_info =
  let { Ot.rf_label; Ot.rf_field_type; _ } = record_field in
  let type_string = Pb_codegen_util.string_of_record_field_type rf_field_type in
  let field_name = rf_label in

  let dfvft field_type field_default =
    default_value_of_field_type ~field_name field_type field_default
  in

  let default_value, optional =
    match rf_field_type with
    | Ot.Rft_nolabel (field_type, _, _) -> dfvft field_type None, true
    | Ot.Rft_required (field_type, _, _, default_value) ->
      dfvft field_type default_value, false
    | Ot.Rft_optional (field_type, _, _, default_value) ->
      let d =
        match default_value with
        | None -> "None"
        | Some _ -> sp "Some (%s)" @@ dfvft field_type default_value
      in
      d, true
    | Ot.Rft_repeated (rt, field_type, _, _, _) ->
      let d =
        match rt with
        | Ot.Rt_list -> "[]"
        | Ot.Rt_repeated_field ->
          sp "Pbrt.Repeated_field.make (%s)" (dfvft field_type None)
      in
      d, true
    | Ot.Rft_associative (at, _, _, _) ->
      let d =
        match at with
        | Ot.At_list -> "[]"
        | Ot.At_hashtable -> "Hashtbl.create 128"
      in
      d, true
    | Ot.Rft_variant { Ot.v_constructors; _ } ->
      (* TODO This initial value could be configurable either via
         the default function or via a protobuf option. *)
      let d =
        match v_constructors with
        | [] -> assert false
        | { Ot.vc_constructor; vc_field_type; _ } :: _ ->
          (match vc_field_type with
          | Ot.Vct_nullary -> vc_constructor
          | Ot.Vct_non_nullary_constructor field_type ->
            sp "%s (%s)" vc_constructor (dfvft field_type None))
      in
      d, true
  in
  {
    fname = field_name;
    default_value;
    ftype = type_string;
    optional;
    requires_presence = record_field.rf_requires_presence;
    presence_idx = record_field.rf_presence_idx;
  }

(* TODO: also update this *)
let gen_record_mutable { Ot.r_name; r_fields } sc : unit =
  let fields_default_info =
    List.map (fun r_field -> record_field_default_info r_field) r_fields
  in

  let rn = Pb_codegen_util.mutable_record_name r_name in
  F.linep sc "let default_%s () : %s = {" rn rn;

  F.sub_scope sc (fun sc ->
      List.iter
        (fun { fname; default_value; _ } ->
          F.linep sc "%s = %s;" fname default_value)
        fields_default_info);
  F.line sc "}"

let gen_record ?and_ { Ot.r_name; r_fields } sc : unit =
  let fields_default_info =
    List.map (fun r_field -> record_field_default_info r_field) r_fields
  in
  let n_presence =
    List.filter (fun dinfo -> dinfo.requires_presence) fields_default_info
    |> List.length
  in

  F.linep sc "%s default_%s " (let_decl_of_and and_) r_name;

  F.sub_scope sc (fun sc ->
      List.iter
        (fun d ->
          if d.optional then
            F.linep sc "?(%s:%s)" d.fname d.ftype
          else if d.requires_presence then
            F.linep sc "?(%s:%s option)" d.fname d.ftype
          else
            F.linep sc "?%s:((%s:%s) = %s)" d.fname d.fname d.ftype
              d.default_value)
        fields_default_info;
      F.linep sc "() : %s  =" r_name);

  F.sub_scope sc (fun sc ->
      if n_presence > 0 then
        F.linep sc "let _presence=Pbrt.Bitfield.create %d in" n_presence;
      F.linep sc "{";
      (* add bitfield *)
      if n_presence > 0 then F.line sc "_presence;";
      List.iter
        (fun d ->
          F.linep sc "(* optional=%b, requires_presence=%b *)" d.optional
            d.requires_presence;
          if d.requires_presence then (
            assert d.optional;
            F.linep sc "%s=(match %s with" d.fname d.fname;
            F.linep sc "  | None -> %s" d.default_value;
            F.linep sc "  | Some v -> %s; v);"
              (Pb_codegen_util.presence_set ~bv:"_presence" ~idx:d.presence_idx
                 ())
          ) else if d.optional then (
            F.linep sc "%s=(match %s with" d.fname d.fname;
            F.linep sc "  | None -> %s" d.default_value;
            F.line sc "  | Some v -> v);"
          ) else
            F.linep sc "%s;" d.fname)
        fields_default_info);

  F.line sc "}"

let gen_unit ?and_ { Ot.er_name } sc =
  F.linep sc "%s default_%s = ()" (let_decl_of_and and_) er_name

let gen_variant ?and_ { Ot.v_name; Ot.v_constructors } sc =
  match v_constructors with
  | [] -> failwith "programmatic TODO error"
  | { Ot.vc_constructor; vc_field_type; _ } :: _ ->
    let decl = let_decl_of_and and_ in
    (match vc_field_type with
    | Ot.Vct_nullary ->
      F.linep sc "%s default_%s (): %s = %s" decl v_name v_name vc_constructor
    | Ot.Vct_non_nullary_constructor field_type ->
      let default_value =
        let field_name = v_name in
        default_value_of_field_type ~field_name field_type None
      in
      (* TODO need to fix the deault value *)
      F.linep sc "%s default_%s () : %s = %s (%s)" decl v_name v_name
        vc_constructor default_value)

let gen_const_variant ?and_ { Ot.cv_name; Ot.cv_constructors } sc =
  let first_constructor_name =
    match cv_constructors with
    | [] -> failwith "programmatic TODO error"
    | { Ot.cvc_name; _ } :: _ -> cvc_name
  in
  F.linep sc "%s default_%s () = (%s:%s)" (let_decl_of_and and_) cv_name
    first_constructor_name cv_name

let gen_struct_full ~with_mutable_records ?and_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record ?and_ r sc;
      if with_mutable_records then gen_record_mutable r sc;
      true
    | Ot.Variant v ->
      gen_variant ?and_ v sc;
      true
    | Ot.Const_variant v ->
      gen_const_variant v sc;
      true
    | Ot.Unit u ->
      gen_unit ?and_ u sc;
      true
  in
  has_encoded

let gen_struct ?and_ t sc =
  gen_struct_full ?and_ ~with_mutable_records:false t sc

let gen_sig_record sc { Ot.r_name; r_fields } =
  F.linep sc "val default_%s : " r_name;

  let fields_default_info : _ list =
    List.map (fun r_field -> record_field_default_info r_field) r_fields
  in

  F.sub_scope sc (fun sc ->
      List.iter
        (fun dinfo -> F.linep sc "?%s:%s ->" dinfo.fname dinfo.ftype)
        fields_default_info;
      F.line sc "unit ->";
      F.line sc r_name);
  let rn = r_name in
  F.linep sc "(** [default_%s ()] is the default value for type [%s] *)" rn rn

let gen_sig_unit sc { Ot.er_name } =
  F.linep sc "val default_%s : unit" er_name;

  let rn = er_name in
  F.linep sc "(** [default_%s ()] is the default value for type [%s] *)" rn rn

let gen_sig ?and_:_ t sc =
  let f type_name =
    F.linep sc "val default_%s : unit -> %s" type_name type_name;
    F.linep sc "(** [default_%s ()] is the default value for type [%s] *)"
      type_name type_name
  in

  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_sig_record sc r;
      true
    | Ot.Variant v ->
      f v.Ot.v_name;
      true
    | Ot.Const_variant { Ot.cv_name; _ } ->
      f cv_name;
      true
    | Ot.Unit u ->
      gen_sig_unit sc u;
      true
  in

  has_encoded

let ocamldoc_title = "Basic values"
let requires_mutable_records = false
