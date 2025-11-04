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
let default_value_of_field_type ?field_name field_type field_default : string =
  match field_type with
  | Ot.Ft_user_defined_type udt ->
    let function_prefix = "default" in
    let f = function_name_of_user_defined ~function_prefix udt in
    Printf.sprintf "%s ()" f
  | Ot.Ft_unit -> "()"
  | Ot.Ft_basic_type bt ->
    default_value_of_basic_type ?field_name bt field_default
  | Ot.Ft_wrapper_type _ -> "None"

type default_info = {
  fname: string;
  ftype: string;
  ftype_underlying: string;
      (** Type of the field without [option] around for optionals *)
  default_value: string;  (** Code for the default value *)
  optional: bool;  (** Are we passing an option? *)
  rfp: Ot.record_field_presence;
  bitfield_idx: int;
}

(** This function returns [(field_name, field_default_value, field_type)] for a
    record field. *)
let record_field_default_info (record_field : Ot.record_field) : default_info =
  let { Ot.rf_label; Ot.rf_field_type; _ } = record_field in
  let type_string =
    Pb_codegen_util.string_of_record_field_type ~with_option:true rf_field_type
  in
  let type_string_underlying =
    Pb_codegen_util.string_of_record_field_type ~with_option:false rf_field_type
  in
  let field_name = rf_label in

  let dfvft field_type field_default =
    default_value_of_field_type ~field_name field_type field_default
  in

  let default_value_of_field_type = function
    | Ot.Rft_nolabel (field_type, _, _) -> dfvft field_type None
    | Ot.Rft_required (field_type, _, _, default_value) ->
      dfvft field_type default_value
    | Ot.Rft_optional (field_type, _, _, default_value) ->
      (match default_value with
      | None -> "None"
      | Some _ -> sp "Some (%s)" @@ dfvft field_type default_value)
    | Ot.Rft_repeated (rt, field_type, _, _, _) ->
      (match rt with
      | Ot.Rt_list -> "[]"
      | Ot.Rt_repeated_field ->
        sp "Pbrt.Repeated_field.make (%s)" (dfvft field_type None))
    | Ot.Rft_associative (at, _, _, _) ->
      (match at with
      | Ot.At_list -> "[]"
      | Ot.At_hashtable -> "Hashtbl.create 8")
    | Ot.Rft_variant { Ot.v_constructors; _ } ->
      (* TODO This initial value could be configurable either via
         the default function or via a protobuf option. *)
      (match v_constructors with
      | [] -> assert false
      | { Ot.vc_constructor; vc_field_type; _ } :: _ ->
        (match vc_field_type with
        | Ot.Vct_nullary -> vc_constructor
        | Ot.Vct_non_nullary_constructor field_type ->
          sp "%s (%s)" vc_constructor (dfvft field_type None)))
  in

  let default_value, optional =
    match record_field.rf_presence with
    | Ot.Rfp_wrapped_option -> "None", true
    | Ot.Rfp_bitfield _ -> default_value_of_field_type rf_field_type, true
    | Ot.Rfp_always -> default_value_of_field_type rf_field_type, false
    | Ot.Rfp_repeated -> default_value_of_field_type rf_field_type, false
  in

  let bitfield_idx =
    match record_field.rf_presence with
    | Ot.Rfp_bitfield idx -> idx
    | _ -> -1
  in

  {
    fname = field_name;
    default_value;
    ftype = type_string;
    ftype_underlying = type_string_underlying;
    optional;
    rfp = record_field.rf_presence;
    bitfield_idx;
  }

let in_bitfield (d : default_info) : bool =
  match d.rfp with
  | Rfp_bitfield _ -> true
  | _ -> false

let gen_record { Ot.r_name; r_fields } sc : unit =
  let fields_default_info =
    List.map (fun r_field -> record_field_default_info r_field) r_fields
  in
  let len_bitfield =
    List.filter in_bitfield fields_default_info |> List.length
  in

  F.linep sc "let default_%s (): %s =" r_name r_name;

  F.linep sc "{";
  F.sub_scope sc (fun sc ->
      (* add bitfield *)
      if len_bitfield > 0 then F.linep sc "_presence=Pbrt.Bitfield.empty;";
      List.iter
        (fun d ->
          (*F.linep sc "(* optional=%b, in_bitfield=%b *)" d.optional d.in_bitfield;*)
          F.linep sc "%s=%s;" d.fname d.default_value)
        fields_default_info);

  F.line sc "}"

let gen_unit { Ot.er_name } sc =
  F.linep sc "let default_%s : %s = ()" er_name er_name

let gen_variant { Ot.v_name; Ot.v_constructors; v_use_polyvariant = _ } sc =
  match v_constructors with
  | [] -> failwith "programmatic TODO error"
  | { Ot.vc_constructor; vc_field_type; _ } :: _ ->
    (match vc_field_type with
    | Ot.Vct_nullary ->
      F.linep sc "let default_%s (): %s = %s" v_name v_name vc_constructor
    | Ot.Vct_non_nullary_constructor field_type ->
      let default_value =
        let field_name = v_name in
        default_value_of_field_type ~field_name field_type None
      in
      (* TODO need to fix the default value *)
      F.linep sc "let default_%s (): %s = %s (%s)" v_name v_name vc_constructor
        default_value)

let gen_const_variant { Ot.cv_name; Ot.cv_constructors } sc =
  let first_constructor_name =
    match cv_constructors with
    | [] -> failwith "programmatic TODO error"
    | { Ot.cvc_name; _ } :: _ -> cvc_name
  in
  F.linep sc "let default_%s () = (%s:%s)" cv_name first_constructor_name
    cv_name

let gen_struct_full ?and_:_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record r sc;
      true
    | Ot.Variant v ->
      gen_variant v sc;
      true
    | Ot.Const_variant v ->
      gen_const_variant v sc;
      true
    | Ot.Unit u ->
      gen_unit u sc;
      true
  in
  has_encoded

let gen_struct ?and_ t sc = gen_struct_full ?and_ t sc

let gen_sig_record sc { Ot.r_name; _ } : unit =
  F.linep sc "val default_%s : unit -> %s " r_name r_name;
  F.linep sc "(** [default_%s ()] is a new empty value for type [%s] *)" r_name
    r_name

let gen_sig_unit sc { Ot.er_name } =
  F.linep sc "val default_%s : unit" er_name;

  let rn = er_name in
  F.linep sc "(** [default_%s] is the default value for type [%s] *)" rn rn

let gen_sig ?and_:_ t sc =
  let gen_default_fun_ type_name =
    F.linep sc "val default_%s : unit -> %s" type_name type_name;
    F.linep sc "(** [default_%s ()] is a new empty value for type [%s] *)"
      type_name type_name
  in

  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_sig_record sc r;
      true
    | Ot.Variant v ->
      gen_default_fun_ v.Ot.v_name;
      true
    | Ot.Const_variant { Ot.cv_name; _ } ->
      gen_default_fun_ cv_name;
      true
    | Ot.Unit u ->
      gen_sig_unit sc u;
      true
  in

  has_encoded

let ocamldoc_title = "Basic values"
