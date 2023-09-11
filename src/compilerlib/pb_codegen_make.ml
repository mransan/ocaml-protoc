module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
open Pb_codegen_util

(** Is this field optional enough that we give it a default value? *)
let field_is_optional (r_field : Ot.record_field) : bool =
  match r_field.rf_field_type with
  | Rft_optional _ -> true
  | _ -> true

(** Obtain information about the fields *)
let fields_of_record { Ot.r_fields; _ } :
    (string * string * [ `Optional of _ | `Required ]) list =
  List.map
    (fun r_field ->
      let fname, fdefault, ftype =
        Pb_codegen_default.record_field_default_info r_field
      in
      if field_is_optional r_field then
        fname, ftype, `Optional fdefault
      else
        fname, ftype, `Required)
    r_fields

let gen_record ?and_ ({ Ot.r_name; _ } as r) sc : unit =
  let fields = fields_of_record r in

  F.linep sc "%s make_%s " (let_decl_of_and and_) r_name;

  F.sub_scope sc (fun sc ->
      List.iter
        (fun (fname, ftype, d) ->
          match d with
          | `Required -> F.linep sc "~(%s:%s)" fname ftype
          | `Optional fvalue ->
            F.linep sc "?%s:((%s:%s) = %s)" fname fname ftype fvalue)
        fields;
      F.linep sc "() : %s  = {" r_name);

  F.sub_scope sc (fun sc ->
      List.iter (fun (fname, _, _) -> F.linep sc "%s;" fname) fields);

  F.line sc "}"

let gen_unit ?and_ { Ot.er_name } sc =
  F.linep sc "%s make_%s = ()" (let_decl_of_and and_) er_name

let gen_struct ?and_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record ?and_ r sc;
      true
    | Ot.Const_variant _ | Ot.Variant _ ->
      (* nothing for variants *)
      false
    | Ot.Unit u ->
      gen_unit ?and_ u sc;
      true
  in
  has_encoded

let gen_sig_record sc ({ Ot.r_name; _ } as r) =
  F.linep sc "val make_%s : " r_name;

  let fields : _ list = fields_of_record r in

  F.sub_scope sc (fun sc ->
      List.iter
        (fun (field_name, field_type, d) ->
          match d with
          | `Optional _ -> F.linep sc "?%s:%s ->" field_name field_type
          | `Required -> F.linep sc "%s:%s ->" field_name field_type)
        fields;
      F.line sc "unit ->";
      F.line sc r_name);
  let rn = r_name in
  F.linep sc "(** [make_%s … ()] is a builder for type [%s] *)" rn rn

let gen_sig_unit sc { Ot.er_name } =
  F.linep sc "val make_%s : unit" er_name;

  let rn = er_name in
  F.linep sc "(** [make_%s ()] is a builder for type [%s] *)" rn rn

let gen_sig ?and_:_ t sc =
  let f type_name =
    F.linep sc "val make_%s : unit -> %s" type_name type_name;
    F.linep sc "(** [make_%s … ()] is a builder for type [%s] *)" type_name
      type_name
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

let ocamldoc_title = "Make functions"
let requires_mutable_records = false
