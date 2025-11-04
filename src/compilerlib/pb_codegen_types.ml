module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
open Pb_codegen_util

let type_decl_of_and = function
  | Some () -> "and"
  | None -> "type"

let gen_record ?and_ ~as_private { Ot.r_name; r_fields } sc =
  let len_bitfield =
    List.filter Ot.record_field_requires_bitfield r_fields |> List.length
  in
  let insert_bitfield = len_bitfield > 0 in
  let priv_qualifier =
    if as_private then
      "private "
    else
      ""
  in

  F.linep sc "%s %s = %s{" (type_decl_of_and and_) r_name priv_qualifier;
  F.sub_scope sc (fun sc ->
      if insert_bitfield then
        F.linep sc
          "mutable _presence: Pbrt.Bitfield.t; (** presence for %d fields *)"
          len_bitfield;
      List.iter
        (fun { Ot.rf_label; rf_field_type; _ } ->
          let type_ =
            Pb_codegen_util.string_of_record_field_type ~with_option:true
              rf_field_type
          in
          F.linep sc "mutable %s : %s;" rf_label type_)
        r_fields);
  F.line sc "}"

let gen_variant ?and_ variant sc =
  let { Ot.v_name; v_constructors; v_use_polyvariant } = variant in

  let openbrace =
    if v_use_polyvariant then (
      F.line sc "(* NOTE: too many constructors for a regular sum type *)";
      " ["
    ) else
      ""
  in
  F.linep sc "%s %s =%s" (type_decl_of_and and_) v_name openbrace;

  F.sub_scope sc (fun sc ->
      List.iter
        (fun { Ot.vc_constructor; vc_field_type; _ } ->
          match vc_field_type with
          | Ot.Vct_nullary -> F.linep sc "| %s" vc_constructor
          | Ot.Vct_non_nullary_constructor field_type ->
            let type_string = string_of_field_type field_type in
            F.linep sc "| %s of %s" vc_constructor type_string)
        v_constructors);
  if v_use_polyvariant then F.line sc "]";
  ()

let gen_const_variant ?and_ { Ot.cv_name; cv_constructors } sc =
  F.linep sc "%s %s =" (type_decl_of_and and_) cv_name;
  F.sub_scope sc (fun sc ->
      List.iter
        (fun { Ot.cvc_name; _ } -> F.linep sc "| %s " cvc_name)
        cv_constructors)

let gen_unit ?and_ { Ot.er_name } sc =
  F.linep sc "%s %s = unit" (type_decl_of_and and_) er_name

let print_ppx_extension { Ot.type_level_ppx_extension; _ } sc =
  match type_level_ppx_extension with
  | None -> ()
  | Some ppx_content -> F.linep sc "[@@%s]" ppx_content

let gen_struct_full ?and_ t scope =
  let { Ot.spec; _ } = t in
  (match spec with
  | Ot.Record r -> gen_record ?and_ ~as_private:false r scope
  | Ot.Variant v -> gen_variant ?and_ v scope
  | Ot.Const_variant v -> gen_const_variant ?and_ v scope
  | Ot.Unit v -> gen_unit ?and_ v scope);
  print_ppx_extension t scope;

  true

let gen_struct ?and_ t sc = gen_struct_full ?and_ t sc

let gen_sig ?and_ t scope =
  let { Ot.spec; _ } = t in
  (match spec with
  | Ot.Record r -> gen_record ?and_ ~as_private:true r scope
  | Ot.Variant v -> gen_variant ?and_ v scope
  | Ot.Const_variant v -> gen_const_variant ?and_ v scope
  | Ot.Unit v -> gen_unit ?and_ v scope);
  print_ppx_extension t scope;
  true

let ocamldoc_title = "Types"
