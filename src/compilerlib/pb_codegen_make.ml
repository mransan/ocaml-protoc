module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
open Pb_codegen_util

type default_info = Pb_codegen_default.default_info = {
  fname: string;
  ftype: string;
  default_value: string;
  optional: bool;  (** Are we passing an option? *)
  rfp: Pb_codegen_ocaml_type.record_field_presence;
  in_bitfield: bool;
  bitfield_idx: int;
}

(** Obtain information about the fields *)
let fields_of_record { Ot.r_fields; _ } : Pb_codegen_default.default_info list =
  List.map
    (fun r_field ->
      let dinfo = Pb_codegen_default.record_field_default_info r_field in
      dinfo)
    r_fields

let gen_record ?and_ ({ Ot.r_name; _ } as r) sc : unit =
  let fields = fields_of_record r in
  let n_presence = List.filter (fun d -> d.in_bitfield) fields |> List.length in

  F.linep sc "%s make_%s " (let_decl_of_and and_) r_name;

  F.sub_scope sc (fun sc ->
      List.iter
        (fun (d : default_info) ->
          if d.optional then
            F.linep sc "?(%s:%s option)" d.fname d.ftype
          else
            F.linep sc "~(%s:%s) " d.fname d.ftype)
        fields;
      F.linep sc "() : %s  =" r_name);

  F.sub_scope sc (fun sc ->
      if n_presence > 0 then
        F.linep sc "let _presence = ref (Pbrt.Bitfield.create %d) in" n_presence;
      List.iter
        (fun d ->
          if d.optional then (
            F.linep sc "let %s=(match %s with" d.fname d.fname;
            if d.in_bitfield then (
              F.linep sc "| None -> %s" d.default_value;
              F.linep sc "| Some v -> _presence := %s; v) in"
                (Pb_codegen_util.presence_set ~bv:"!_presence"
                   ~idx:d.bitfield_idx ())
            ) else (
              F.linep sc "| None -> %s" d.default_value;
              F.line sc "| Some v -> v) in"
            )
          ) else
            ())
        fields;

      let strrec = ref "" in
      if n_presence > 0 then strrec := !strrec ^ "_presence= !_presence;";
      strrec := !strrec ^ String.concat ";" (List.map (fun d -> d.fname) fields);
      F.linep sc "{ %s }" !strrec)

let gen_struct ?and_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record ?and_ r sc;
      true
    | Ot.Const_variant _ | Ot.Variant _ | Ot.Unit _ ->
      (* nothing for variants *)
      false
  in
  has_encoded

let gen_sig_record sc ({ Ot.r_name; _ } as r) =
  F.linep sc "val make_%s : " r_name;

  let fields : _ list = fields_of_record r in

  F.sub_scope sc (fun sc ->
      List.iter
        (fun d ->
          if d.optional then
            F.linep sc "?%s:%s ->" d.fname d.ftype
          else
            F.linep sc "%s:%s ->" d.fname d.ftype)
        fields;
      F.line sc "unit ->";
      F.line sc r_name);
  let rn = r_name in
  F.linep sc "(** [make_%s … ()] is a builder for type [%s] *)" rn rn

let gen_sig ?and_:_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_sig_record sc r;
      true
    | Ot.Variant _ | Ot.Const_variant _ | Ot.Unit _ -> false
  in

  has_encoded

let ocamldoc_title = "Make functions"
let requires_mutable_records = false
