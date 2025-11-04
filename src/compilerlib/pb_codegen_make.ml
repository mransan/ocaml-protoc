module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

type default_info = Pb_codegen_default.default_info = {
  fname: string;
  ftype: string;
  ftype_underlying: string;
  default_value: string;
  optional: bool;  (** Are we passing an option? *)
  rfp: Pb_codegen_ocaml_type.record_field_presence;
  bitfield_idx: int;
}

(** Obtain information about the fields *)
let fields_of_record { Ot.r_fields; _ } : Pb_codegen_default.default_info list =
  List.map
    (fun r_field ->
      let dinfo = Pb_codegen_default.record_field_default_info r_field in
      dinfo)
    r_fields

let gen_record ({ Ot.r_name; _ } as r) sc : unit =
  let fields = fields_of_record r in

  (* generate [has_field] accessors *)
  List.iter
    (fun (d : default_info) ->
      match d.rfp with
      | Rfp_bitfield _ ->
        F.linep sc "let[@inline] %s_has_%s (self:%s) : bool = %s" r_name d.fname
          r_name
          (Pb_codegen_util.presence_get ~bv:"self._presence" ~idx:d.bitfield_idx
             ())
      | _ -> ())
    fields;
  F.line sc "";

  (* generate [set_field] accessors *)
  List.iter
    (fun (d : default_info) ->
      F.linep sc "let[@inline] %s_set_%s (self:%s) (x:%s) : unit =" r_name
        d.fname r_name d.ftype_underlying;
      F.sub_scope sc (fun sc ->
          match d.rfp with
          | Rfp_bitfield idx ->
            F.linep sc "self._presence <- %s; self.%s <- x"
              (Pb_codegen_util.presence_set ~bv:"self._presence" ~idx ())
              d.fname
          | Rfp_wrapped_option -> F.linep sc "self.%s <- Some x" d.fname
          | Rfp_repeated | Rfp_always -> F.linep sc "self.%s <- x" d.fname))
    fields;
  F.line sc "";

  F.linep sc "let copy_%s (self:%s) : %s =" r_name r_name r_name;
  F.sub_scope sc (fun sc ->
      let field0 = List.hd fields in
      F.linep sc "{ self with %s = self.%s }" field0.fname field0.fname);
  F.line sc "";

  F.linep sc "let make_%s " r_name;
  F.sub_scope sc (fun sc ->
      List.iter
        (fun (d : default_info) ->
          match d.rfp with
          | Rfp_bitfield _ -> F.linep sc "?(%s:%s option)" d.fname d.ftype
          | Rfp_wrapped_option -> F.linep sc "?(%s:%s)" d.fname d.ftype
          | Rfp_repeated -> F.linep sc "?(%s=%s)" d.fname d.default_value
          | Rfp_always -> F.linep sc "~(%s:%s) " d.fname d.ftype)
        fields;
      F.linep sc "() : %s  =" r_name);
  F.sub_scope sc (fun sc ->
      F.linep sc "let _res = default_%s () in" r_name;
      List.iter
        (fun d ->
          if d.optional then (
            F.linep sc "(match %s with" d.fname;
            F.linep sc "| None -> ()";
            F.linep sc "| Some v -> %s_set_%s _res v);" r_name d.fname
          ) else
            F.linep sc "%s_set_%s _res %s;" r_name d.fname d.fname)
        fields;
      F.line sc "_res");

  ()

let gen_struct ?and_:_ ~mode:_ t sc =
  let { Ot.spec; _ } = t in

  let has_encoded =
    match spec with
    | Ot.Record r ->
      gen_record r sc;
      true
    | Ot.Const_variant _ | Ot.Variant _ | Ot.Unit _ ->
      (* nothing for variants *)
      false
  in
  has_encoded

let gen_sig_record sc ({ Ot.r_name; _ } as r) =
  let fields : _ list = fields_of_record r in

  F.linep sc "val make_%s : " r_name;
  F.sub_scope sc (fun sc ->
      List.iter
        (fun d ->
          match d.rfp with
          | Rfp_bitfield _ | Rfp_wrapped_option | Rfp_repeated ->
            F.linep sc "?%s:%s ->" d.fname d.ftype_underlying
          | Rfp_always -> F.linep sc "%s:%s ->" d.fname d.ftype)
        fields;
      F.line sc "unit ->";
      F.line sc r_name);
  let rn = r_name in
  F.linep sc "(** [make_%s … ()] is a builder for type [%s] *)" rn rn;

  F.line sc "";
  F.linep sc "val copy_%s : %s -> %s" r_name r_name r_name;

  List.iter
    (fun (d : default_info) ->
      (match d.rfp with
      | Rfp_bitfield _ ->
        F.line sc "";
        F.linep sc "val %s_has_%s : %s -> bool" r_name d.fname r_name;
        F.linep sc "  (** presence of field %S in [%s] *)" d.fname r_name
      | _ -> ());

      F.line sc "";
      F.linep sc "val %s_set_%s : %s -> %s -> unit" r_name d.fname r_name
        d.ftype_underlying;
      F.linep sc "  (** set field %s in %s *)" d.fname r_name)
    fields;

  ()

let gen_sig ?and_:_ ~mode:_ t sc =
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
