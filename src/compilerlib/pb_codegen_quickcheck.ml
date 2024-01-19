module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let type_name t =
  match t with
  | { Ot.spec = Ot.Record { Ot.r_name; _ }; _ } -> r_name
  | { Ot.spec = Ot.Variant v; _ } -> v.Ot.v_name
  | { Ot.spec = Ot.Const_variant { Ot.cv_name; _ }; _ } -> cv_name
  | { Ot.spec = Ot.Unit { Ot.er_name; _ }; _ } -> er_name

let gen_sig ?and_:_ t sc =
  let type_name = type_name t in
  F.linep sc "val quickcheck_%s : %s Pbrt_quickcheck.Type_class.t" type_name
    type_name;
  F.linep sc
    "(** [quickcheck_%s] contains helpers to test the type %s with quickcheck \
     *)"
    type_name type_name;
  true

let gen_struct ?and_:_ t sc =
  let type_name = type_name t in
  F.linep sc "let quickcheck_%s =" type_name;
  F.linep sc "  { Pbrt_quickcheck.Type_class.";
  let field f = F.linep sc "    %s = %s_%s;" f f type_name in
  List.iter field
    [ "pp"; "equal"; "encode_pb"; "decode_pb"; "encode_json"; "decode_json" ];
  F.linep sc "  }";

  true

let ocamldoc_title = "QuickCheck"
let requires_mutable_records = false

let plugin : Pb_codegen_plugin.t =
  let module P = struct
    let gen_sig = gen_sig
    let gen_struct = gen_struct
    let ocamldoc_title = ocamldoc_title
    let requires_mutable_records = requires_mutable_records
  end in
  (module P)
