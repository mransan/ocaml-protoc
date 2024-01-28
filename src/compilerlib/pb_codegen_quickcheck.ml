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
  F.empty_line sc;
  F.linep sc
    "val quickcheck_tests_%s : ?gen:%s QCheck2.Gen.t -> unit -> QCheck2.Test.t \
     list"
    type_name type_name;
  F.linep sc
    "(** [quickcheck_tests_%s ?gen ()] builds a test suite for the type %S. \
     Inputs are generated with QuickCheck. Use [gen] to override the \
     generator. *)"
    type_name type_name;
  true

let gen_struct ?and_:_ t sc =
  let type_name = type_name t in
  F.linep sc "let quickcheck_%s =" type_name;
  F.linep sc "  { Pbrt_quickcheck.Type_class.";
  F.linep sc "    type_name = %S;" type_name;
  let field f = F.linep sc "    %s = %s_%s;" f f type_name in
  List.iter field
    [
      "pp";
      "gen";
      "equal";
      "encode_pb";
      "decode_pb";
      "encode_json";
      "decode_json";
    ];
  F.linep sc "  }";
  F.empty_line sc;
  F.linep sc "let quickcheck_tests_%s ?gen () =" type_name;
  F.linep sc "  Pbrt_quickcheck.Test.make ?gen quickcheck_%s" type_name;

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

let gen_all_tests_sig ts sc =
  F.line sc "val all_quickcheck_tests :";
  List.iter (fun t -> F.linep sc "    ?include_%s:bool ->" (type_name t)) ts;
  F.line sc "    unit -> QCheck2.Test.t list";
  F.linep sc
    "(** [all_quickcheck_tests ()] builds a test suite which, by default, \
     includes tests for all known types. Use [~include_test:false] to exclude \
     a particular test. *)"

let gen_all_tests_struct ts sc =
  F.line sc "let all_quickcheck_tests";
  List.iter (fun t -> F.linep sc "    ?(include_%s = true)" (type_name t)) ts;
  F.line sc "    () =";
  F.line sc "  List.flatten [";
  List.iter
    (fun t ->
      let type_name = type_name t in
      F.linep sc "    if include_%s then quickcheck_tests_%s () else [];"
        type_name type_name)
    ts;
  F.line sc "  ]"