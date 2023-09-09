module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
module L = Pb_logger
open Pb_codegen_util

let gen_field field_type =
  match field_type with
  | Ot.Ft_user_defined_type udt ->
    let function_prefix = "pp" in
    function_name_of_user_defined ~function_prefix udt
  | Ot.Ft_wrapper_type { Ot.wt_type; _ } ->
    sp "Pbrt.Pp.pp_wrapper_%s" (string_of_basic_type ~for_pp:true wt_type)
  | _ -> sp "Pbrt.Pp.pp_%s" (string_of_field_type ~for_pp:true field_type)

let gen_record ?and_ { Ot.r_name; r_fields } sc =
  L.log "gen_pp, record_name: %s\n" r_name;

  F.line sc @@ sp "%s pp_%s fmt (v:%s) = " (let_decl_of_and and_) r_name r_name;
  F.sub_scope sc (fun sc ->
      F.line sc "let pp_i fmt () =";
      F.sub_scope sc (fun sc ->
          List.iteri
            (fun i record_field ->
              let first = i = 0 in
              let { Ot.rf_label; rf_field_type; _ } = record_field in

              let var_name = sp "v.%s" rf_label in
              match rf_field_type with
              | Ot.Rft_nolabel (field_type, _, _)
              | Ot.Rft_required (field_type, _, _, _) ->
                let field_string_of = gen_field field_type in
                F.line sc
                @@ sp "Pbrt.Pp.pp_record_field ~first:%b \"%s\" %s fmt %s;"
                     first rf_label field_string_of var_name
                (* Rft_required *)
              | Ot.Rft_optional (field_type, _, _, _) ->
                let field_string_of = gen_field field_type in
                F.line sc
                @@ sp
                     "Pbrt.Pp.pp_record_field ~first:%b \"%s\" \
                      (Pbrt.Pp.pp_option %s) fmt %s;"
                     first rf_label field_string_of var_name
                (* Rft_optional *)
              | Ot.Rft_repeated (rt, field_type, _, _, _) ->
                let field_string_of = gen_field field_type in
                (match rt with
                | Ot.Rt_list ->
                  F.line sc
                  @@ sp
                       "Pbrt.Pp.pp_record_field ~first:%b \"%s\" \
                        (Pbrt.Pp.pp_list %s) fmt %s;"
                       first rf_label field_string_of var_name
                | Ot.Rt_repeated_field ->
                  F.line sc
                  @@ sp
                       "Pbrt.Pp.pp_record_field ~first:%b \"%s\" \
                        (Pbrt.Pp.pp_list %s) fmt (Pbrt.Repeated_field.to_list \
                        %s);"
                       first rf_label field_string_of var_name)
                (* Rft_repeated_field *)
              | Ot.Rft_variant { Ot.v_name; v_constructors = _ } ->
                (* constructors are ignored because the pretty printing is completely
                 * delegated to the pretty print function associated with that variant.
                 * This is indeed different from the [decode]/[encode] functions which
                 * requires `inlining` the decoding/encoding logic within the record (This
                 * requirement is indeed comming from the imposed Protobuf format)
                 *)
                F.line sc
                @@ sp "Pbrt.Pp.pp_record_field ~first:%b \"%s\" %s fmt %s;"
                     first rf_label ("pp_" ^ v_name) var_name
                (* Rft_variant_field *)
              | Ot.Rft_associative (at, _, (key_type, _), (value_type, _)) ->
                let pp_runtime_function =
                  match at with
                  | Ot.At_list -> "pp_associative_list"
                  | Ot.At_hashtable -> "pp_hastable"
                in
                let pp_key = gen_field (Ot.Ft_basic_type key_type) in
                let pp_value = gen_field value_type in
                F.line sc
                @@ sp
                     "Pbrt.Pp.pp_record_field ~first:%b \"%s\" (Pbrt.Pp.%s %s \
                      %s) fmt %s;"
                     first rf_label pp_runtime_function pp_key pp_value var_name
              (* Associative_list *))
            r_fields);
      F.line sc "in";
      F.line sc "Pbrt.Pp.pp_brk pp_i fmt ()")

let gen_unit ?and_ { Ot.er_name } sc : unit =
  F.line sc
  @@ sp "%s pp_%s fmt (v:%s) = " (let_decl_of_and and_) er_name er_name;
  F.sub_scope sc (fun sc ->
      F.line sc "let pp_i fmt () =";
      F.sub_scope sc (fun sc -> F.line sc "Pbrt.Pp.pp_unit fmt ()");
      F.line sc "in";
      F.line sc "Pbrt.Pp.pp_brk pp_i fmt ()")

let gen_variant ?and_ { Ot.v_name; Ot.v_constructors } sc =
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) v_name v_name;
  F.sub_scope sc (fun sc ->
      F.line sc "match v with";
      List.iter
        (fun { Ot.vc_constructor; vc_field_type; _ } ->
          match vc_field_type with
          | Ot.Vct_nullary ->
            F.line sc
            @@ sp "| %s  -> Format.fprintf fmt \"%s\"" vc_constructor
                 vc_constructor
          | Ot.Vct_non_nullary_constructor field_type ->
            let field_string_of = gen_field field_type in
            F.line sc
            @@ sp "| %s x -> Format.fprintf fmt \"@[<hv2>%s(@,%%a)@]\" %s x"
                 vc_constructor vc_constructor field_string_of)
        v_constructors)

let gen_const_variant ?and_ { Ot.cv_name; cv_constructors } sc =
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) cv_name cv_name;
  F.sub_scope sc (fun sc ->
      F.line sc "match v with";
      List.iter
        (fun { Ot.cvc_name; _ } ->
          F.line sc @@ sp "| %s -> Format.fprintf fmt \"%s\"" cvc_name cvc_name)
        cv_constructors)

let gen_struct ?and_ t sc =
  let { Ot.spec; _ } = t in
  (match spec with
  | Ot.Record r -> gen_record ?and_ r sc
  | Ot.Variant v -> gen_variant ?and_ v sc
  | Ot.Const_variant v -> gen_const_variant ?and_ v sc
  | Ot.Unit u -> gen_unit ?and_ u sc);
  true

let gen_sig ?and_ t sc =
  let _ = and_ in
  let { Ot.spec; _ } = t in
  let f type_name =
    F.line sc
    @@ sp "val pp_%s : Format.formatter -> %s -> unit " type_name type_name;
    F.line sc @@ sp "(** [pp_%s v] formats v *)" type_name
  in
  (match spec with
  | Ot.Record { Ot.r_name; _ } -> f r_name
  | Ot.Variant v -> f v.Ot.v_name
  | Ot.Const_variant { Ot.cv_name; _ } -> f cv_name
  | Ot.Unit { Ot.er_name; _ } -> f er_name);
  true

let ocamldoc_title = "Formatters"

let plugin : Pb_codegen_plugin.t =
  let module P = struct
    let gen_sig = gen_sig
    let gen_struct = gen_struct
    let ocamldoc_title = ocamldoc_title
  end in
  (module P)
