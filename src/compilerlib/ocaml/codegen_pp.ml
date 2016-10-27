module T = Ocaml_types 
module F = Fmt
module L = Logger
module E = Exception

open Codegen_util

let gen_pp_field field_type = 
  match field_type with 
  | T.Ft_user_defined_type t -> function_name_of_user_defined "pp" t 
  | _ ->  sp "Pbrt.Pp.pp_%s" (string_of_field_type field_type) 


let gen_pp_record  ?and_ {T.r_name; r_fields} sc = 
  L.log "gen_pp, record_name: %s\n" r_name; 

  F.line sc @@ sp "%s pp_%s fmt (v:%s) = " (let_decl_of_and and_) r_name r_name;
  F.scope sc (fun sc ->
    F.line sc "let pp_i fmt () ="; 
    F.scope sc (fun sc -> 
      F.line sc "Format.pp_open_vbox fmt 1;";
      List.iter (fun record_field -> 

        let {T.rf_label; rf_field_type; _ } = record_field in 

        let var_name = sp "v.%s" rf_label in 
        match rf_field_type with 

        | T.Rft_required (field_type, _, _, _) -> ( 
          let field_string_of = gen_pp_field field_type in 
          F.line sc @@ sp 
            "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;" 
            rf_label field_string_of var_name
        ) (* Rft_required *)

        | T.Rft_optional (field_type, _, _, _) -> (
          let field_string_of = gen_pp_field field_type in 
          F.line sc @@ sp 
            "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_option %s) fmt %s;" 
            rf_label field_string_of var_name
        ) (* Rft_optional *) 

        | T.Rft_repeated_field (rt, field_type, _, _, _) ->  (
          let field_string_of = gen_pp_field field_type in 
          begin match rt with 
          | T.Rt_list -> (
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt %s;" 
              rf_label field_string_of var_name
          ) 
          | T.Rt_repeated_field -> (
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt (Pbrt.Repeated_field.to_list %s);" 
              rf_label field_string_of var_name
          ) 
          end
        ) (* Rft_repeated_field *)

        | T.Rft_variant_field {T.v_name; v_constructors = _ } -> (
          (* constructors are ignored because the pretty printing is completely 
           * delegated to the pretty print function associated with that variant. 
           * This is indeed different from the [decode]/[encode] functions which 
           * requires `inlining` the decoding/encoding logic within the record (This 
           * requirement is indeed comming from the imposed Protobuf format) 
           *)
          F.line sc @@ sp 
            "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;" 
            rf_label ("pp_" ^ v_name) var_name
        ) (* Rft_variant_field *)

        | T.Rft_associative_field (at, _, (key_type,_), (value_type, _)) -> (

          let pp_runtime_function = match at with
            | T.At_list -> "pp_associative_list"
            | T.At_hashtable -> "pp_hastable"
          in
          let pp_key = gen_pp_field (T.Ft_basic_type key_type) in 
          let pp_value = gen_pp_field value_type in 
          F.line sc @@ sp "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.%s %s %s) fmt %s;"
            rf_label pp_runtime_function pp_key pp_value var_name  
        ) (* Associative_list *)

      ) r_fields; 
      F.line sc "Format.pp_close_box fmt ()";
    );
    F.line sc "in";
    F.line sc "Pbrt.Pp.pp_brk pp_i fmt ()";
  )

let gen_pp_variant ?and_ {T.v_name; T.v_constructors; } sc = 
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) v_name v_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.vc_constructor;vc_field_type; _ } ->  
      match vc_field_type with
      | T.Vct_nullary -> ( 
        F.line sc @@ sp 
          "| %s  -> Format.fprintf fmt \"%s\"" 
          vc_constructor vc_constructor 
      )
      | T.Vct_non_nullary_constructor field_type -> (  
        let field_string_of = gen_pp_field field_type in 
        F.line sc @@ sp  
          "| %s x -> Format.fprintf fmt \"@[%s(%%a)@]\" %s x" 
          vc_constructor vc_constructor field_string_of 
      )
    ) v_constructors;
  )

let gen_pp_const_variant ?and_ {T.cv_name; T.cv_constructors; } sc = 
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) cv_name cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s -> Format.fprintf fmt \"%s\"" name name
    ) cv_constructors; 
  )

let gen_struct ?and_ t sc = 
  begin 
    match t with
    | {T.spec = T.Record r; _} -> gen_pp_record ?and_ r sc
    | {T.spec = T.Variant v; _ } -> gen_pp_variant ?and_ v sc
    | {T.spec = T.Const_variant v; _} -> gen_pp_const_variant ?and_ v sc
  end; 
  true

let gen_sig ?and_ t sc = 
  let _ = and_ in
  let f type_name =  
    F.line sc @@ sp "val pp_%s : Format.formatter -> %s -> unit " type_name type_name;
    F.line sc @@ sp "(** [pp_%s v] formats v *)" type_name;
  in 
  begin
    match t with 
    | {T.spec = T.Record {T.r_name; _}; _} -> f r_name
    | {T.spec = T.Variant v; _} -> f v.T.v_name
    | {T.spec = T.Const_variant {T.cv_name; _}; _} -> f cv_name
  end;
  true

let ocamldoc_title = "Formatters" 
