module T = Ocaml_types 
module F = Fmt
module L = Logger
module E = Exception

open Codegen_util

let gen_pp_field field_type = 
  match field_type with 
  | T.User_defined_type t -> function_name_of_user_defined "pp" t 
  | _ ->  sp "Pbrt.Pp.pp_%s" (string_of_field_type field_type) 

let gen_pp_record  ?and_ {T.record_name; fields } sc = 
  L.log "gen_pp, record_name: %s\n" record_name; 

  F.line sc @@ sp "%s pp_%s fmt (v:%s) = " (let_decl_of_and and_) record_name
  record_name;
  F.scope sc (fun sc ->
    F.line sc "let pp_i fmt () ="; 
    F.scope sc (fun sc -> 
      F.line sc "Format.pp_open_vbox fmt 1;";
      List.iter (fun field -> 

        let {
          T.field_type; 
          field_name; 
          type_qualifier ; 
          encoding = record_field_encoding} = field in 

        let x:string = sp "v.%s" field_name in 
        match record_field_encoding with 
        | T.Regular_field _ -> ( 
          let field_string_of = gen_pp_field field_type in 
          match type_qualifier with
          | T.No_qualifier -> 
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;" 
              field_name field_string_of x
          | T.Option -> 
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_option %s) fmt %s;" 
              field_name field_string_of x
          | T.List -> 
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt %s;" 
              field_name field_string_of x
          | T.Repeated_field -> 
            F.line sc @@ sp 
              "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt (Pbrt.Repeated_field.to_list %s);" 
              field_name field_string_of x
        )
        | T.One_of {T.variant_constructors = _ ; variant_name; T.variant_encoding = T.Inlined_within_message} -> (
          F.line sc @@ sp 
            "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;" 
            field_name ("pp_" ^ variant_name) x
        )
        | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone } -> (
          E.programmatic_error E.One_of_should_be_inlined_in_message
        )
      ) fields; 
      F.line sc "Format.pp_close_box fmt ()";
    );
    F.line sc "in";
    F.line sc "Pbrt.Pp.pp_brk pp_i fmt ()";
  )

let gen_pp_variant ?and_ {T.variant_name; T.variant_constructors; T.variant_encoding = _ } sc = 
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) variant_name variant_name;
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.field_type; field_name; _ } ->
      let field_string_of = gen_pp_field field_type in 
      match field_type with
      | T.Unit -> 
        F.line sc @@ sp 
          "| %s  -> Format.fprintf fmt \"%s\"" 
          field_name field_name 
      | _ -> 
        F.line sc @@ sp  
          "| %s x -> Format.fprintf fmt \"@[%s(%%a)@]\" %s x" 
          field_name field_name field_string_of 
    ) variant_constructors ;
  )

let gen_pp_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } sc = 
  F.line sc @@ sp "%s pp_%s fmt (v:%s) =" (let_decl_of_and and_) cvariant_name cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s -> Format.fprintf fmt \"%s\"" name name
    ) cvariant_constructors; 
  )

let gen_struct ?and_ t sc = 
  begin 
    match t with
    | {T.spec = T.Record r  } -> gen_pp_record ?and_ r sc
    | {T.spec = T.Variant v } -> gen_pp_variant ?and_ v sc
    | {T.spec = T.Const_variant v } -> gen_pp_const_variant ?and_ v sc
  end; 
  true

let gen_sig ?and_ t sc = 
  let f type_name =  
    F.line sc @@ sp "val pp_%s : Format.formatter -> %s -> unit " type_name type_name;
    F.line sc @@ sp "(** [pp_%s v] formats v] *)" type_name;
  in 
  begin
    match t with 
    | {T.spec = T.Record {T.record_name ; _ } } -> f record_name 
    | {T.spec = T.Variant v } -> f v.T.variant_name 
    | {T.spec = T.Const_variant {T.cvariant_name; _ ; } } -> f cvariant_name
  end;
  true

let ocamldoc_title = "Formatters" 
