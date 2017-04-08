
module Ot = Pb_codegen_ocaml_type 
module F = Pb_codegen_formatting

open Pb_codegen_util

let type_decl_of_and = function | Some () -> "and" | None -> "type" 

let is_imperative_type = function
  (*TODO Rename *)
  | Ot.Rft_nolabel _ 
  | Ot.Rft_required _ 
  | Ot.Rft_optional _ 
  | Ot.Rft_variant _ 
  | Ot.Rft_repeated (Ot.Rt_list, _, _, _, _) 
  | Ot.Rft_associative (Ot.At_list, _, _, _) -> false 
  | Ot.Rft_repeated (Ot.Rt_repeated_field,_, _, _, _) 
  | Ot.Rft_associative (Ot.At_hashtable, _, _, _) -> true


let gen_record_mutable module_prefix {Ot.r_name; r_fields} sc = 
  
  let field_prefix field_type = 
    if is_imperative_type field_type then "" else "mutable "
  in

  let r_name = Pb_codegen_util.mutable_record_name r_name in
  
  F.linep sc "type %s = {" r_name;
  F.scope sc (fun sc -> 
    List.iter (fun {Ot.rf_label; rf_field_type; _} ->  
      let prefix = field_prefix rf_field_type in 
      let type_ = 
        Pb_codegen_util.string_of_record_field_type 
          ~module_prefix rf_field_type 
      in 
      F.linep sc "%s%s : %s;" prefix rf_label type_
    ) r_fields;
  ); 
  F.line sc "}"

let gen_record ?and_ {Ot.r_name; r_fields } sc = 

  let field_prefix field_mutable = 
    if field_mutable then "mutable " else "" 
  in

  F.linep sc "%s %s = {" (type_decl_of_and and_) r_name;
  F.scope sc (fun sc -> 
    List.iter (fun {Ot.rf_label; rf_field_type; rf_mutable;} ->  
      let prefix = field_prefix rf_mutable in 
      let type_ = 
        Pb_codegen_util.string_of_record_field_type rf_field_type 
      in 
      F.linep sc "%s%s : %s;" prefix rf_label type_
    ) r_fields;
  ); 
  F.line sc "}"

let gen_variant ?and_ variant sc =  
  let {Ot.v_name; v_constructors; } = variant in

  F.linep sc "%s %s =" (type_decl_of_and and_) v_name; 

  F.scope sc (fun sc -> 
    List.iter (fun {Ot.vc_constructor; vc_field_type; _} ->
      match vc_field_type with
      | Ot.Vct_nullary -> F.linep sc "| %s" vc_constructor
      | Ot.Vct_non_nullary_constructor  field_type -> (
        let type_string = string_of_field_type field_type in 
        F.linep sc "| %s of %s" vc_constructor type_string 
      )
    ) v_constructors;
  )

let gen_const_variant ?and_ {Ot.cv_name; cv_constructors} sc = 
  F.linep sc "%s %s =" (type_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    List.iter (fun {Ot.cvc_name; _} ->
      F.linep sc "| %s " cvc_name
    ) cv_constructors;
  )

let print_ppx_extension {Ot.type_level_ppx_extension; _ } sc = 
  match type_level_ppx_extension with
  | None -> () 
  | Some ppx_content -> F.linep sc "[@@%s]" ppx_content

let gen_struct ?and_ t scope = 
  let {Ot.spec; _} = t in 
  begin match spec with 
    | Ot.Record r  -> gen_record ?and_ r scope 
    | Ot.Variant v -> gen_variant  ?and_ v scope 
    | Ot.Const_variant v -> gen_const_variant ?and_ v scope
  end; 
  print_ppx_extension t scope; 
  true

let gen_sig ?and_ t scope = 
  let {Ot.spec; _} = t in 
  begin match spec with 
    | Ot.Record r ->gen_record ?and_ r scope 
    | Ot.Variant v -> gen_variant  ?and_ v scope  
    | Ot.Const_variant v -> gen_const_variant ?and_ v scope 
  end; 
  print_ppx_extension t scope; 
  true

let ocamldoc_title = "Types"

let file_suffix = "types"
