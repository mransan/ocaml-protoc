
module T = Ocaml_types
module F = Fmt

open Codegen_util

let type_decl_of_and = function | Some _ -> "and" | None -> "type" 

let gen_type_record ?mutable_ ?and_ {T.record_name; fields } sc = 
  let field_prefix = match mutable_ with
    | None    -> (fun _ -> "") 
    | Some () -> (function 
      | T.Repeated_field -> "" 
        (* Pbrt.Repeated_field.t is already a mutable data type. 
         *)
      | _                -> "mutable "
    ) 
  in
  F.line sc @@ sp "%s %s = {" (type_decl_of_and and_) record_name;
  F.scope sc (fun sc -> 
    List.iter (fun {T.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type ~type_qualifier field_type in 
      F.line sc @@ sp "%s%s : %s;" (field_prefix type_qualifier) field_name type_name
    ) fields;
  ); 
  F.line sc "}"

let gen_type_variant ?and_ variant sc =  
  let {T.variant_name; variant_constructors; variant_encoding = _ } = variant in
  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) variant_name; 
  F.scope sc (fun sc -> 
    List.iter (fun {T.field_name; field_type; type_qualifier; _ } -> 
      match field_type with
      | T.Unit -> F.line sc @@ sp "| %s" field_name 
      | _ -> (
        let type_name = string_of_field_type ~type_qualifier field_type in 
         F.line sc @@ sp "| %s of %s" field_name type_name
      )
    ) variant_constructors;
  )

let gen_type_const_variant ?and_ {T.cvariant_name; cvariant_constructors } sc = 
  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s " name
    ) cvariant_constructors;
  )

let gen_struct ?and_ t scope = 
  begin
    match t with 
    | {T.spec = T.Record r; _ } -> gen_type_record ~mutable_:() ?and_ r scope  
    | {T.spec = T.Variant v; _ } -> gen_type_variant  ?and_ v scope  
    | {T.spec = T.Const_variant v; _ } -> gen_type_const_variant ?and_ v scope 
  end; 
  true

let gen_sig = gen_struct 

let ocamldoc_title = "Types"
