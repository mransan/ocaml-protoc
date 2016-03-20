module T = Ocaml_types
module F = Fmt 
module E = Exception 
module L = Logger 
module Enc = Encoding_util

open Codegen_util

let gen_default_field field_name field_type field_default = 
  match field_type, field_default with 
  | T.User_defined_type t, _ -> 
    function_name_of_user_defined "default" t  ^ " ()"
  | T.Unit  , None -> "()"
  | T.String, None -> "\"\""
  | T.String, Some (Pbpt.Constant_string s) -> sp "\"%s\"" s 
  | T.Float , None -> "0." 
  | T.Float , Some (Pbpt.Constant_float f) -> string_of_float f
  | T.Int   , None -> "0"
  | T.Int   , Some (Pbpt.Constant_int i) -> string_of_int i
  | T.Int32 , None -> "0l"
  | T.Int32 , Some (Pbpt.Constant_int i) -> sp "%il" i
  | T.Int64 , None -> "0L"
  | T.Int64 , Some (Pbpt.Constant_int i) -> sp "%iL" i
  | T.Bytes , None -> "Bytes.create 64"  
  | T.Bytes , Some (Pbpt.Constant_string s) -> sp "Bytes.of_string \"%s\"" s  
  | T.Bool  , None -> "false"
  | T.Bool  , Some (Pbpt.Constant_bool b) -> string_of_bool b
  | _ -> E.invalid_default_value 
    ~field_name ~info:"invalid default type" ()

let gen_default_record  ?mutable_ ?and_ {T.record_name; fields } sc = 

  let record_name = match mutable_  with
    | None -> record_name 
    | Some () -> Codegen_util.mutable_record_name record_name 
  in 

  L.log "gen_pp, record_name: %s\n" record_name; 

  F.line sc @@ sp "%s default_%s () : %s = {" (let_decl_of_and and_) record_name record_name;
  F.scope sc (fun sc -> 
    List.iter (fun field -> 
      L.log "gen_pp field_name: %s\n" field.T.field_name;
     
      let { T.field_type; field_name; type_qualifier ; encoding_type } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        let field_default_of = gen_default_field field_name field_type encoding_type.Enc.default in 
        match type_qualifier with
        | T.No_qualifier -> 
          F.line sc @@ sp "%s = %s;" field_name field_default_of; 
        | T.Option -> (
          match encoding_type.Enc.default with
          | None -> F.line sc @@ sp "%s = None;" field_name
          | Some _ -> 
            F.line sc @@ sp "%s = Some (%s);" field_name field_default_of; 
        )  
        | T.List -> 
          F.line sc @@ sp "%s = [];" field_name; 
        | T.Repeated_field  -> 
          F.line sc @@ sp "%s = Pbrt.Repeated_field.make (%s);" field_name field_default_of ; 
      )
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (
        match variant_constructors with
        | []     -> failwith "programmatic TODO error" 
        | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
          match field_type with 
          | T.Unit -> F.line sc @@ sp "%s = %s;" field_name constructor_name 
          | _ -> (
            F.line sc @@ sp "%s = %s (%s);" 
              field_name constructor_name (gen_default_field field_name field_type encoding_type.Encoding_util.default)  
          )
      ) (* one of        *)
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone} -> (
        E.programmatic_error E.One_of_should_be_inlined_in_message
      )
    ) fields;          (* record fields *) 
  ); (* end of function *)
  F.line sc "}"

let gen_default_variant ?and_ {T.variant_name; T.variant_constructors ; T.variant_encoding = _ } sc = 
  match variant_constructors with
  | []     -> failwith "programmatic TODO error" 
  | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
    let decl = let_decl_of_and and_ in 
    match field_type with
    | T.Unit -> 
      F.line sc @@ sp "%s default_%s () : %s = %s" decl variant_name variant_name constructor_name 
    |  _ -> 
      let default_field = gen_default_field variant_name field_type encoding_type.Encoding_util.default in
      F.line sc @@ sp "%s default_%s () : %s = %s (%s)" 
         decl variant_name variant_name constructor_name default_field 

let gen_default_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } sc = 
  let first_constructor_name = match cvariant_constructors with
    | []            -> failwith "programmatic TODO error"
    | (name, _) ::_ -> name 
  in  
  F.line sc @@ sp "%s default_%s () = (%s:%s)" 
    (let_decl_of_and and_) cvariant_name first_constructor_name cvariant_name 

let gen_struct ?and_ t sc = 
  let (), has_encoded = match t with 
    | {T.spec = T.Record r  } ->
      (
        gen_default_record ?and_ r sc; 
        F.empty_line sc;
        gen_default_record ~mutable_:() ~and_:() r sc
      ), true 
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
      | T.Standalone -> gen_default_variant ?and_ v sc, true
      | T.Inlined_within_message -> (), false
    )
    | {T.spec = T.Const_variant v } -> gen_default_const_variant v sc, true
  in
  has_encoded

let gen_sig ?and_ t sc = 
  let f type_name =  
    F.line sc @@ sp "val default_%s : unit -> %s" type_name type_name;
    F.line sc @@ sp "(** [default_%s ()] is the default value for type [%s] *)" type_name type_name;
  in 
  let (), has_encoded = match t with 
    | {T.spec = T.Record {T.record_name ; _ } }-> f record_name, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
      | T.Standalone -> f v.T.variant_name, true
      | T.Inlined_within_message -> (), false
    ) 
    | {T.spec = T.Const_variant {T.cvariant_name ; _ ; } } -> f cvariant_name, true
  in
  has_encoded

let ocamldoc_title = "Default values" 
