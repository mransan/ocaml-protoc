module Pc = Protobuf_codec
module E  = Exception 
module L  = Logger 
module OCaml_types = Ocaml_types 

let type_name message_scope name = 
  let module S = String in  
  let all_names =  message_scope @ [name] in 
  match all_names with 
  | []     -> failwith "Programmatic error"
  | hd::[] -> S.lowercase hd 
  | _      -> S.concat "_" @@ List.map S.lowercase all_names

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let record_field_name s =
  String.lowercase s 

let empty = function | [] -> true | _ -> false 

let type_name_of_message field_message_scope message_scope message_name = 
  let module S = String in 

  let message_scope = 
    (* TODO this is a brute force method which only works for 
        field which are in the same namespaces as their types. 
     *) 
    if field_message_scope.Pbtt.namespaces = message_scope.Pbtt.namespaces 
    then {message_scope with 
      Pbtt.namespaces = [] 
    }
    else message_scope in  

  let {Pbtt.namespaces; Pbtt.message_names} = message_scope in 

  if empty namespaces && empty message_names 
  then S.lowercase message_name 
  else 

    let namespaces = List.map (fun s -> 
      S.capitalize @@ S.lowercase s
    ) namespaces in 
    let message_names = List.map S.lowercase message_names in 

    let module_prefix = match namespaces with
      | [] -> ""
      | _  -> 
        S.concat "." namespaces  ^ "."
    in 
    match message_names with
    | [] -> module_prefix ^ (S.lowercase message_name)
    | _  -> 
      module_prefix ^ 
      S.concat "_" message_names ^ 
      "_" ^
      S.lowercase message_name 


let get_type_name_from_all_messages field_message_scope all_types i = 
  let module S = String in 
  try 
    let t = Pbtt_util.type_of_id all_types i  in 
    let type_scope = Pbtt_util.type_scope_of_type t in 
    let type_name  = Pbtt_util.type_name_of_type  t in 
    type_name_of_message field_message_scope type_scope type_name 
  with | Not_found -> failwith "Programmatic error could not find type"

let compile_field ?as_constructor f type_qualifier message_scope all_types field = 
  let field_name = Pbtt_util.field_name field in 
  let encoding_type = Pbtt_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

  let field_encoding = Encoding_util.encoding_of_field_type all_types field in 
  let field_type   = match encoding_type with
    | Pbtt.Field_type_double  -> OCaml_types.Float
    | Pbtt.Field_type_float  ->  OCaml_types.Float
    | Pbtt.Field_type_int32  ->  OCaml_types.Int
    | Pbtt.Field_type_int64  ->  OCaml_types.Int
    | Pbtt.Field_type_uint32  -> OCaml_types.Int
    | Pbtt.Field_type_uint64 -> OCaml_types.Int
    | Pbtt.Field_type_sint32  -> OCaml_types.Int
    | Pbtt.Field_type_sint64  -> OCaml_types.Int
    | Pbtt.Field_type_fixed32  -> OCaml_types.Int
    | Pbtt.Field_type_fixed64  -> OCaml_types.Int
    | Pbtt.Field_type_sfixed32  -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed32" ~backend_name:"OCaml" () 
    | Pbtt.Field_type_sfixed64 -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed64" ~backend_name:"OCaml" () 
    | Pbtt.Field_type_bool  -> OCaml_types.Bool
    | Pbtt.Field_type_string  -> OCaml_types.String
    | Pbtt.Field_type_bytes  -> OCaml_types.Bytes
    | Pbtt.Field_type_type id -> 
      let name = get_type_name_from_all_messages message_scope all_types id in 
      OCaml_types.User_defined_type name 
  in 
  {
    OCaml_types.field_type; 
    OCaml_types.field_name; 
    OCaml_types.type_qualifier; 
    OCaml_types.encoding_type = f field_encoding ; 
  }

let compile_oneof all_types message_scope outer_message_name {Pbtt.oneof_name ; Pbtt.oneof_fields } = 
  let {Pbtt.message_names; _ } = message_scope in 
  let variant_name = type_name (message_names @ [outer_message_name]) oneof_name in 
  let constructors = List.map (fun field -> 
    (* TODO fix hard coding the empty_scope and rather
        pass down the appropriate scope.
      *)
    compile_field ~as_constructor:() (fun x -> x)  OCaml_types.No_qualifier message_scope all_types field 
  ) oneof_fields in 
  OCaml_types.({variant_name; constructors; })

let compile_message  
  (all_types: Pbtt.resolved Pbtt.proto) 
  (message: Pbtt.resolved Pbtt.message ) :
  OCaml_types.type_ list   = 

  let {
    Pbtt.message_scope;
    Pbtt.message_name; 
    Pbtt.message_body; 
    Pbtt.id = _ ; 
  } = message in 

  let {Pbtt.message_names; Pbtt.namespaces = _ } = message_scope in 
  let record_name = type_name message_names message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Pbtt.Message_field f -> (
      let type_qualifier = match Pbtt_util.field_label f with 
        | `Optional -> OCaml_types.Option 
        | `Required -> OCaml_types.No_qualifier
        | `Repeated -> OCaml_types.List
      in 
      (variants, (compile_field (fun x -> OCaml_types.Regular_field x) type_qualifier message_scope all_types f)::fields)
    )
    | Pbtt.Message_oneof_field f -> (
      let variant = compile_oneof all_types message_scope message_name f in 
      let field   = OCaml_types.({
        field_type =  User_defined_type (variant.variant_name); 
        field_name =  record_field_name f.Pbtt.oneof_name;
        type_qualifier = No_qualifier;
        encoding_type = One_of variant; 
      }) in 
      ((OCaml_types.Variant variant)::variants, field::fields) 
    )
  ) ([], []) message_body in 

  List.rev (OCaml_types.(Record {
    record_name; 
    fields = List.rev fields;
  }) :: variants)

let compile_enum {Pbtt.enum_name; Pbtt.enum_values; Pbtt.enum_scope; Pbtt.enum_id = _ } = 
  let {Pbtt.message_names; Pbtt.namespaces = _ } = enum_scope in 
  let variant_name = type_name message_names enum_name in 
  let constructors = List.map (fun {Pbtt.enum_value_name; Pbtt.enum_value_int} -> 
    (constructor_name enum_value_name,  enum_value_int)
  ) enum_values in 
  OCaml_types.(Const_variant {
    variant_name; 
    constructors;  
  })

let compile all_types = function 
  | Pbtt.Message m -> compile_message all_types m 
  | Pbtt.Enum    e -> [compile_enum e] 

module Codegen = struct 

end  
