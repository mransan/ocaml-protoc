module T = Ocaml_types

let sp x =  Printf.sprintf x 
(** [sp x] same as sprintf but prefixed with new line *)

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let string_of_field_type ?type_qualifier:(type_qualifier = T.No_qualifier) field_type = 
  let s = match field_type with 
    | T.Unit -> "unit"
    | T.String -> "string"
    | T.Float  -> "float"
    | T.Int    -> "int"
    | T.Int32  -> "int32"
    | T.Int64  -> "int64"
    | T.Bytes  -> "bytes"
    | T.Bool   -> "bool"
    | T.User_defined_type {T.module_ = None; T.type_name} -> type_name
    | T.User_defined_type {T.module_ = Some module_; T.type_name} -> module_ ^ "." ^ type_name
  in
  match type_qualifier with 
  | T.No_qualifier -> s 
  | T.Option       -> s ^ " option"
  | T.List         -> s ^ " list"
  | T.Repeated_field -> s ^ " Pbrt.Repeated_field.t"
 
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)
let function_name_of_user_defined prefix = function 
  | {T.module_ = Some module_; T.type_name} -> 
    sp "%s.%s_%s" module_ prefix type_name
  | {T.module_ = None; T.type_name} -> 
    sp "%s_%s" prefix type_name 

let caml_file_name_of_proto_file_name proto = 
  let splitted = Util.rev_split_by_char '.' proto in 
  if List.length splitted < 2 || 
     List.hd splitted <> "proto" 
  then failwith "Proto file has no valid extension"
  else 
    String.concat "_" @@ List.rev @@ ("pb" :: (List.tl splitted)) 

