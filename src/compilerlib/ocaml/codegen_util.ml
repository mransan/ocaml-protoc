module T = Ocaml_types

let sp x =  Printf.sprintf x 
(** [sp x] same as sprintf but prefixed with new line *)

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let string_of_basic_type = function 
  | T.Bt_string -> "string"
  | T.Bt_float  -> "float"
  | T.Bt_int    -> "int"
  | T.Bt_int32  -> "int32"
  | T.Bt_int64  -> "int64"
  | T.Bt_bytes  -> "bytes"
  | T.Bt_bool   -> "bool"

let string_of_user_defined = function 
  | {T.udt_module = None; T.udt_type_name; _ } -> 
    udt_type_name 
  | {T.udt_module = Some module_; T.udt_type_name; _ } -> 
    module_ ^ "." ^ udt_type_name 

let string_of_field_type = function 
  | T.Ft_unit -> "unit"
  | T.Ft_basic_type bt -> string_of_basic_type bt 
  | T.Ft_user_defined_type ud -> string_of_user_defined ud  

let string_of_repeated_type = function
  | T.Rt_list -> "list"
  | T.Rt_repeated_field -> "Pbrt.Repeated_field.t"

let string_of_associative_type = function
  | T.At_list -> "list"
  | T.At_hashtable -> "Hashtbl.t"

let string_of_record_field_type = function
  | T.Rft_nolabel (field_type, _, _)
  | T.Rft_required (field_type, _, _, _) -> 
      string_of_field_type field_type
  | T.Rft_optional (field_type, _, _, _) -> 
      (string_of_field_type field_type) ^ " option"
  | T.Rft_repeated_field (rt, field_type, _, _,_) -> 
      (string_of_field_type field_type) ^ " " ^ (string_of_repeated_type rt)
  | T.Rft_associative_field (T.At_list, _, (key_type, _), (value_type, _)) -> 
      Printf.sprintf "(%s * %s) %s" 
        (string_of_basic_type key_type)
        (string_of_field_type value_type) 
        (string_of_associative_type T.At_list) 
  | T.Rft_associative_field (T.At_hashtable, _, (key_type, _), (value_type, _)) -> 
      Printf.sprintf "(%s, %s) %s" 
        (string_of_basic_type key_type)
        (string_of_field_type value_type) 
        (string_of_associative_type T.At_hashtable) 
  | T.Rft_variant_field {T.v_name; _ } -> v_name
 
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)
let function_name_of_user_defined prefix = function 
  | {T.udt_module = Some module_; T.udt_type_name; _} -> 
    sp "%s.%s_%s" module_ prefix udt_type_name 
  | {T.udt_module = None; T.udt_type_name; _} -> 
    sp "%s_%s" prefix udt_type_name 

let caml_file_name_of_proto_file_name proto = 
  let splitted = Util.rev_split_by_char '.' proto in 
  if List.length splitted < 2 || 
     List.hd splitted <> "proto" 
  then failwith "Proto file has no valid extension"
  else 
    String.concat "_" @@ List.rev @@ ("pb" :: (List.tl splitted)) 

let mutable_record_name s = s ^ "_mutable" 

let string_of_payload_kind ?capitalize payload_kind packed = 
  let s = match payload_kind,  packed with
  | Ocaml_types.Pk_varint _ , false -> "varint"
  | Ocaml_types.Pk_bits32   , false -> "bits32"
  | Ocaml_types.Pk_bits64   , false -> "bits64"
  | Ocaml_types.Pk_bytes    , _ -> "bytes"
  | Ocaml_types.Pk_varint _ , true 
  | Ocaml_types.Pk_bits32   , true
  | Ocaml_types.Pk_bits64   , true  -> "bytes"
  in 
  match capitalize with
  | None -> s 
  | Some () -> String.capitalize s [@@ocaml.warning "-3"] 

