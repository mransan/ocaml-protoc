module Ot = Pb_codegen_ocaml_type

let sp x =  Printf.sprintf x 
(** [sp x] same as sprintf but prefixed with new line *)

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let string_of_basic_type = function 
  | Ot.Bt_string -> "string"
  | Ot.Bt_float  -> "float"
  | Ot.Bt_int    -> "int"
  | Ot.Bt_int32  -> "int32"
  | Ot.Bt_int64  -> "int64"
  | Ot.Bt_bytes  -> "bytes"
  | Ot.Bt_bool   -> "bool"

let string_of_user_defined = function 
  | {Ot.udt_module = None; Ot.udt_type_name; _ } -> 
    udt_type_name 
  | {Ot.udt_module = Some module_; Ot.udt_type_name; _ } -> 
    module_ ^ "." ^ udt_type_name 

let string_of_field_type = function 
  | Ot.Ft_unit -> "unit"
  | Ot.Ft_basic_type bt -> string_of_basic_type bt 
  | Ot.Ft_user_defined_type ud -> string_of_user_defined ud  

let string_of_repeated_type = function
  | Ot.Rt_list -> "list"
  | Ot.Rt_repeated_field -> "Pbrt.Repeated_field.t"

let string_of_associative_type = function
  | Ot.At_list -> "list"
  | Ot.At_hashtable -> "Hashtbl.t"

let string_of_record_field_type = function
  | Ot.Rft_nolabel (field_type, _, _)
  | Ot.Rft_required (field_type, _, _, _) -> 
      string_of_field_type field_type
  | Ot.Rft_optional (field_type, _, _, _) -> 
      (string_of_field_type field_type) ^ " option"
  | Ot.Rft_repeated_field (rt, field_type, _, _,_) -> 
      (string_of_field_type field_type) ^ " " ^ (string_of_repeated_type rt)
  | Ot.Rft_associative_field (Ot.At_list, _, (key_type, _), (value_type, _)) -> 
      Printf.sprintf "(%s * %s) %s" 
        (string_of_basic_type key_type)
        (string_of_field_type value_type) 
        (string_of_associative_type Ot.At_list) 
  | Ot.Rft_associative_field (Ot.At_hashtable, _, (key_type, _), (value_type, _)) -> 
      Printf.sprintf "(%s, %s) %s" 
        (string_of_basic_type key_type)
        (string_of_field_type value_type) 
        (string_of_associative_type Ot.At_hashtable) 
  | Ot.Rft_variant_field {Ot.v_name; _ } -> v_name
 
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)
let function_name_of_user_defined prefix = function 
  | {Ot.udt_module = Some module_; Ot.udt_type_name; _} -> 
    sp "%s.%s_%s" module_ prefix udt_type_name 
  | {Ot.udt_module = None; Ot.udt_type_name; _} -> 
    sp "%s_%s" prefix udt_type_name 

let caml_file_name_of_proto_file_name proto = 
  let splitted = Pb_util.rev_split_by_char '.' proto in 
  if List.length splitted < 2 || 
     List.hd splitted <> "proto" 
  then failwith "Proto file has no valid extension"
  else 
    String.concat "_" @@ List.rev @@ ("pb" :: (List.tl splitted)) 

let mutable_record_name s = s ^ "_mutable" 

let string_of_payload_kind ?capitalize payload_kind packed = 
  let s = match payload_kind,  packed with
  | Ot.Pk_varint _ , false -> "varint"
  | Ot.Pk_bits32   , false -> "bits32"
  | Ot.Pk_bits64   , false -> "bits64"
  | Ot.Pk_bytes    , _ -> "bytes"
  | Ot.Pk_varint _ , true 
  | Ot.Pk_bits32   , true
  | Ot.Pk_bits64   , true  -> "bytes"
  in 
  match capitalize with
  | None -> s 
  | Some () -> String.capitalize s [@@ocaml.warning "-3"] 

(* this function transforms a `lower_case_like_this` into an 
 * ocamlCaseLikeThis *)
let camel_case_of_label s = 
  let len = String.length s in 
  let b = Bytes.create len in 
  let capitalize = ref false and blen = ref 0 in 
  for i = 0 to (len - 1) do 
    let c = String.get s i in   
    if c = '_'
    then begin
      capitalize := true; 
    end
    else begin
      begin 
        if !capitalize
        then begin Bytes.set b !blen (Char.uppercase c) end
        else begin Bytes.set b !blen c end;
      end;
      capitalize := false; 
      incr blen; 
    end
  done;
  Bytes.sub_string b 0 !blen

let camel_case_of_constructor s = 
  camel_case_of_label (String.lowercase s)

let runtime_function = function 
  | `Decode , Ot.Pk_varint false, Ot.Bt_int   -> "Pbrt.Decoder.int_as_varint" 
  | `Decode , Ot.Pk_varint true , Ot.Bt_int   -> "Pbrt.Decoder.int_as_zigzag" 
  | `Decode , Ot.Pk_varint false, Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_varint" 
  | `Decode , Ot.Pk_varint true , Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_zigzag" 
  | `Decode , Ot.Pk_varint false, Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_varint" 
  | `Decode , Ot.Pk_varint true , Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_zigzag" 
  | `Decode , Ot.Pk_bits32, Ot.Bt_int32 -> "Pbrt.Decoder.int32_as_bits32" 
  | `Decode , Ot.Pk_bits64, Ot.Bt_int64 -> "Pbrt.Decoder.int64_as_bits64" 
  | `Decode , Ot.Pk_varint false, Ot.Bt_bool -> "Pbrt.Decoder.bool" 
  | `Decode , Ot.Pk_bits32, Ot.Bt_float -> "Pbrt.Decoder.float_as_bits32" 
  | `Decode , Ot.Pk_bits64, Ot.Bt_float -> "Pbrt.Decoder.float_as_bits64" 
  | `Decode , Ot.Pk_bits32, Ot.Bt_int -> "Pbrt.Decoder.int_as_bits32" 
  | `Decode , Ot.Pk_bits64, Ot.Bt_int -> "Pbrt.Decoder.int_as_bits64" 
  | `Decode , Ot.Pk_bytes, Ot.Bt_string -> "Pbrt.Decoder.string" 
  | `Decode , Ot.Pk_bytes, Ot.Bt_bytes -> "Pbrt.Decoder.bytes" 
  | `Encode , Ot.Pk_varint false, Ot.Bt_int   -> "Pbrt.Encoder.int_as_varint" 
  | `Encode , Ot.Pk_varint true , Ot.Bt_int   -> "Pbrt.Encoder.int_as_zigzag" 
  | `Encode , Ot.Pk_varint false, Ot.Bt_int32 -> "Pbrt.Encoder.int32_as_varint" 
  | `Encode , Ot.Pk_varint true , Ot.Bt_int32 -> "Pbrt.Encoder.int32_as_zigzag" 
  | `Encode , Ot.Pk_varint false, Ot.Bt_int64 -> "Pbrt.Encoder.int64_as_varint" 
  | `Encode , Ot.Pk_varint true , Ot.Bt_int64 -> "Pbrt.Encoder.int64_as_zigzag" 
  | `Encode , Ot.Pk_bits32, Ot.Bt_int32 -> "Pbrt.Encoder.int32_as_bits32" 
  | `Encode , Ot.Pk_bits64, Ot.Bt_int64 -> "Pbrt.Encoder.int64_as_bits64" 
  | `Encode , Ot.Pk_varint false, Ot.Bt_bool -> "Pbrt.Encoder.bool" 
  | `Encode , Ot.Pk_bits32, Ot.Bt_float -> "Pbrt.Encoder.float_as_bits32" 
  | `Encode , Ot.Pk_bits64, Ot.Bt_float -> "Pbrt.Encoder.float_as_bits64" 
  | `Encode , Ot.Pk_bits32, Ot.Bt_int -> "Pbrt.Encoder.int_as_bits32" 
  | `Encode , Ot.Pk_bits64, Ot.Bt_int -> "Pbrt.Encoder.int_as_bits64" 
  | `Encode , Ot.Pk_bytes, Ot.Bt_string -> "Pbrt.Encoder.string" 
  | `Encode , Ot.Pk_bytes, Ot.Bt_bytes -> "Pbrt.Encoder.bytes" 
  | _ -> failwith "Invalid encoding/OCaml type combination"
