module Ot = Pb_codegen_ocaml_type

let sp x = Printf.sprintf x

let let_decl_of_and = function
  | Some _ -> "and"
  | None -> "let rec"

let string_of_basic_type ?(for_pp = false) = function
  | Ot.Bt_string -> "string"
  | Ot.Bt_float -> "float"
  | Ot.Bt_int -> "int"
  | Ot.Bt_int32 -> "int32"
  | Ot.Bt_uint32 ->
    if for_pp then
      "unsigned_of_int32"
    else
      "[`unsigned of int32]"
  | Ot.Bt_int64 -> "int64"
  | Ot.Bt_uint64 ->
    if for_pp then
      "unsigned_of_int64"
    else
      "[`unsigned of int64]"
  | Ot.Bt_bytes -> "bytes"
  | Ot.Bt_bool -> "bool"

let string_of_user_defined ?module_prefix = function
  | { Ot.udt_module_prefix = None; Ot.udt_type_name; _ } ->
    (match module_prefix with
    | None -> udt_type_name
    | Some module_prefix -> module_prefix ^ "." ^ udt_type_name)
  | { Ot.udt_module_prefix = Some module_prefix; Ot.udt_type_name; _ } ->
    module_prefix ^ "." ^ udt_type_name

let string_of_field_type ?for_pp ?module_prefix = function
  | Ot.Ft_unit -> "unit"
  | Ot.Ft_basic_type bt -> string_of_basic_type ?for_pp bt
  | Ot.Ft_user_defined_type udt -> string_of_user_defined ?module_prefix udt
  | Ot.Ft_wrapper_type { Ot.wt_type; wt_pk = _ } ->
    string_of_basic_type ?for_pp wt_type ^ " option"

let string_of_repeated_type = function
  | Ot.Rt_list -> "list"
  | Ot.Rt_repeated_field -> "Pbrt.Repeated_field.t"

let string_of_associative_type = function
  | Ot.At_list -> "list"
  | Ot.At_hashtable -> "Hashtbl.t"

let string_of_record_field_type ?module_prefix = function
  | Ot.Rft_nolabel (field_type, _, _) | Ot.Rft_required (field_type, _, _, _) ->
    string_of_field_type ?module_prefix field_type
  | Ot.Rft_optional (field_type, _, _, _) ->
    string_of_field_type ?module_prefix field_type ^ " option"
  | Ot.Rft_repeated (rt, field_type, _, _, _) ->
    string_of_field_type ?module_prefix field_type
    ^ " " ^ string_of_repeated_type rt
  | Ot.Rft_associative (Ot.At_list, _, (key_type, _), (value_type, _)) ->
    Printf.sprintf "(%s * %s) %s"
      (string_of_basic_type key_type)
      (string_of_field_type ?module_prefix value_type)
      (string_of_associative_type Ot.At_list)
  | Ot.Rft_associative (Ot.At_hashtable, _, (key_type, _), (value_type, _)) ->
    Printf.sprintf "(%s, %s) %s"
      (string_of_basic_type key_type)
      (string_of_field_type ?module_prefix value_type)
      (string_of_associative_type Ot.At_hashtable)
  | Ot.Rft_variant { Ot.v_name; _ } ->
    (match module_prefix with
    | None -> v_name
    | Some module_prefix -> module_prefix ^ "." ^ v_name)

(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`.

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each
    user defined field type.
 *)
let function_name_of_user_defined ~function_prefix = function
  | { Ot.udt_module_prefix = Some module_prefix; Ot.udt_type_name; _ } ->
    sp "%s.%s_%s" module_prefix function_prefix udt_type_name
  | { Ot.udt_module_prefix = None; Ot.udt_type_name; _ } ->
    sp "%s_%s" function_prefix udt_type_name

let module_type_name_of_service_client (service : Ot.service) : string =
  String.uppercase_ascii service.service_name ^ "_CLIENT"

let module_type_name_of_service_server (service : Ot.service) : string =
  String.uppercase_ascii service.service_name ^ "_SERVER"

let function_name_of_rpc_reserved_keywords_list = [ "make" ]

let function_name_of_rpc (rpc : Ot.rpc) =
  let candidate = String.uncapitalize_ascii rpc.rpc_name in
  if List.mem candidate function_name_of_rpc_reserved_keywords_list then
    candidate ^ "_"
  else
    candidate

let caml_file_name_of_proto_file_name ~proto_file_name =
  let splitted = Pb_util.rev_split_by_char '.' proto_file_name in
  if List.length splitted < 2 || List.hd splitted <> "proto" then
    failwith "Proto file has no valid extension"
  else
    String.concat "_" @@ List.rev @@ List.tl splitted

let mutable_record_name s = s ^ "_mutable"

let string_of_payload_kind ?capitalize payload_kind packed =
  let s =
    match payload_kind, packed with
    | Ot.Pk_varint _, false -> "varint"
    | Ot.Pk_bits32, false -> "bits32"
    | Ot.Pk_bits64, false -> "bits64"
    | Ot.Pk_bytes, _ -> "bytes"
    | Ot.Pk_varint _, true | Ot.Pk_bits32, true | Ot.Pk_bits64, true -> "bytes"
  in
  match capitalize with
  | None -> s
  | Some () -> String.capitalize_ascii s

(* this function transforms a `lower_case_like_this` into an
 * ocamlCaseLikeThis *)
let camel_case_of_label s =
  let len = String.length s in
  let b = Bytes.create len in
  let capitalize = ref false and blen = ref 0 in
  for i = 0 to len - 1 do
    let c = String.get s i in
    if c = '_' then
      capitalize := true
    else (
      if !capitalize then
        Bytes.set b !blen (Char.uppercase_ascii c)
      else
        Bytes.set b !blen c;
      capitalize := false;
      incr blen
    )
  done;
  Bytes.sub_string b 0 !blen

let camel_case_of_constructor s = camel_case_of_label (String.lowercase_ascii s)

let collect_modules_of_field_type modules = function
  | Ot.Ft_user_defined_type { Ot.udt_module_prefix = Some m; _ } -> m :: modules
  | _ -> modules

let collect_modules_of_variant modules { Ot.v_constructors; _ } =
  List.fold_left
    (fun modules { Ot.vc_field_type; _ } ->
      match vc_field_type with
      | Ot.Vct_nullary -> modules
      | Ot.Vct_non_nullary_constructor field_type ->
        collect_modules_of_field_type modules field_type)
    modules v_constructors

let collect_modules_of_record_field_type modules = function
  | Ot.Rft_nolabel (field_type, _, _)
  | Ot.Rft_required (field_type, _, _, _)
  | Ot.Rft_optional (field_type, _, _, _)
  | Ot.Rft_repeated (_, field_type, _, _, _)
  | Ot.Rft_associative (_, _, _, (field_type, _)) ->
    collect_modules_of_field_type modules field_type
  | Ot.Rft_variant variant -> collect_modules_of_variant modules variant

let collect_modules_of_record modules { Ot.r_fields; _ } =
  List.fold_left
    (fun modules { Ot.rf_field_type; _ } ->
      collect_modules_of_record_field_type modules rf_field_type)
    modules r_fields

let collect_modules_of_type_spec modules = function
  | Ot.Record r -> collect_modules_of_record modules r
  | Ot.Variant v -> collect_modules_of_variant modules v
  | Ot.Const_variant _ -> modules
  | Ot.Unit _ -> modules

let collect_modules_of_types ocaml_types =
  List.fold_left
    (fun modules { Ot.spec; _ } -> collect_modules_of_type_spec modules spec)
    [] ocaml_types
  |> List.sort_uniq Stdlib.compare

(*let module_of_context module_prefix file_suffix = function
  | `Single_file -> ""
  | `Multi_file -> Printf.sprintf "%s_%s." module_prefix file_suffix *)
