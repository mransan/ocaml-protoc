(*
  The MIT License (MIT)

  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

module E = Pb_exception
module Ot = Pb_codegen_ocaml_type
module Tt = Pb_typing_type_tree
module Typing_util = Pb_typing_util

(* [rev_split_by_naming_convention s] will split [s] according to the protobuf
 *  coding style convention. The rule split are
 *  {ul
 *  {- character ['_'] is a separator}
 *  {- the first uppercase letter after a lower case is a separator
 *     (ie FooBar will be split into [ ["Bar";"Foo"] ]}
 *  }
 *)
let rev_split_by_naming_convention s =
  let is_uppercase c =
    (64 < (Char.code c) && (Char.code c) < 91)
  in

  let add_sub_string start_i end_i l  =
    if start_i = end_i
    then l
    else (String.sub s start_i (end_i - start_i)) :: l
  in

  let l, start_i, _ = Pb_util.string_fold_lefti (fun acc i c ->
    let (l, start_i, uppercase_run) = acc in
    match c, uppercase_run with
    | '_', _ ->
      (add_sub_string start_i i l, i + 1, false)
    | c , false  when (is_uppercase c) ->
      (add_sub_string start_i i l, i, true)
    | _ ->
      (l, start_i, is_uppercase c)
  ) ([], 0, false) s in

  let len = String.length s in
  add_sub_string start_i len l


let fix_ocaml_keyword_conflict s =
  match s with
  | "and"
  | "as"
  | "assert"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "lazy"
  | "let"
  | "match"
  | "method"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "unit"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with"
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr" -> s ^ "_"
  | _     -> s

let constructor_name s =
  rev_split_by_naming_convention s
  |> List.rev
  |> String.concat "_"
  |> String.lowercase
  |> String.capitalize [@@ocaml.warning "-3"]

let module_name = constructor_name

let label_name_of_field_name s =
  rev_split_by_naming_convention s
  |> List.rev
  |> String.concat "_"
  |> String.lowercase
  |> fix_ocaml_keyword_conflict [@@ocaml.warning "-3"]

let module_prefix_of_file_name file_name =
  let file_name = Filename.basename file_name in
  match String.rindex file_name '.' with
  | dot_index ->
    module_name @@ (String.sub file_name 0 dot_index)
  | exception Not_found -> E.invalid_file_name file_name

let type_name message_scope name =
  let module S = String in
  let all_names =  message_scope @ [name] in
  let all_names = List.map (fun s ->
    rev_split_by_naming_convention s
    |> List.rev
    |> List.map String.lowercase
  ) all_names [@ocaml.warning "-3"]  in
  let all_names = List.flatten all_names in

  match all_names with
  | []     -> failwith "Programmatic error"
  | hd::[] -> fix_ocaml_keyword_conflict hd
  | _      -> S.concat "_" all_names

let wrapper_type_of_type_name = function
  | "FloatValue" -> Ot.({wt_type = Bt_float; wt_pk = Pk_bits32})
  | "DoubleValue" -> Ot.({wt_type = Bt_float; wt_pk = Pk_bits64})
  | "Int64Value" -> Ot.({wt_type = Bt_int64; wt_pk = (Pk_varint false)})
  | "UInt64Value" -> Ot.({wt_type = Bt_int64; wt_pk = (Pk_varint false)})
  | "Int32Value" -> Ot.({wt_type = Bt_int32; wt_pk = (Pk_varint false)})
  | "UInt32Value" -> Ot.({wt_type = Bt_int32; wt_pk = (Pk_varint false)})
  | "BoolValue" -> Ot.({wt_type = Bt_bool; wt_pk = (Pk_varint false)})
  | "StringValue" -> Ot.({wt_type = Bt_string; wt_pk = Pk_bytes})
  | "BytesValue" -> Ot.({wt_type = Bt_bytes; wt_pk = Pk_bytes})
  | type_name -> E.unsupported_wrapper_type type_name

(** [user_defined_type_of_id module_ all_types i] returns the field type name
    for the type identied by [i] and which is expected to be in [all_types].

    [module_] is the module of the type that this field belong to. If [module_]
    is the same as the type [i] module then it won't be added to the field type
    name. However if the field type belongs to a different module then it will
    be included. This distinction is necessary as OCaml will fail to compile
    if the type of a field which is defined within the same module is prefix
    with the module name. (This is essentially expecting (rightly) a sub module
    with the same name.
 *)
let user_defined_type_of_id all_types file_name i =
  let module_prefix = module_prefix_of_file_name file_name in
  match Typing_util.type_of_id all_types i with
  | exception Not_found ->
    E.programmatic_error E.No_type_found_for_id
  | {Tt.file_name; spec; _ } as t ->
      if Typing_util.is_empty_message t
      then Ot.Ft_unit
      else
        let field_type_module_prefix = module_prefix_of_file_name file_name in
        if field_type_module_prefix = "Wrappers"
        then 
          Ot.Ft_wrapper_type (
              wrapper_type_of_type_name (Typing_util.type_name_of_type t))
        else 
          let udt_type = match spec with
            | Tt.Enum _ -> `Enum
            | Tt.Message _ -> `Message
          in
          let {Tt.message_names; _ } = Typing_util.type_scope_of_type t in
          let udt_type_name =
              type_name message_names (Typing_util.type_name_of_type t) in
          if field_type_module_prefix = module_prefix
          then Ot.(Ft_user_defined_type {
            udt_type;
            udt_module_prefix = None;
            udt_type_name
          })
          else Ot.(Ft_user_defined_type {
            udt_type;
            udt_module_prefix = Some field_type_module_prefix;
            udt_type_name
          })

let encoding_info_of_field_type all_types field_type =
  match field_type with
    | `Double     -> Ot.Pk_bits64
    | `Float      -> Ot.Pk_bits32
    | `Int32      -> Ot.Pk_varint false
    | `Int64      -> Ot.Pk_varint false
    | `Uint32     -> Ot.Pk_varint false
    | `Uint64     -> Ot.Pk_varint false
    | `Sint32     -> Ot.Pk_varint true
    | `Sint64     -> Ot.Pk_varint true
    | `Fixed32    -> Ot.Pk_bits32
    | `Fixed64    -> Ot.Pk_bits64
    | `Sfixed32   -> Ot.Pk_bits32
    | `Sfixed64   -> Ot.Pk_bits64
    | `Bool       -> Ot.Pk_varint false
    | `String     -> Ot.Pk_bytes
    | `Bytes      -> Ot.Pk_bytes
    | `User_defined id ->
      begin match Typing_util.type_of_id all_types id with
      | {Tt.spec = Tt.Enum _; _ } -> Ot.Pk_varint false
      | {Tt.spec = Tt.Message _; _} -> Ot.Pk_bytes
      end

let encoding_of_field all_types
    (field:(Pb_field_type.resolved, 'a) Tt.field)  =

  let packed = match Typing_util.field_option field "packed" with
    | Some (Pb_option.Constant_bool x) -> x
    | Some _ -> E.invalid_packed_option (Typing_util.field_name field)
    | None -> false
  in

  let pk =
      encoding_info_of_field_type all_types (Typing_util.field_type field) in
  (
    pk,
    Typing_util.field_number field,
    packed,
    Typing_util.field_default field
  )

let compile_field_type
      all_types file_options field_options file_name field_type =

  let ocaml_type = match Pb_option.get field_options "ocaml_type" with
    | Some (Pb_option.Constant_litteral "int_t") -> `Int_t
    | _ -> `None
  in

  let int32_type = match Pb_option.get file_options "int32_type" with
    | Some (Pb_option.Constant_litteral "int_t") -> Ot.(Ft_basic_type Bt_int)
    | _ -> Ot.(Ft_basic_type Bt_int32)
  in

  let int64_type = match Pb_option.get file_options "int64_type" with
    | Some (Pb_option.Constant_litteral "int_t") -> Ot.(Ft_basic_type Bt_int)
    | _ -> Ot.(Ft_basic_type Bt_int64)
  in

  let module T = struct
    type b32 = [ `Int32 | `Uint32 | `Sint32 | `Fixed32 | `Sfixed32 ]
    type b64 = [ `Int64 | `Uint64 | `Sint64 | `Fixed64 | `Sfixed64 ]
    type int = [ b32 | b64 ]
  end in

  match field_type, ocaml_type with
  | #T.int    , `Int_t ->  Ot.(Ft_basic_type Bt_int)
  | #T.b32    , _ -> int32_type
  | #T.b64    , _ -> int64_type
  | `Double   , _ -> Ot.(Ft_basic_type Bt_float)
  | `Float    , _ -> Ot.(Ft_basic_type Bt_float)
  | `Bool     , _ -> Ot.(Ft_basic_type Bt_bool)
  | `String   , _ -> Ot.(Ft_basic_type Bt_string)
  | `Bytes    , _ -> Ot.(Ft_basic_type Bt_bytes)
  | `User_defined id, _ ->
    user_defined_type_of_id all_types file_name id

let is_mutable ?field_name field_options =
  match Pb_option.get field_options "ocaml_mutable"  with
  | Some (Pb_option.Constant_bool v) -> v
  | Some _ -> Pb_exception.invalid_mutable_option ?field_name ()
  | None -> false

let ocaml_container field_options =
  match Pb_option.get field_options "ocaml_container" with
  | None -> None
  | Some (Pb_option.Constant_litteral container_name) -> Some container_name
  | Some _ -> None

let variant_of_oneof
        ?include_oneof_name
        ~outer_message_names
        all_types
        file_options
        file_name
        oneof_field =

  let v_constructors = List.map (fun field ->
    let pbtt_field_type =  Typing_util.field_type field in
    let field_type = compile_field_type
      all_types
      file_options
      (Typing_util.field_options field)
      file_name
      pbtt_field_type
    in

    let (
      vc_payload_kind,
      vc_encoding_number, _, _) = encoding_of_field all_types field in

    let vc_constructor = constructor_name (Typing_util.field_name field) in

    Ot.({
      vc_constructor;
      vc_encoding_number;
      vc_payload_kind;
      vc_field_type = match field_type with
        | Ft_unit -> Vct_nullary
        | _       -> Vct_non_nullary_constructor field_type;
    })
  ) oneof_field.Tt.oneof_fields in

  let v_name = match include_oneof_name with
    | None    -> type_name outer_message_names ""
    | Some () -> type_name outer_message_names oneof_field.Tt.oneof_name
  in
  Ot.({v_name; v_constructors})

(*
 * Notes on type level PPX extension handling.
 *
 * ocaml-protoc supports 2 custom options for defining type level ppx
 * extensions:
 * a) message option called ocaml_type_ppx
 * b) file option called ocaml_all_types_ppx
 *
 * 'ocaml_type_ppx' has priority over 'ocaml_all_types_ppx' extension.
 * This means that if a message contains 'ocaml_type_ppx' extension then the
 * associated string will be used for the OCaml generated type ppx extension.
 *
 * 'ocaml_all_types_ppx' is a file option which is a convenient workflow when
 * the ppx extensions are the same for all types. (Most likely the case).
 *
 *)

(* utility function to return the string value from a sring option
 *)
let string_of_string_option message_name = function
  | None -> None
  | Some Pb_option.Constant_string s -> Some s
  | _ -> E.invalid_ppx_extension_option message_name

(* utility function to implement the priority logic defined in the notes above.
 *)
let process_all_types_ppx_extension
      file_name file_options type_level_ppx_extension =
  match type_level_ppx_extension with
  | Some x -> Some x
  | None ->
    Pb_option.get file_options "ocaml_all_types_ppx"
    |> string_of_string_option file_name

let compile_message
    (file_options: Pb_option.set)
    (all_types: Pb_field_type.resolved Tt.proto)
    (file_name:string)
    (scope:Tt.type_scope)
    (message: Pb_field_type.resolved Tt.message ) :
    Ot.type_ list   =

  let module_prefix = module_prefix_of_file_name file_name in
  (* TODO maybe module_ should be resolved before `compile_message` since
     it is common with compile_enum
   *)

  let {Tt.message_name; Tt.message_body;_} = message in

  let {Tt.message_names; _ } = scope in

  let type_level_ppx_extension =
    Typing_util.message_option message "ocaml_type_ppx"
    |> string_of_string_option message_name
    |> process_all_types_ppx_extension file_name file_options
  in

  (* In case a message is only made of a `one of` field then we
     generate a only a variant rather than both a variant and a message with
     a single field. This is an optimization which makes the generated
     OCaml code much easier.
   *)
  match message_body with
  | []  -> []

  | Tt.Message_oneof_field f :: [] -> (
    let outer_message_names = message_names @ [message_name] in
    let variant = variant_of_oneof
        ~outer_message_names all_types file_options file_name f in
    [Ot.({module_prefix; spec = Variant variant;type_level_ppx_extension})]
  )

  | _ ->
    let variants, fields = List.fold_left (fun (variants, fields) -> function
      | Tt.Message_field field -> begin

        let (
          pk,
          encoding_number,
          packed, _) = encoding_of_field all_types field
        in

        let field_name = Typing_util.field_name field in

        let field_options = Typing_util.field_options field in

        let field_type = Typing_util.field_type field in

        let ocaml_field_type = compile_field_type
          all_types
          file_options
          field_options
          file_name
          field_type in

        let field_default = Typing_util.field_default field in

        let mutable_  = is_mutable ~field_name field_options in

        let record_field_type = match Typing_util.field_label field with
          | `Nolabel ->
            (* From proto3 section on default value:
             * https://goo.gl/NKt9Cc
             *
             * --
             * For message fields, the field is not set. Its exact value is
             * language-dependent. See the generated code guide for details.
             * --
             *
             * Since we must support the face that the message won't be sent
             * we always make such a field an OCaml option. It's the
             * responsability of the application to check for [None] and
             * perform any error handling if required. *)
            let is_message =
              begin match ocaml_field_type with
              | Ot.Ft_user_defined_type {Ot.udt_type = `Message;_} -> true
              | _ -> false
              end
            in
            if is_message
            then Ot.Rft_optional (ocaml_field_type, encoding_number, pk, None)
            else Ot.Rft_nolabel (ocaml_field_type, encoding_number, pk)

          | `Required ->
             Ot.Rft_required (
                ocaml_field_type, encoding_number, pk, field_default)

          | `Optional ->
             Ot.Rft_optional (
                ocaml_field_type, encoding_number, pk, field_default)

          | `Repeated ->
            let repeated_type = begin match ocaml_container field_options with
              | None -> Ot.Rt_list
              | Some "repeated_field" -> Ot.Rt_repeated_field
              | Some _ -> failwith "Invalid ocaml_container attribute value"
            end in
            Ot.Rft_repeated (
              repeated_type,
              ocaml_field_type,
              encoding_number,
              pk,
              packed
            )
        in

        let record_field = Ot.({
          rf_label = label_name_of_field_name field_name;
          rf_field_type = record_field_type;
          rf_mutable = mutable_;
        }) in

        (variants, record_field::fields)
      end (* Message_field *)

      | Tt.Message_oneof_field field -> (
        let outer_message_names = message_names @ [message_name] in
        let variant = variant_of_oneof
            ~include_oneof_name:()
            ~outer_message_names
            all_types
            file_options
            file_name
            field in

        let record_field = Ot.({
          rf_label = label_name_of_field_name field.Tt.oneof_name;
          rf_mutable = false;
            (* TODO feature:
             * Currently the field option of a oneof field is not being parsed
             * at all. This enhancement should essentially propage from the
             * parser all the way down to here.
             *)
          rf_field_type = Rft_variant variant;
        }) in

        let variants =
          let t = Ot.({
            module_prefix;
            spec = Variant variant;
            type_level_ppx_extension}) in
          t::variants in

        let fields   = record_field::fields in

        (variants, fields)

      ) (* Message_oneof_field *)

      | Tt.Message_map_field mf -> (
        let {
          Tt.map_name;
          map_number;
          map_key_type;
          map_value_type;
          map_options} = mf in

        let key_type = compile_field_type
          all_types
          file_options
          map_options
          file_name
          map_key_type in

        let key_pk = encoding_info_of_field_type all_types map_key_type in

        let key_type = match key_type with
          | Ot.Ft_basic_type bt -> bt
          | _ -> failwith "Only Basic Types are supported for map keys"
        in

        let value_type = compile_field_type
          all_types
          file_options
          map_options
          file_name
          map_value_type in

        let value_pk = encoding_info_of_field_type all_types map_value_type in

        let associative_type = match ocaml_container map_options with
          | None -> Ot.At_list
          | Some "hashtbl" -> Ot.At_hashtable
          | Some _ -> failwith "Invalid ocaml_container attribute value for map"
        in

        let record_field_type = Ot.(Rft_associative
          (associative_type, map_number, (key_type, key_pk), (value_type, value_pk))
        ) in

        let record_field = Ot.({
          rf_label = label_name_of_field_name map_name;
          rf_field_type = record_field_type;
          rf_mutable = is_mutable ~field_name:map_name map_options;
        }) in

        (variants, record_field::fields)

      ) (* Message_map_field *)

    ) ([], []) message_body in (* fold_left body *)

    let record = Ot.({
      r_name = type_name message_names message_name;
      r_fields = List.rev fields;
    }) in

    let type_ = Ot.({
      module_prefix;
      spec = Record record;
      type_level_ppx_extension;
    }) in

    List.rev (type_ :: variants)

let compile_enum file_options file_name scope enum =

  let {Tt.enum_name; enum_values; _ } = enum in
  let module_prefix = module_prefix_of_file_name file_name in
  let {Tt.message_names; Tt.packages = _ } = scope in

  let cv_constructors = List.map (fun {Tt.enum_value_name; Tt.enum_value_int} ->
    {
      Ot.cvc_name = constructor_name enum_value_name;
      Ot.cvc_binary_value = enum_value_int;
      Ot.cvc_string_value = enum_value_name;
    }
  ) enum_values in

  let type_level_ppx_extension =
    Typing_util.enum_option enum "ocaml_enum_ppx"
    |> string_of_string_option enum_name
    |> process_all_types_ppx_extension file_name file_options
  in

  Ot.({
    module_prefix;
    spec = Const_variant {
      cv_name = type_name message_names enum_name;
      cv_constructors;
    };
    type_level_ppx_extension;
  })

let compile all_types = function
  | {Tt.spec = Tt.Message m ; file_name; file_options; scope; _ } ->
    compile_message file_options all_types file_name scope m

  | {Tt.spec = Tt.Enum e ; file_name; scope; file_options; _ } ->
    [compile_enum file_options file_name scope e]

