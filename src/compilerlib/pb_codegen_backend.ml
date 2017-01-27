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

module Pt = Pb_parsing_parse_tree 
module E = Pb_exception 
module Ot = Pb_codegen_ocaml_type
module Parsing_util = Pb_parsing_util
module Tt = Pb_typing_type_tree 
module Typing_util = Pb_typing_util 

(* [rev_split_by_naming_convention s] will split [s] according to the protobuf
 *  coding style convention. The rule split are 
 *  {ul
 *  {- character ['_'] is a separator}
 *  {- the first uppercase letter after a lower case is a separator (ie FooBar will be split into [ ["Bar";"Foo"] ]}  
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

let module_of_file_name file_name = 
  let file_name = Filename.basename file_name in 
  match String.rindex file_name '.' with 
  | dot_index -> 
    module_name @@ (String.sub file_name 0 dot_index ^ "_pb") 
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
  let module_ = module_of_file_name file_name in 
  match Typing_util.type_of_id all_types i with
  | exception Not_found -> 
    E.programmatic_error E.No_type_found_for_id 
  | {Tt.file_name; spec; _ } as t -> 
      if Typing_util.is_empty_message t 
      then Ot.Ft_unit 
      else 
        let udt_nested = begin match spec with 
          | Tt.Enum _ -> false 
          | Tt.Message _ -> true 
        end in 
        let field_type_module = module_of_file_name file_name in  
        let {Tt.message_names; _ } = Typing_util.type_scope_of_type t in 
        let udt_type_name = type_name message_names (Typing_util.type_name_of_type t) in 
        if field_type_module = module_ 
        then Ot.(Ft_user_defined_type {udt_nested; udt_module = None; udt_type_name}) 
        else Ot.(Ft_user_defined_type {udt_nested; udt_module = Some field_type_module; udt_type_name}) 

let encoding_info_of_field_type all_types field_type = 
  match field_type with 
    | Tt.Field_type_double     -> Ot.Pk_bits64
    | Tt.Field_type_float      -> Ot.Pk_bits32 
    | Tt.Field_type_int32      -> Ot.Pk_varint false
    | Tt.Field_type_int64      -> Ot.Pk_varint false
    | Tt.Field_type_uint32     -> Ot.Pk_varint false
    | Tt.Field_type_uint64     -> Ot.Pk_varint false
    | Tt.Field_type_sint32     -> Ot.Pk_varint true
    | Tt.Field_type_sint64     -> Ot.Pk_varint true 
    | Tt.Field_type_fixed32    -> Ot.Pk_bits32
    | Tt.Field_type_fixed64    -> Ot.Pk_bits64
    | Tt.Field_type_sfixed32   -> Ot.Pk_bits32
    | Tt.Field_type_sfixed64   -> Ot.Pk_bits64
    | Tt.Field_type_bool       -> Ot.Pk_varint false 
    | Tt.Field_type_string     -> Ot.Pk_bytes
    | Tt.Field_type_bytes      -> Ot.Pk_bytes
    | Tt.Field_type_type id -> 
      begin match Typing_util.type_of_id all_types id with 
      | {Tt.spec = Tt.Enum _; _ } -> Ot.Pk_varint false
      | {Tt.spec = Tt.Message _; _} -> Ot.Pk_bytes
      end 

let encoding_of_field all_types 
    (field:(Tt.resolved_field_type, 'a) Tt.field)  = 

  let packed = match Typing_util.field_option field "packed" with 
    | Some (Pt.Constant_bool x) -> x 
    | Some _ -> E.invalid_packed_option (Typing_util.field_name field)  
    | None -> false 
  in 

  let pk = encoding_info_of_field_type all_types (Typing_util.field_type field) in 

  (pk, Typing_util.field_number field, packed, Typing_util.field_default field)

let compile_field_type ?field_name all_types file_options field_options file_name field_type = 

  let ocaml_type = match Typing_util.find_field_option field_options "ocaml_type" with
    | Some (Pt.Constant_litteral "int_t") -> `Int_t
    | _ -> `None  
  in 

  let int32_type = match Parsing_util.file_option file_options "int32_type" with
    | Some (Pt.Constant_litteral "int_t") -> Ot.(Ft_basic_type Bt_int) 
    | _ -> Ot.(Ft_basic_type Bt_int32)
  in 
  
  let int64_type = match Parsing_util.file_option file_options "int64_type" with
    | Some (Pt.Constant_litteral "int_t") -> Ot.(Ft_basic_type Bt_int) 
    | _ -> Ot.(Ft_basic_type Bt_int64)
  in 

  match field_type, ocaml_type with
  | Tt.Field_type_double, _ -> Ot.(Ft_basic_type Bt_float)
  | Tt.Field_type_float, _ ->  Ot.(Ft_basic_type Bt_float)
  | Tt.Field_type_int32, `Int_t ->  Ot.(Ft_basic_type Bt_int) 
  | Tt.Field_type_int64, `Int_t ->  Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_uint32, `Int_t -> Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_uint64, `Int_t -> Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_sint32, `Int_t -> Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_sint64, `Int_t -> Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_fixed32, `Int_t -> Ot.(Ft_basic_type Bt_int)  
  | Tt.Field_type_fixed64, `Int_t -> Ot.(Ft_basic_type Bt_int)
  | Tt.Field_type_int32, _ ->  int32_type
  | Tt.Field_type_int64, _ ->  int64_type
  | Tt.Field_type_uint32, _ -> int32_type
  | Tt.Field_type_uint64, _ -> int64_type
  | Tt.Field_type_sint32, _ -> int32_type
  | Tt.Field_type_sint64, _ -> int64_type
  | Tt.Field_type_fixed32, _ -> int32_type
  | Tt.Field_type_fixed64, _ -> int64_type
  | Tt.Field_type_sfixed32, _ -> 
      E.unsupported_field_type ?field_name ~field_type:"sfixed32" ~backend_name:"OCaml" () 
  | Tt.Field_type_sfixed64, _ -> 
      E.unsupported_field_type ?field_name ~field_type:"sfixed64" ~backend_name:"OCaml" () 
  | Tt.Field_type_bool, _   -> Ot.(Ft_basic_type Bt_bool)
  | Tt.Field_type_string, _ -> Ot.(Ft_basic_type Bt_string)
  | Tt.Field_type_bytes, _  -> Ot.(Ft_basic_type Bt_bytes)
  | Tt.Field_type_type id, _ -> 
    user_defined_type_of_id all_types file_name id

let is_mutable ?field_name field_options = 
  match Typing_util.find_field_option field_options "ocaml_mutable"  with
  | Some (Pt.Constant_bool v) -> v 
  | Some _ -> Pb_exception.invalid_mutable_option ?field_name () 
  | None -> false
            
let ocaml_container field_options = 
  match Typing_util.find_field_option field_options "ocaml_container" with
  | None -> None 
  | Some (Pt.Constant_litteral container_name) -> Some container_name
  | Some _ -> None

let variant_of_oneof ?include_oneof_name ~outer_message_names all_types file_options file_name oneof_field = 

  let v_constructors = List.map (fun field -> 
    let pbtt_field_type =  Typing_util.field_type field in 
    let field_type = compile_field_type 
      ~field_name:(Typing_util.field_name field) 
      all_types 
      file_options 
      (Typing_util.field_options field) 
      file_name 
      pbtt_field_type
    in 
    let (vc_payload_kind, vc_encoding_number, _, _) = encoding_of_field all_types field in 
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
 * ocaml-protoc supports 2 custom options for defining type level ppx extensions. 
 * a) message option called ocaml_type_ppx
 * b) file option called ocaml_all_types_ppx 
 *
 * 'ocaml_type_ppx' has priority over 'ocaml_all_types_ppx' extension. This means
 * that if a message contains 'ocaml_type_ppx' extension then the associated string
 * will be used for the OCaml generated type ppx extension. 
 *
 * 'ocaml_all_types_ppx' is a file option which is a convenient workflow when the 
 * ppx extensions are the same for all types. (Most likely the case). 
 *
 *)

(* utility function to return the string value from a sring option
 *)
let string_of_string_option message_name = function 
  | None -> None
  | Some Pt.Constant_string s -> Some s 
  | _ -> E.invalid_ppx_extension_option message_name 
  
(* utility function to implement the priority logic defined in the notes above. 
 *)
let process_all_types_ppx_extension file_name file_options type_level_ppx_extension = 
  match type_level_ppx_extension with
  | Some x -> Some x 
  | None -> 
    Parsing_util.file_option file_options "ocaml_all_types_ppx" 
    |> string_of_string_option file_name

let compile_message  
  (file_options: Pt.file_option list)
  (all_types: Tt.resolved_field_type Tt.proto) 
  (file_name:string) 
  (scope:Tt.type_scope) 
  (message: Tt.resolved_field_type Tt.message ) :
  Ot.type_ list   = 

  let module_ = module_of_file_name file_name in 
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
    let variant = variant_of_oneof ~outer_message_names all_types file_options file_name f in
    [Ot.({module_; spec = Variant variant;type_level_ppx_extension})]
  ) 

  | _ -> 
    let variants, fields = List.fold_left (fun (variants, fields) -> function
      | Tt.Message_field field -> (

        let (
          pk, 
          encoding_number, 
          packed, _) = encoding_of_field all_types field 
        in    

        let field_name = Typing_util.field_name field in 

        let field_options = Typing_util.field_options field in 

        let field_type = compile_field_type 
          ~field_name
          all_types 
          file_options 
          field_options 
          file_name
          (Typing_util.field_type field) in  

        let field_default = Typing_util.field_default field in 

        let mutable_  = is_mutable ~field_name field_options in 

        let record_field_type = match Typing_util.field_label field with
          | `Nolabel -> Ot.Rft_nolabel (field_type, encoding_number, pk)
          | `Required -> Ot.Rft_required (field_type, encoding_number, pk, field_default) 
          | `Optional -> Ot.Rft_optional (field_type, encoding_number, pk, field_default) 
          | `Repeated -> 
            let repeated_type = begin match ocaml_container field_options with 
              | None -> Ot.Rt_list 
              | Some "repeated_field" -> Ot.Rt_repeated_field 
              | Some _ -> failwith "Invalid ocaml_container attribute value" 
            end in 
            Ot.Rft_repeated_field (repeated_type, field_type, encoding_number, pk, packed) 
        in

        let record_field = Ot.({
          rf_label = label_name_of_field_name field_name; 
          rf_field_type = record_field_type; 
          rf_mutable = mutable_; 
        }) in

        (variants, record_field::fields)
      ) (* Message_field *)

      | Tt.Message_oneof_field field -> (
        let outer_message_names = message_names @ [message_name] in 
        let variant = variant_of_oneof ~include_oneof_name:() ~outer_message_names all_types file_options file_name field in

        let record_field = Ot.({
          rf_label = label_name_of_field_name field.Tt.oneof_name; 
          rf_mutable = false; 
            (* TODO feature: 
             * Currently the field option of a oneof field is not being parsed at all. This 
             * enhancement should essentially propage from the parser all the way down 
             * to here. 
             *)
          rf_field_type = Rft_variant_field variant; 
        }) in 
        
        let variants = Ot.({module_; spec = Variant variant; type_level_ppx_extension})::variants in 

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
          ~field_name:(Printf.sprintf "key of %s" map_name)
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
          ~field_name:(Printf.sprintf "value of %s" map_name)
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

        let record_field_type = Ot.(Rft_associative_field 
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
      module_; 
      spec = Record record;
      type_level_ppx_extension;
    }) in 

    List.rev (type_ :: variants)  

let compile_enum file_options file_name scope enum = 

  let {Tt.enum_name; enum_values; _ } = enum in
  let module_ = module_of_file_name file_name in 
  let {Tt.message_names; Tt.packages = _ } = scope in 

  let cv_constructors = List.map (fun {Tt.enum_value_name; Tt.enum_value_int} -> 
    (constructor_name enum_value_name,  enum_value_int)
  ) enum_values in 

  let type_level_ppx_extension = 
    Typing_util.enum_option enum "ocaml_enum_ppx" 
    |> string_of_string_option enum_name 
    |> process_all_types_ppx_extension file_name file_options 
  in

  Ot.({
    module_; 
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

