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

module E  = Exception 
module L  = Logger 
module OCaml_types = Ocaml_types 

let rev_split_by_naming_convention s =
  let is_uppercase c = 
    (64 < (Char.code c) && (Char.code c) < 91)
  in 

  let add_sub_string start_i end_i l  = 
    if start_i = end_i 
    then l 
    else (String.sub s start_i (end_i - start_i)) :: l
  in 
  
  let l, start_i, _ = Util.string_fold_lefti (fun (l, start_i, uppercase_run) i c ->
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
  |> String.capitalize

let module_name = constructor_name 

let record_field_name s =
  rev_split_by_naming_convention s
  |> List.rev
  |> String.concat "_"
  |> String.lowercase 
  |> fix_ocaml_keyword_conflict 


let module_of_file_name file_name = 
  let file_name = Filename.basename file_name in 
  match String.rindex file_name '.' with 
  | dot_index -> 
    module_name @@ (String.sub file_name 0 dot_index ^ "_pb") 
  | exception Not_found -> raise @@ E.invalid_file_name file_name  

let type_name message_scope name = 
  let module S = String in  
  let all_names =  message_scope @ [name] in 
  let all_names = List.map (fun s -> 
    rev_split_by_naming_convention s
    |> List.rev 
    |> List.map String.lowercase 
  ) all_names in 
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
  match Pbtt_util.type_of_id all_types i with
  | exception Not_found -> 
    raise @@ E.programmatic_error E.No_type_found_for_id 
  | {Pbtt.file_name; _ } as t -> 
      let field_type_module = module_of_file_name file_name in  
      let {Pbtt.message_names; _ } = Pbtt_util.type_scope_of_type t in 
      let type_name = type_name message_names (Pbtt_util.type_name_of_type t) in 
      if field_type_module = module_ 
      then OCaml_types.({module_ = None; type_name}) 
      else OCaml_types.({module_ = Some field_type_module; type_name}) 

let compile_field ?as_constructor all_types f type_qualifier file_name field = 
  let field_name = Pbtt_util.field_name field in 
  let encoding_type = Pbtt_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

  let field_encoding = Encoding_util.encoding_of_field_type all_types field in 
  let field_type = match encoding_type with
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
      let user_defined_type = user_defined_type_of_id all_types file_name id in 
      OCaml_types.User_defined_type user_defined_type
  in {
    OCaml_types.field_type; 
    OCaml_types.field_name; 
    OCaml_types.type_qualifier; 
    OCaml_types.encoding_type = f field_encoding ; 
  }

let compile_oneof all_types file_name message_scope outer_message_name variant_encoding {Pbtt.oneof_name ; Pbtt.oneof_fields } = 
  let {Pbtt.message_names; _ } = message_scope in 
  let variant_name = match variant_encoding with
    | OCaml_types.Inlined_within_message -> type_name (message_names @ [outer_message_name]) oneof_name 
    | OCaml_types.Standalone -> type_name (message_names @ [outer_message_name]) "" in 
  let variant_constructors = List.map (fun field -> 
    compile_field ~as_constructor:() all_types (fun x -> x) OCaml_types.No_qualifier file_name field 
  ) oneof_fields in 
  OCaml_types.({
    variant_name; 
    variant_constructors; 
    variant_encoding;
  })

let compile_message  
  (all_types: Pbtt.resolved Pbtt.proto) 
  (file_name:string) 
  (scope:Pbtt.type_scope) 
  (message: Pbtt.resolved Pbtt.message ) :
  OCaml_types.type_ list   = 

  let module_ = module_of_file_name file_name in 
  (* TODO maybe module_ should be resolved before `compile_message` since 
     it is common with compile_enum
   *)
  let {Pbtt.message_name; Pbtt.message_body;} = message in 

  let {Pbtt.message_names; _ } = scope in  
  let record_name = type_name message_names message_name in 

  (* In case a message is only made of a `one of` field then we 
     generate a only a variant rather than both a variant and a message with 
     a single field. This is an optimization which makes the generated
     OCaml code much easier. 
   *)
  match message_body with 
  | Pbtt.Message_oneof_field f :: [] -> (
    [OCaml_types.({
      module_; 
      spec = Variant (compile_oneof all_types file_name scope message_name Standalone f);  
    })]
  ) 
  | _ -> 
    let variants, fields = List.fold_left (fun (variants, fields) -> function
      | Pbtt.Message_field f -> (
        let type_qualifier = match Pbtt_util.field_label f with 
          | `Optional -> OCaml_types.Option 
          | `Required -> OCaml_types.No_qualifier
          | `Repeated -> OCaml_types.List
        in 
        (variants, (compile_field all_types (fun x -> OCaml_types.Regular_field x) type_qualifier file_name f)::fields)
      )
      | Pbtt.Message_oneof_field f -> (
        let variant = compile_oneof all_types file_name scope message_name OCaml_types.Inlined_within_message f in 
        let field   = OCaml_types.({
          field_type =  User_defined_type {type_name = variant.variant_name; module_ = None}; 
          field_name =  record_field_name f.Pbtt.oneof_name;
          type_qualifier = No_qualifier;
          encoding_type = One_of variant; 
        }) in 
        ((OCaml_types.{module_; spec = Variant variant})::variants, field::fields) 
      )) ([], []) message_body in 
    List.rev (OCaml_types.({
        module_;
        spec    = Record {
        record_name; 
        fields = List.rev fields;
      }}) :: variants)

let compile_enum file_name scope {Pbtt.enum_name; Pbtt.enum_values; } = 
  let module_ = module_of_file_name file_name in 
  let {Pbtt.message_names; Pbtt.packages = _ } = scope in 
  let cvariant_name = type_name message_names enum_name in 
  let cvariant_constructors = List.map (fun {Pbtt.enum_value_name; Pbtt.enum_value_int} -> 
    (constructor_name enum_value_name,  enum_value_int)
  ) enum_values in 
  OCaml_types.({
    module_; 
    spec = Const_variant {
      cvariant_name; 
      cvariant_constructors;  
  }})

let compile all_types = function 
  | {Pbtt.spec = Pbtt.Message m ; file_name; scope; _ } -> compile_message all_types file_name scope m 
  | {Pbtt.spec = Pbtt.Enum    e ; file_name; scope; _ } -> [compile_enum file_name scope e] 

