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

module E = Exception 
module L = Logger 

let field_name {Pbtt.field_parsed; _ } = 
  let {Pbpt.field_name; _ } = field_parsed in 
  field_name 

let field_number {Pbtt.field_parsed = {Pbpt.field_number;_}; _ } = 
  field_number 

let field_type {Pbtt.field_type; _ } = 
  field_type

let field_label {Pbtt.field_parsed = {Pbpt.field_label; _ }; _ } = 
  field_label 

let field_default {Pbtt.field_default; _ } = field_default 

let field_option {Pbtt.field_options; _ } option_name = 
  match List.assoc option_name field_options with
  | x -> Some x 
  | exception Not_found -> None  
  
let empty_scope  = { 
  Pbtt.packages = []; 
  message_names = [] 
} 

let type_id_of_type {Pbtt.id; _ } = id 

let type_of_id all_types id  = 
  List.find (fun t -> type_id_of_type t = id) all_types 

let string_of_unresolved { Pbtt.scope; type_name; from_root }  =
    Printf.sprintf "unresolved:{scope %s, type_name: %s, from_root: %b}"
    (Util.string_of_string_list scope) 
    type_name 
    from_root  

let string_of_type_scope {Pbtt.packages; message_names}  = 
  Printf.sprintf "scope:{packages:%s, message_names:%s}" 
    (Util.string_of_string_list packages)
    (Util.string_of_string_list message_names) 

let string_of_message id scope {Pbtt.message_name; message_body} = 
  Printf.sprintf "message: {id:%i, message_scope:%s, name:%s, field nb:%i}" 
    id 
    (string_of_type_scope scope) 
    message_name
    (List.length message_body)

let scope_of_package = function
  | Some s -> {empty_scope with 
    Pbtt.packages = List.rev @@ Util.rev_split_by_char '.' s
  }
  | None -> empty_scope 

let unresolved_of_string s = 
  match Util.rev_split_by_char '.' s with 
  | [] -> E.programmatic_error E.Invalid_string_split
  | hd :: tl -> {
    Pbtt.scope = (List.rev tl); 
    Pbtt.type_name = hd;
    Pbtt.from_root = String.get s 0 = '.';
  }

let field_type_of_string = function
 | "double"    -> Pbtt.Field_type_double 
 | "float"     -> Pbtt.Field_type_float 
 | "int32"     -> Pbtt.Field_type_int32 
 | "int64"     -> Pbtt.Field_type_int64 
 | "uint32"    -> Pbtt.Field_type_uint32 
 | "uint64"    -> Pbtt.Field_type_uint64
 | "sint32"    -> Pbtt.Field_type_sint32 
 | "sint64"    -> Pbtt.Field_type_sint64 
 | "fixed32"   -> Pbtt.Field_type_fixed32 
 | "fixed64"   -> Pbtt.Field_type_fixed64 
 | "sfixed32"  -> Pbtt.Field_type_sfixed32 
 | "sfixed64"  -> Pbtt.Field_type_sfixed64
 | "bool"      -> Pbtt.Field_type_bool 
 | "string"    -> Pbtt.Field_type_string 
 | "bytes"     -> Pbtt.Field_type_bytes 
 | s  -> Pbtt.Field_type_type  (unresolved_of_string s) 

let map_field_type : 'a Pbtt.field_type  -> 'b Pbtt.field_type = function  
 | Pbtt.Field_type_double     -> Pbtt.Field_type_double    
 | Pbtt.Field_type_float      -> Pbtt.Field_type_float 
 | Pbtt.Field_type_int32      -> Pbtt.Field_type_int32 
 | Pbtt.Field_type_int64      -> Pbtt.Field_type_int64 
 | Pbtt.Field_type_uint32     -> Pbtt.Field_type_uint32 
 | Pbtt.Field_type_uint64     -> Pbtt.Field_type_uint64
 | Pbtt.Field_type_sint32     -> Pbtt.Field_type_sint32 
 | Pbtt.Field_type_sint64     -> Pbtt.Field_type_sint64 
 | Pbtt.Field_type_fixed32    -> Pbtt.Field_type_fixed32 
 | Pbtt.Field_type_fixed64    -> Pbtt.Field_type_fixed64 
 | Pbtt.Field_type_sfixed32   -> Pbtt.Field_type_sfixed32 
 | Pbtt.Field_type_sfixed64   -> Pbtt.Field_type_sfixed64
 | Pbtt.Field_type_bool       -> Pbtt.Field_type_bool 
 | Pbtt.Field_type_string     -> Pbtt.Field_type_string 
 | Pbtt.Field_type_bytes      -> Pbtt.Field_type_bytes 
 | Pbtt.Field_type_type _ -> E.programmatic_error E.Unexpected_field_type 

let compile_default_p2 all_types field = 
  let field_name = field_name field in 
  let field_type = field_type field in 
  let field_default = field_default field in  
  match field_default with
  | None -> None
  | Some constant -> (
    match field_type with  
    | Pbtt.Field_type_double 
    | Pbtt.Field_type_float -> (
      match constant with 
      | Pbpt.Constant_int i   -> Some (Pbpt.Constant_float (float_of_int i))
      | Pbpt.Constant_float _ -> Some constant 
      | _  -> 
        E.invalid_default_value 
          ~field_name ~info:"invalid default type (float/int expected)" ()
    )
    | Pbtt.Field_type_int32 
    | Pbtt.Field_type_int64 
    | Pbtt.Field_type_sint32 
    | Pbtt.Field_type_sint64 
    | Pbtt.Field_type_fixed32 
    | Pbtt.Field_type_fixed64 
    | Pbtt.Field_type_sfixed32 
    | Pbtt.Field_type_sfixed64 -> (
      match constant with 
      | Pbpt.Constant_int _ -> Some constant
      | _ -> E.invalid_default_value ~field_name ~info:"invalid default type (int expected)" ()
    )
    | Pbtt.Field_type_uint32 
    | Pbtt.Field_type_uint64 -> (
      match constant with 
      | Pbpt.Constant_int i -> if i >=0 
        then Some constant 
        else E.invalid_default_value 
          ~field_name ~info:"negative default value for unsigned int" () 
      | _ -> E.invalid_default_value
          ~field_name ~info:"invalid default type (int expected)" ()
    )
    | Pbtt.Field_type_bool -> (
      match constant with 
      | Pbpt.Constant_bool _ -> Some constant
      | _  -> E.invalid_default_value 
        ~field_name ~info:"invalid default type (bool expected)" ()
    ) 
    | Pbtt.Field_type_string -> (
      match constant with 
      | Pbpt.Constant_string _ -> Some constant 
      | _  -> E.invalid_default_value ~field_name ~info:"invalid default type (string expected)" ()
    ) 
    | Pbtt.Field_type_bytes -> E.invalid_default_value 
      ~field_name ~info:"default value not supported for bytes" ()
    | Pbtt.Field_type_type (id:Pbtt.resolved) -> (
      match constant with 
      | Pbpt.Constant_litteral default_enum_value -> (
        let {Pbtt.spec; _ } = type_of_id all_types id in 
        match spec with 
        | Pbtt.Message _ -> E.invalid_default_value 
          ~field_name ~info:"field of type message cannot have a default litteral value" ()
        | Pbtt.Enum {Pbtt.enum_values; _ } -> ( 
          let default_enum_value = Util.apply_until (fun {Pbtt.enum_value_name; _ } -> 
            if enum_value_name = default_enum_value 
            then Some enum_value_name 
            else None
          ) enum_values 
          in
          match default_enum_value with
          | Some _ -> Some constant
          | None   -> E.invalid_default_value 
            ~field_name ~info:"Invalid default enum value" () 
        )
      ) 
      | _ -> E.invalid_default_value 
        ~field_name ~info:"default value not supported for message" ()
    )
  )

let get_default field_name field_options field_type = 
  match List.assoc "default" field_options with
  | constant -> Some constant (* (compile_default field_name constant field_type) *)
  | exception Not_found -> None 

let compile_field_p1 ({
    Pbpt.field_name;
    Pbpt.field_number = _ ;
    Pbpt.field_label  = _ ;
    Pbpt.field_type;
    Pbpt.field_options;
  } as field_parsed) = 

  let field_type    = field_type_of_string field_type in 
  let field_default = get_default field_name field_options field_type in 
  {
    Pbtt.field_parsed;
    Pbtt.field_type;
    Pbtt.field_default;
    Pbtt.field_options;
  }

let compile_map ({
    Pbpt.map_name;
    Pbpt.map_number;
    Pbpt.map_key_type;
    Pbpt.map_value_type;
  }) =
  let map_type_name = Printf.sprintf "Map_%s_%s" map_key_type map_value_type in
  let field_type =
    Pbtt.Field_type_type
      {
        Pbtt.scope = [];
        Pbtt.type_name = map_type_name;
        Pbtt.from_root = false;
      }
  in
  let map_type =
    let key_field =
      {
        Pbpt.field_name = "key";
        Pbpt.field_number = 1;
        Pbpt.field_label = `Required;
        Pbpt.field_type = map_key_type;
        Pbpt.field_options = [];
      }
    in
    let value_field =
      {
        Pbpt.field_name = "value";
        Pbpt.field_number = 2;
        Pbpt.field_label = `Required;
        Pbpt.field_type = map_value_type;
        Pbpt.field_options = [];
      }
    in
    let message_body =
      [
        Pbpt.Message_field key_field;
        Pbpt.Message_field value_field;
      ]
    in
    {
      Pbpt.id = -1;
      Pbpt.message_name = map_type_name;
      Pbpt.message_body;
    }
  in
  let field_parsed =
    {
      Pbpt.field_name = map_name;
      Pbpt.field_number = map_number;
      Pbpt.field_label = `Repeated;
      Pbpt.field_type = map_type_name;
      Pbpt.field_options = [];
    }
  in
  {
    Pbtt.field_parsed;
    Pbtt.field_type;
    Pbtt.field_default = None;
    Pbtt.field_options = [];
  }, map_type

let compile_oneof_p1 ({
    Pbpt.oneof_name; 
    Pbpt.oneof_fields;
  }) = 
  {
    Pbtt.oneof_name; 
    Pbtt.oneof_fields = List.map compile_field_p1 oneof_fields; 
  }
  
let not_found f : bool = 
  try f () ; false 
  with | Not_found -> true 

let rec list_assoc2 x = function
    [] -> raise Not_found
  | (a,b)::l -> if compare b x = 0 then a else list_assoc2 x l

(** type creation function *)
let type_of_spec file_name file_options id scope spec = { 
  Pbtt.id; 
  Pbtt.scope;
  Pbtt.file_name;
  Pbtt.file_options;
  Pbtt.spec; 
}

(** compile a [Pbpt] enum to a [Pbtt] type *) 
let compile_enum_p1 file_name file_options scope {Pbpt.enum_id; enum_name; enum_values } : 'a Pbtt.proto_type =  
  let enum_values = List.map (fun enum_value -> {
    Pbtt.enum_value_name = enum_value.Pbpt.enum_value_name;
    Pbtt.enum_value_int = enum_value.Pbpt.enum_value_int;
  }) enum_values in 

  type_of_spec file_name file_options enum_id scope (Pbtt.Enum {
    Pbtt.enum_name;
    Pbtt.enum_values
  })

(** compile a [Pbpt] message a list of [Pbtt] types (ie messages can 
    defined more than one type). 
  *)
let rec compile_message_p1 file_name file_options message_scope ({
  Pbpt.id;
  Pbpt.message_name; 
  Pbpt.message_body; 
})  = 
  
  let {Pbtt.message_names; _ } = message_scope in  
  let sub_scope = {message_scope with 
    Pbtt.message_names = message_names @ [message_name] 
  } in 

  let message_body, extensions, all_sub = List.fold_left (fun (message_body, extensions, all_types) -> function  
    | Pbpt.Message_field f -> 
        let field = Pbtt.Message_field (compile_field_p1 f) in 
        (field  :: message_body, extensions, all_types)
    | Pbpt.Message_map_field m ->
        let field, extra_type = compile_map m in
        let field = Pbtt.Message_field field in
        let extra_types = compile_message_p1 file_name file_options sub_scope extra_type in
        (field  :: message_body, extensions, all_types @ extra_types)
    | Pbpt.Message_oneof_field o -> 
        let field = Pbtt.Message_oneof_field (compile_oneof_p1 o) in 
        (field :: message_body, extensions, all_types)
    | Pbpt.Message_sub m -> 
        let all_sub_types = compile_message_p1 file_name file_options sub_scope m in 
        (message_body,  extensions, all_types @ all_sub_types)
    | Pbpt.Message_enum ({Pbpt.enum_id; _ } as enum)-> 
        (
          message_body,  
          extensions, 
          all_types @ [compile_enum_p1 file_name file_options sub_scope enum]
        )
    | Pbpt.Message_extension extension_ranges -> 
        (message_body,  extensions @ extension_ranges , all_types)
  ) ([], [], []) message_body in
  
  let message_body = List.rev message_body in 
  
  (* Both field name and field number must be unique 
     within a message scope. This includes the field in a 
     oneof field inside the message. 

     This function verifies this constrain and raises
     the corresponding Duplicated_field_number exception in 
     case it is violated. 
  *)
  let validate_duplicate (number_index:(int*string) list) field = 
    let number = field_number field in 
    let name   = field_name   field in 
    if not_found (fun () -> ignore @@ List.assoc  number number_index) && 
       not_found (fun () -> ignore @@ list_assoc2 name   number_index)
    then 
      (number, name)::number_index
    else 
      E.duplicated_field_number 
        ~field_name:name ~previous_field_name:"" ~message_name ()
  in

  ignore @@ List.fold_left (fun number_index -> function 
    | Pbtt.Message_field f -> validate_duplicate number_index f
    | Pbtt.Message_oneof_field {Pbtt.oneof_fields; _ } ->
       List.fold_left validate_duplicate number_index oneof_fields
  ) [] message_body ;  

  all_sub @ [type_of_spec file_name file_options id message_scope Pbtt.(Message {
    extensions; 
    message_name; 
    message_body;
  })] 

let compile_proto_p1 file_name {Pbpt.package; messages; enums; file_options; _ } =  
  let scope = scope_of_package package in 
  let pbtt_msgs = List.fold_right (fun e pbtt_msgs -> 
    (compile_enum_p1 file_name file_options scope e) :: pbtt_msgs 
  ) enums [] in
  List.fold_left (fun pbtt_msgs pbpt_msg -> 
    pbtt_msgs @ compile_message_p1 file_name file_options scope pbpt_msg
  ) pbtt_msgs messages 

let type_scope_of_type {Pbtt.scope; _ } = scope 

let is_empty_message = function 
  | {Pbtt.spec = Pbtt.Message {Pbtt.message_body;_} } -> 
    0 = List.length message_body 
  | _ -> false

let type_name_of_type = function
  | {Pbtt.spec = Pbtt.Enum {Pbtt.enum_name; _ } } -> enum_name 
  | {Pbtt.spec = Pbtt.Message {Pbtt.message_name; _ } } -> message_name

let find_all_types_in_field_scope all_types (scope:Pbtt.field_scope) = 
  List.filter (fun t -> 
    let {Pbtt.packages; Pbtt.message_names; } = type_scope_of_type t in 
    let dec_scope = packages @ message_names in 
    dec_scope = scope
  ) all_types

let compile_message_p2 types {Pbtt.packages; Pbtt.message_names; } ({
  Pbtt.message_name; 
  Pbtt.message_body} as message)  = 

  (* stringify the message scope so that it can 
     be used with the field scope. 
     
     see `Note on scoping` in the README.md file
   *)
  let message_scope = packages @ message_names @ [message_name] in 

  let process_field_in_scope types scope type_name = 
    let types = find_all_types_in_field_scope types scope in 
    try 
      let t =  List.find (fun t -> 
        type_name = type_name_of_type t 
      ) types in
      Some (type_id_of_type t)  
     with | Not_found -> None 
  in 

  (* this method returns all the scope to search for a type starting 
     by the most innner one first. 

     if [message_scope] = ['Msg1'; 'Msg2'] and [field_scope] = ['Msg3'] then 
     the following scopes will be returned:
     [
       ['Msg1'; 'Msg2'; 'Msg3'];  // This would be the scope of the current msg
       ['Msg1'; 'Msg3'; ];        // Outer message scope
       ['Msg3'; ]                 // Top level scope
     ]
  *)
  let search_scopes (field_scope:string list) from_root : (string list) list = 
    if from_root
    then [field_scope]
    else 
      let rec loop scopes = function
        | [] -> field_scope::scopes 
        | l  -> 
          loop ((l @ field_scope)::scopes) (Util.pop_last l)
      in 
      List.rev @@ loop [] message_scope 
  in 
    
  let compile_field_p2 message_name field = 
    let field_name = field_name field in 
    L.log "[pbtt] field_name: %s\n" field_name; 
    match field.Pbtt.field_type with 
    | Pbtt.Field_type_type ({Pbtt.scope; type_name; from_root} as unresolved) -> ( 

      L.endline @@ "[pbtt] " ^ string_of_unresolved unresolved ; 
      
      let search_scopes = search_scopes scope from_root in 

      L.log "[pbtt] message scope: %s\n" @@ Util.string_of_string_list message_scope;
      List.iteri (fun i scope -> 
        L.log "[pbtt] search_scope[%2i] : %s\n" i @@ Util.string_of_string_list scope 
      ) search_scopes;

      let id = Util.apply_until (fun scope -> 
        process_field_in_scope types scope type_name
      ) search_scopes in 
      (* TODO this is where we are resolving and potentially where we 
         could keep track of the whether it's a message or enum 
       *)
      
      match id with 
      | Some id -> (Pbtt.Field_type_type id:Pbtt.resolved Pbtt.field_type) 
      | None    -> 
        E.unresolved_type ~field_name ~type_:type_name ~message_name () 
    )
    | field_type -> map_field_type field_type
  in

  let message_body = List.fold_left (fun message_body  -> function
    | Pbtt.Message_field field ->
      let field      = {field with Pbtt.field_type = compile_field_p2 message_name field} in  
      let field      = {field with Pbtt.field_default = compile_default_p2 types field} in  
      Pbtt.Message_field field :: message_body
    | Pbtt.Message_oneof_field ({Pbtt.oneof_fields; _ } as oneof )  -> 
      let oneof_fields = List.fold_left (fun oneof_fields field -> 
        let field_type = compile_field_p2 message_name field in  
        {field with Pbtt.field_type }:: oneof_fields 
      ) [] oneof_fields in  
      let oneof_fields = List.rev oneof_fields in 
      Pbtt.Message_oneof_field {oneof with Pbtt.oneof_fields } :: message_body 
  ) [] message_body in 
  let message_body = List.rev message_body in 
  {message with Pbtt.message_body; }

let compile_proto_p2 all_types t = 
  match t with 
  | {Pbtt.file_name; file_options; id; scope; spec = Pbtt.Message  m } -> {
    Pbtt.file_name; 
    Pbtt.file_options;
    Pbtt.scope;
    Pbtt.id; 
    Pbtt.spec = Pbtt.Message (compile_message_p2 all_types scope m) 
  }
  | {Pbtt.file_name; file_options; id; scope; spec = (Pbtt.Enum _ ) as spec ; } ->  { 
    Pbtt.file_name; 
    Pbtt.file_options; 
    Pbtt.id;
    Pbtt.scope;
    Pbtt.spec; 
  } 

let node_of_proto_type = function 
  | {Pbtt.id; Pbtt.spec = Pbtt.Enum _ ; _ }  -> Graph.create_node id [] 
  | {Pbtt.id; Pbtt.spec = Pbtt.Message {Pbtt.message_body; _ }; _ } -> 
    let sub = List.flatten @@ List.map (function
      | Pbtt.Message_field {Pbtt.field_type; _ } -> (
        match field_type with
        | Pbtt.Field_type_type x -> [x]
        | _                      -> []
      )
      | Pbtt.Message_oneof_field {Pbtt.oneof_fields; _ } -> 
          List.flatten @@ List.map (fun {Pbtt.field_type; _ } -> (
           match field_type with
           | Pbtt.Field_type_type x -> [x]
           | _ -> []
          )
          ) oneof_fields  
    ) message_body in 
    Graph.create_node id sub

let is_id input_id {Pbtt.id; _ } = (input_id = id) 

let group proto = 
  let g    = List.map node_of_proto_type proto  in 
  let g    = List.fold_left (fun m n -> 
    Graph.add_node n m 
  ) Graph.empty_graph g in 
  let sccs = Graph.tarjan g in 
  List.map (fun l -> 
    List.map (fun id -> List.find (is_id id) proto) l 
  ) sccs 
