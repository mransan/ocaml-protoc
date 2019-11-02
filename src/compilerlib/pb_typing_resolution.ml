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
module Pt = Pb_parsing_parse_tree
module Tt = Pb_typing_type_tree 

module Types_by_scope = struct 
  type type_ = Pb_field_type.unresolved Tt.proto_type 
  
  let name_of_type {Tt.spec; _ } = 
    match spec with
    | Tt.Enum {Tt.enum_name; _ } -> enum_name 
    | Tt.Message {Tt.message_name; _} -> message_name  
  
  module String_map = Map.Make(struct 
    type t = string 
    let compare (x:string) (y:string) = Stdlib.compare x y  
  end) 
  
  type t = {
    name : string; 
    types : type_ String_map.t;
    subs : t String_map.t;
  }
  
  let empty ?name:(name = "") () = {
    name; 
    types = String_map.empty;
    subs = String_map.empty
  }
  
  let add t type_ = 
    let rec aux t packages message_names = 
      let {types; subs; _}  = t in 
      match packages, message_names with
      | [], [] -> 
        {t with types = String_map.add (name_of_type type_) type_ types}
  
      | name::packages, message_names 
      | ([] as packages), name :: message_names -> 
        let sub = 
          match String_map.find name subs with
          | sub -> aux sub packages message_names 
          | exception Not_found -> aux (empty ~name ()) packages message_names 
        in
        let subs = subs |> String_map.remove name |> String_map.add name sub in
          (* remove + add = replace *)
        {t with subs} 
    in
  
    let {Tt.scope = {Tt.packages; message_names} ; _ } = type_ in 
    aux t packages message_names  
  
  let find t path type_name = 
    let rec aux t = function
      | [] -> String_map.find type_name t.types 
      | name :: path -> 
        aux (String_map.find name t.subs) path 
    in 
    aux t path  
  
  let rec print ?level:(level = 0) {types; subs; name} = 
    let pr = Printf.printf in 
    pr "%s %s\n" (Pb_util.indentation_prefix level) name;  
    String_map.iter (fun _ t ->
      pr "%s +-%s \n" 
          (Pb_util.indentation_prefix level) (name_of_type t) 
    ) types; 
    String_map.iter (fun _ sub -> 
      print ~level:(level + 1) sub 
    ) subs

  let print t = print t 

  let empty = empty () 

end (* Types_by_scope *)

(* this function returns the type path of a message which is the 
 * packages followed by the enclosing message names and eventually 
 * the message name of the given type. 
 *
 * If the type is an enum then [Failure] is raised. 
 * TODO: change [Failure] to a [Pb_exception.Compilation_error] *) 
let type_path_of_type {Tt.scope; spec; _ } = 
  match spec with
  | Tt.Enum _ -> assert(false) 
  | Tt.Message {Tt.message_name; _} -> 
      let {Tt.packages; message_names} = scope in 
      packages @ message_names @ [message_name] 
  
(* this function returns all the scope to search for a type starting 
 * by the most innner one first. 
 *
 * If [message_scope] = ['Msg1'; 'Msg2'] and [field_scope] = ['Msg3'] then 
 * the following scopes will be returned:
 * [
 *   ['Msg1'; 'Msg2'; 'Msg3'];  // This would be the scope of the current msg
 *   ['Msg1'; 'Msg3'; ];        // Outer message scope
 *   ['Msg3'; ]                 // Top level scope
 * ] *)
let compute_search_type_paths unresolved_field_type message_type_path = 
  let {
    Pb_field_type.type_path = type_path; 
    type_name = _ ; 
    from_root; 
  } = unresolved_field_type in 

  if from_root
  then [type_path]
  else 
    let rec loop type_paths= function
      | [] -> type_path::type_paths
      | l  -> 
        loop ((l @ type_path)::type_paths) (Pb_util.List.pop_last l)
    in 
    List.rev @@ loop [] message_type_path

(* this function ensure that the default value of the field is correct 
 * with respect to its type when this latter is a builtin one. 
 *
 * in case the default value is invalid then an 
 * [Pb_exception.Compilation_error] is raised. 
 *
 * Note that this function also does type coersion when the default value 
 * is an int and the builtin type is a float or double. *) 
let resolve_builtin_type_field_default field_name builtin_type field_default = 
  match field_default with
  | None -> None
  | Some constant -> 
    match builtin_type with  
    | #Pb_field_type.builtin_type_floating_point ->
      begin match constant with 
      | Pb_option.Constant_int i   -> Some (Pb_option.Constant_float (float_of_int i))
      | Pb_option.Constant_float _ -> Some constant 
      | _  -> 
        E.invalid_default_value 
          ~field_name ~info:"invalid default type (float/int expected)" ()
      end

    | #Pb_field_type.builtin_type_signed_int -> 
      begin match constant with 
      | Pb_option.Constant_int _ -> Some constant
      | _ -> 
        let info = "invalid default type (int expected)" in 
        E.invalid_default_value ~field_name ~info ()
      end

    | #Pb_field_type.builtin_type_unsigned_int -> 
      begin match constant with 
      | Pb_option.Constant_int i -> if i >=0 
        then Some constant 
        else E.invalid_default_value 
          ~field_name ~info:"negative default value for unsigned int" () 
      | _ -> E.invalid_default_value
          ~field_name ~info:"invalid default type (int expected)" ()
      end

    | `Bool -> 
      begin match constant with 
      | Pb_option.Constant_bool _ -> Some constant
      | _  -> 
        let info = "invalid default type (bool expected)" in 
        E.invalid_default_value ~field_name ~info ()
      end

    | `String -> 
      begin match constant with 
      | Pb_option.Constant_string _ -> Some constant 
      | _  -> 
        let info = "invalid default type (string expected)" in 
        E.invalid_default_value ~field_name ~info ()
      end

    | `Bytes -> E.invalid_default_value 
      ~field_name ~info:"default value not supported for bytes" ()

(* This function verifies that the default value for a used defined 
 * field is correct. 
 *
 * In protobuf, only field which type is [enum] can have a default 
 * value. Field of type [message] can't. 
 *
 * In the case the field is an enum then the default value must be
 * a litteral value which is one of the enum value. 
 *
 * If the validation fails then [Pb_exception.Compilation_error] is raised *)
let resolve_enum_field_default field_name type_ field_default = 
  match field_default with 
  | None -> None 
  | Some ((Pb_option.Constant_litteral default_enum_value) as constant)  -> 
    let {Tt.spec; _ } = type_ in  
    begin match spec with
    | Tt.Message _ -> 
      let info = 
        "field of type message cannot have a " ^ 
        "default litteral value" 
      in 
      E.invalid_default_value ~field_name ~info ()

    | Tt.Enum {Tt.enum_values; _ } -> 
      let default_enum_value = Pb_util.List.apply_until (fun enum -> 
        let {Tt.enum_value_name; _ } = enum in 
        if enum_value_name = default_enum_value 
        then Some enum_value_name 
        else None
      ) enum_values in
      begin match default_enum_value with
      | Some _ -> Some constant
      | None   -> E.invalid_default_value 
        ~field_name ~info:"Invalid default enum value" () 
      end
    end
  | _ -> E.invalid_default_value 
    ~field_name ~info:"default value not supported for message" ()


(* this function resolves both the type and the defaut value of a field 
 * type. Note that it is necessary to verify both at the same time since
 * the default value must be of the same type as the field type in order
 * to be valid. 
 *
 * For builtin the type the validation is trivial while for user defined 
 * type a search must be done for all the possible scopes the type 
 * might be in. *)
let resolve_field_type_and_default 
    t field_name field_type field_default message_type_path = 

  match field_type with 
  | #Pb_field_type.builtin_type as builtin_type -> 
    let field_default = 
      resolve_builtin_type_field_default field_name builtin_type field_default
    in 
    (builtin_type, field_default)
    

  | `User_defined unresolved_field_type -> 
    let {Pb_field_type.type_name; _ } = unresolved_field_type in 
    let rec aux = function
      | [] -> raise Not_found
      | type_path :: tl -> 
        match Types_by_scope.find t type_path type_name with
        | type_ -> 
          let id = type_.Tt.id in 
          let field_default = 
            resolve_enum_field_default field_name type_ field_default 
          in 
          (`User_defined id, field_default)
        | exception Not_found -> aux tl  
    in 
    aux (compute_search_type_paths unresolved_field_type message_type_path) 

(* this function resolves all the field type of the given type *)
let resolve_type t type_ =

  let {
    Tt.scope; 
    id; 
    file_name; 
    file_options; 
    spec; 
  } = type_ in 

  match spec with
  | Tt.Enum e -> 
    {Tt.scope; id; file_name; file_options; spec = Tt.Enum e}

  | Tt.Message message -> 
    let {
      Tt.extensions; 
      message_options; 
      message_name; 
      message_body; 
    } = message in 

    let message_type_path = type_path_of_type type_ in 

    let resolve_field field =
      let {
        Tt.field_parsed; 
        field_type; 
        field_default;
        field_options; 
      } = field in 
      let field_name = field_parsed.Pt.field_name in 
      let field_type, field_default = 
        let do_resolve () = 
          resolve_field_type_and_default 
              t field_name field_type field_default message_type_path 
        in 
        match do_resolve () with
        | ret -> ret 
        | exception Not_found -> 
          E.unresolved_type ~field_name ~type_:"" ~message_name () 
      in 
      {Tt.field_parsed;field_type;field_default;field_options}
    in

    let message_body = List.map (function 
      | Tt.Message_field field -> 
        Tt.Message_field (resolve_field field)

      | Tt.Message_oneof_field oneof -> 
        let {Tt.oneof_name; oneof_fields} = oneof in 
        let oneof_fields = List.map resolve_field oneof_fields in 
        Tt.Message_oneof_field {Tt.oneof_name; oneof_fields}

      | Tt.Message_map_field map -> 
        let {
          Tt.map_name;
          map_number;
          map_key_type;
          map_value_type;
          map_options;
        } = map in 

        let field_default = None in 

        let map_value_type, _ = 
          resolve_field_type_and_default 
              t map_name map_value_type field_default message_type_path 
        in 

        Tt.Message_map_field {
          Tt.map_name; 
          map_number;
          map_options;
          map_key_type;
          map_value_type;
        }
    ) message_body in 

    let spec = Tt.Message {
      Tt.extensions; 
      message_options; 
      message_name; 
      message_body
    } in 
    {Tt.scope; id; file_name; file_options; spec}

let resolve_types types = 
  let t = List.fold_left Types_by_scope.add Types_by_scope.empty types in 
  List.map (resolve_type t) types 
