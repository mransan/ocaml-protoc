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

(** {2 Compilation errors } *)

module P = Printf
module Loc = Pb_location

type programmatic_error =
  | Invalid_string_split 
  | No_type_found_for_id 

let string_of_programmatic_error e =  
  "Programatic_error" ^ match e with
  | Invalid_string_split -> "string split error"
  | No_type_found_for_id  -> "no type was found for type id" 

type unresolved_type = {
  field_name: string; 
  type_:string; 
  message_name:string 
}

type duplicate_field_number = {
  field_name: string; 
  previous_field_name  : string;
  message_name: string; 
}

type invalid_default_value = {
  field_name: string option; 
  info: string; 
}

type unsupported_field_type = {
  field_name: string option; 
  field_type: string; 
  backend_name:string;
}

type error = 
  | Unresolved_type of unresolved_type 
    (* When the type of a field could not be resolved *) 
  | Duplicated_field_number of duplicate_field_number 
    (* When there are 2 field with either identical number or name *)
  | Invalid_default_value of invalid_default_value 
    (* When a default value type type does not match the field type *)
  | Unsupported_field_type of unsupported_field_type 
  | Programatic_error of programmatic_error 
  | Invalid_import_qualifier of Loc.t  
  | Invalid_file_name of string  
  | Import_file_not_found of string 
  | Invalid_packed_option of string 
  | Missing_semicolon_for_enum_value of string * Loc.t
  | Invalid_enum_specification of string * Loc.t 
  | Invalid_mutable_option of string option  
  | Missing_one_of_name of Loc.t 
  | Missing_field_label of string * string 
  | Parsing_error of string * Loc.t
  | Invalid_ppx_extension_option of string 
  | Invalid_protobuf_syntax of string 
  | Invalid_proto3_field_label of string * string 
  | Default_field_option_not_supported of string * string 
  | Invalid_first_enum_value_proto3 of string * string option 
  | Invalid_key_type_for_map of string 
  | Unsupported_wrapper_type of string 

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

let string_of_error = function 
  | Unresolved_type { field_name; type_; message_name} -> 
    P.sprintf 
      "unresolved type for field name : %s (type:%s, in message: %s)" 
      field_name type_ message_name

  | Duplicated_field_number {field_name; previous_field_name; message_name} -> 
    P.sprintf 
      "duplicated field number for field name: %s (previous field name:%s, message: %s)"
      field_name previous_field_name message_name

  | Invalid_default_value {field_name; info}  -> 
    P.sprintf "invalid default value for field name:%s (info: %s)"
      (Pb_util.Option.default "" field_name) info

  | Unsupported_field_type {field_name; field_type; backend_name} -> 
    P.sprintf "unsupported field type for field name:%s with type:%s in bakend: %s"
      (Pb_util.Option.default "" field_name) field_type backend_name

  | Programatic_error e -> 
    P.sprintf "programmatic error: %s" (string_of_programmatic_error e)

  | Invalid_import_qualifier loc ->
    P.sprintf "%sInvalid import qualified, only 'public' supported" (Loc.to_string loc) 

  | Invalid_file_name file_name -> 
    P.sprintf ("Invalid file name: %s, " ^^ "format must <name>.proto") file_name  

  | Import_file_not_found file_name -> 
    P.sprintf ("File: %s, " ^^ "could not be found.") file_name  

  | Invalid_packed_option field_name ->
    P.sprintf "Invalid packed option for field: %s" field_name

  | Missing_semicolon_for_enum_value (enum_value, loc)-> 
    P.sprintf "%sMissing semicolon for enum value: %s" (Loc.to_string loc) enum_value

  | Missing_one_of_name loc -> 
    P.sprintf "%sMissing oneof name" (Loc.to_string loc) 

  | Missing_field_label (field_name, message_name) -> 
    P.sprintf 
      ("Missing field label for field: %s in message: %s. "^^ 
      "[required|repeated|optional] expected") 
      field_name message_name  

  | Parsing_error (detail, loc) -> 
    Printf.sprintf "%s%s." (Loc.to_string loc) detail

  | Invalid_enum_specification (enum_name, loc) -> 
    P.sprintf 
      "%sMissing enum specification (<identifier> = <id>;) for enum value: %s"
      (Loc.to_string loc) enum_name
  
  | Invalid_mutable_option field_name -> 
    P.sprintf "Invalid mutable option for field %s" (Pb_util.Option.default "" field_name) 
  
  | Invalid_ppx_extension_option message_name -> 
    P.sprintf "Invalid ppx extension value for message: %s, string expected" message_name
  
  | Invalid_protobuf_syntax syntax -> 
    P.sprintf 
      "Invalid protobuf syntax: \"%s\", expected \"proto2\" or \"proto3\"" 
      syntax

  | Invalid_proto3_field_label (field_name, message_name) ->
    P.sprintf 
      ("Invalid field label for field: %s in message: %s. " ^^ 
       "[Required|Optional] are not supported.")
      field_name message_name
  
  | Default_field_option_not_supported (field_name, message_name) -> 
    P.sprintf 
      ("Explicit default values are not allowed in proto3. " ^^ 
       "field name: %s, message name: %s.")
      field_name message_name
  
  | Invalid_first_enum_value_proto3 (enum_name, message_name) -> 
    P.sprintf 
      ("The first enum value must be 0 in proto3." ^^ 
       "enum name: %s, message name: %s.")
      enum_name (Pb_util.Option.string_of_option (fun x -> x) message_name)

  | Invalid_key_type_for_map field_name -> 
    P.sprintf
      ("Invalid key type for field %s. Map key type only supports integral " ^^
       "and string type"
      ) 
      field_name

  | Unsupported_wrapper_type type_name -> 
    P.sprintf
      "Unsupported wrapper type %s, please raise issue on github."
      type_name

let () =
  Printexc.register_printer (fun exn ->
    match exn with
    | Compilation_error e -> Some (string_of_error e)
    | _                   -> None
    )

let unresolved_type ~field_name ~type_ ~message_name () = 
  raise (Compilation_error (Unresolved_type {
    field_name; 
    type_; 
    message_name
  }))

let duplicated_field_number ~field_name ~previous_field_name ~message_name  () = 
  raise (Compilation_error (Duplicated_field_number {
    field_name; 
    previous_field_name; 
    message_name;
  }))

let invalid_default_value ?field_name ~info () = 
  raise (Compilation_error (Invalid_default_value {field_name; info} ))

let unsupported_field_type ?field_name ~field_type ~backend_name () = 
  raise (Compilation_error (Unsupported_field_type {
    field_name;
    field_type;
    backend_name;
  }))

let import_file_not_found file_name = 
  raise (Compilation_error (Import_file_not_found file_name)) 

let programmatic_error e = 
  raise (Compilation_error (Programatic_error e)) 

let invalid_import_qualifier loc  = 
  raise (Compilation_error (Invalid_import_qualifier loc))

let invalid_file_name file_name = 
  raise (Compilation_error (Invalid_file_name file_name)) 

let invalid_packed_option field_name = 
  raise (Compilation_error (Invalid_packed_option field_name)) 

let missing_semicolon_for_enum_value enum_value loc = 
  raise (Compilation_error (Missing_semicolon_for_enum_value (enum_value, loc)))

let invalid_enum_specification enum_name loc = 
  raise (Compilation_error (Invalid_enum_specification (enum_name, loc)))
  
let invalid_mutable_option ?field_name () = 
  raise (Compilation_error (Invalid_mutable_option field_name))

let missing_one_of_name loc = 
  raise (Compilation_error (Missing_one_of_name loc))

let missing_field_label ~field_name ~message_name = 
  raise (Compilation_error (Missing_field_label (field_name, message_name)))
  
let invalid_ppx_extension_option message_name = 
  raise (Compilation_error (Invalid_ppx_extension_option message_name)) 

let ocamlyacc_parsing_error loc = 
  raise (Compilation_error (Parsing_error ("Parsing error",loc)))

let protoc_parsing_error e loc = 
  raise (Compilation_error (Parsing_error (string_of_error e, loc)))

let unknown_parsing_error detail loc = 
  raise (Compilation_error (Parsing_error (detail, loc)))

let invalid_protobuf_syntax syntax =
  raise (Compilation_error (Invalid_protobuf_syntax syntax))

let invalid_proto3_field_label ~field_name ~message_name = 
  raise (Compilation_error (Invalid_proto3_field_label (field_name,
      message_name)))

let default_field_option_not_supported ~field_name ~message_name = 
  raise (Compilation_error (Default_field_option_not_supported (field_name,
      message_name)))

let invalid_first_enum_value_proto3 ?message_name ~enum_name () = 
  raise (Compilation_error (Invalid_first_enum_value_proto3 (enum_name,
      message_name)))
  
let invalid_key_type_for_map field_name = 
  raise (Compilation_error (Invalid_key_type_for_map field_name)) 

let unsupported_wrapper_type type_name = 
  raise (Compilation_error (Unsupported_wrapper_type type_name)) 
