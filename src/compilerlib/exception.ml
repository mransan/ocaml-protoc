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

type programmatic_error =
  | Invalid_string_split 
  | Unexpected_field_type 
  | No_type_found_for_id 
  | One_of_should_be_inlined_in_message

let string_of_programmatic_error e =  
  "Programatic_error" ^ match e with
  | Invalid_string_split -> "string split error"
  | Unexpected_field_type -> "unexpected field type"
  | No_type_found_for_id  -> "no type was found for type id" 
  | One_of_should_be_inlined_in_message -> "one of variant encoding must be inlined in message"

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
  field_name: string; 
  info: string; 
}

type unsupported_field_type = {
  field_name: string; 
  field_type: string; 
  backend_name:string;
}

type error = 
  | Unresolved_type of unresolved_type 
    (** When the type of a field could not be resolved *) 
  | Duplicated_field_number of duplicate_field_number 
    (** When there are 2 field with either identical number or name *)
  | Invalid_default_value of invalid_default_value 
    (** When a default value type type does not match the field type *)
  | Unsupported_field_type of unsupported_field_type 
  | Programatic_error of programmatic_error 
  | Invalid_import_qualifier 
  | Invalid_file_name of string  
  | Import_file_not_found of string 
  | Invalid_message_declaration of string 
  | Invalid_packed_option of string 
  | Missing_semicolon_for_enum_value of string
  | Invalid_enum_specification of string 
  | Missing_one_of_name 
  | Syntax_error of Location.t 


exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

let prepare_error = function 
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
      field_name info

  | Unsupported_field_type {field_name; field_type; backend_name} -> 
    P.sprintf "unsupported field type for field name:%s with type:%s in bakend: %s"
      field_name field_type backend_name

  | Programatic_error e -> 
    P.sprintf "programmatic error: %s" (string_of_programmatic_error e)

  | Invalid_import_qualifier ->
    "Invalid import qualified, only 'public' supported"

  | Invalid_file_name file_name -> 
    P.sprintf 
      ("Invalid file name: %s, " ^^ 
       "format must <name>.proto") file_name  

  | Import_file_not_found file_name -> 
    P.sprintf 
      ("File: %s, " ^^ 
       "could not be found.") file_name  

  | Invalid_message_declaration s -> 
    P.sprintf "Invalid message declaration : %s" s

  | Invalid_packed_option field_name ->
    P.sprintf "Invalid packed option for field: %s" field_name

  | Missing_semicolon_for_enum_value enum_value -> 
    P.sprintf "Missing semicolon for enum value: %s" enum_value

  | Missing_one_of_name -> 
    P.sprintf "Missing oneof name"

  | Syntax_error loc -> 
    let line    = loc.Location.loc_start.Lexing.pos_lnum in
    P.sprintf "Syntax error at line: %i" line  

  | Invalid_enum_specification enum_name -> 
    P.sprintf 
      "Missing enum specification (<identifier> = <id>;) for enum value: %s" enum_name

let () =
  Printexc.register_printer (fun exn ->
    match exn with
    | Compilation_error e -> Some (prepare_error e)
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

let invalid_default_value ~field_name ~info () = 
  raise (Compilation_error (Invalid_default_value {field_name; info} ))

let unsupported_field_type ~field_name ~field_type ~backend_name () = 
  raise (Compilation_error (Unsupported_field_type {
    field_name;
    field_type;
    backend_name;
  }))

let import_file_not_found file_name = 
  raise (Compilation_error (Import_file_not_found file_name)) 

let programmatic_error e = 
  raise (Compilation_error (Programatic_error e)) 

let invalid_import_qualifier () = 
  raise (Compilation_error Invalid_import_qualifier)

let invalid_file_name file_name = 
  raise (Compilation_error (Invalid_file_name file_name)) 

let invalid_message_declaration s = 
  raise (Compilation_error (Invalid_message_declaration s))

let invalid_packed_option field_name = 
  raise (Compilation_error (Invalid_packed_option field_name)) 

let missing_semicolon_for_enum_value enum_value = 
  raise (Compilation_error (Missing_semicolon_for_enum_value enum_value))

let invalid_enum_specification enum_name = 
  raise (Compilation_error (Invalid_enum_specification enum_name))

let missing_one_of_name () = 
  raise (Compilation_error Missing_one_of_name)

let syntax_error loc = 
  raise (Compilation_error (Syntax_error loc)) 
