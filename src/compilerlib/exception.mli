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

(** Compiler exception *) 

type programmatic_error =
  | Invalid_string_split 
  | Unexpected_field_type 
  | No_type_found_for_id 
  | One_of_should_be_inlined_in_message

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

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

val unresolved_type : 
  field_name:string -> 
  type_:string  ->
  message_name:string -> 
  unit -> 'a

val duplicated_field_number : 
  field_name:string ->
  previous_field_name:string -> 
  message_name:string -> 
  unit -> 'a 

val invalid_default_value : 
  field_name:string -> 
  info:string ->
  unit -> 'a

val unsupported_field_type : 
  field_name:string ->
  field_type:string -> 
  backend_name:string ->
  unit -> 'a

val import_file_not_found : string -> 'a 

val programmatic_error : programmatic_error -> 'a

val invalid_import_qualifier : unit -> 'a

val invalid_file_name : string -> 'a 

val invalid_message_declaration : string -> 'a 

val invalid_packed_option : string -> 'a 
(** [invalid_packed_option field_name] *)
