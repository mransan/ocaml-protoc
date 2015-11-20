
type programmatic_error =
  | Invalid_string_split 
  | Unexpected_field_type 
  | No_type_found_for_id 

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

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

val unresolved_type : 
  field_name:string -> 
  type_:string  ->
  message_name:string -> 
  unit -> exn

val duplicated_field_number : 
  field_name:string ->
  previous_field_name:string -> 
  message_name:string -> 
  unit -> exn 

val invalid_default_value : 
  field_name:string -> 
  info:string ->
  unit -> exn

val unsupported_field_type : 
  field_name:string ->
  field_type:string -> 
  backend_name:string ->
  unit -> exn

val programmatic_error : programmatic_error -> exn

val invalid_import_qualifier : unit -> exn

val invalid_file_name : string -> exn 
