
(** {2 Compilation errors } *)

module P = Printf

type programmatic_error =
  | Invalid_string_split 
  | Unexpected_field_type 
  | No_type_found_for_id 

let string_of_programmatic_error e =  
  "Programatic_error" ^ match e with
  | Invalid_string_split -> "string split error"
  | Unexpected_field_type -> "unexpected field type"
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


exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

let () =
  Printexc.register_printer (fun exn ->
    match exn with
    | Compilation_error (Unresolved_type { field_name; type_; message_name}) -> 
      Some (P.sprintf 
        "unresolved type for field name : %s (type:%s, in message: %s)" 
        field_name type_ message_name
      )
    | Compilation_error (Duplicated_field_number 
        {field_name; previous_field_name; message_name}) -> 
      Some (P.sprintf 
        "duplicated field number for field name: %s (previous field name:%s, message: %s)"
        field_name previous_field_name message_name
      )
    | Compilation_error (Invalid_default_value {field_name; info} ) -> 
      Some (P.sprintf "invalid default value for field name:%s (info: %s)"
        field_name info
      )
    | Compilation_error (Unsupported_field_type {field_name; field_type; backend_name}) -> 
      Some (P.sprintf "unsupported field type for field name:%s with type:%s in bakend: %s"
        field_name field_type backend_name
      )
    | Compilation_error (Programatic_error e) -> 
      Some (P.sprintf "programmatic error: %s" (string_of_programmatic_error e)) 
    | Compilation_error Invalid_import_qualifier ->
      Some "Invalid import qualified, only 'public' supported"
    | Compilation_error (Invalid_file_name file_name) -> 
      let s = Printf.sprintf 
        ("Invalid file name: %s, " ^^ 
         "format must <name>.proto") file_name  
      in
      Some s
    | Compilation_error (Import_file_not_found file_name) -> 
      let s = Printf.sprintf 
        ("File: %s, " ^^ 
         "could not be found.") file_name  
      in
      Some s
    | _         -> None
    )

let unresolved_type ~field_name ~type_ ~message_name () = 
  (Compilation_error (Unresolved_type {
    field_name; 
    type_; 
    message_name
  }))

let duplicated_field_number ~field_name ~previous_field_name ~message_name  () = 
  (Compilation_error (Duplicated_field_number {
    field_name; 
    previous_field_name; 
    message_name;
  }))

let invalid_default_value ~field_name ~info () = 
  (Compilation_error (Invalid_default_value {field_name; info} ))

let unsupported_field_type ~field_name ~field_type ~backend_name () = 
  Compilation_error (Unsupported_field_type {
    field_name;
    field_type;
    backend_name;
  })

let import_file_not_found file_name = 
  Compilation_error (Import_file_not_found file_name) 

let programmatic_error e = Compilation_error (Programatic_error e) 

let invalid_import_qualifier () = 
  Compilation_error Invalid_import_qualifier

let invalid_file_name file_name = 
  Compilation_error (Invalid_file_name file_name) 
