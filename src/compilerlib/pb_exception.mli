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

(** {2 Types} *)

type programmatic_error =
  | Invalid_string_split 
  | No_type_found_for_id 

type error

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

(** {2 Raise Functions} *)

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
  ?field_name:string -> 
  info:string ->
  unit -> 'a

val unsupported_field_type : 
  ?field_name:string ->
  field_type:string -> 
  backend_name:string ->
  unit -> 'a

val import_file_not_found : string -> 'a 

val programmatic_error : programmatic_error -> 'a

val invalid_import_qualifier : Pb_location.t -> 'a

val invalid_file_name : string -> 'a 

val invalid_packed_option : string -> 'a 

val missing_semicolon_for_enum_value : string -> Pb_location.t -> 'a 

val invalid_enum_specification : string -> Pb_location.t -> 'a 

val invalid_mutable_option : ?field_name:string -> unit -> 'a

val missing_one_of_name : Pb_location.t -> 'a 

val missing_field_label : field_name:string -> message_name:string -> 'a 

val invalid_ppx_extension_option : string -> 'a 

val ocamlyacc_parsing_error : Pb_location.t -> 'a 

val protoc_parsing_error : error -> Pb_location.t -> 'a 

val unknown_parsing_error : string -> Pb_location.t -> 'a 

val invalid_protobuf_syntax : string -> 'a 

val invalid_proto3_field_label : 
  field_name:string -> 
  message_name:string -> 
  'a 

val default_field_option_not_supported : 
  field_name:string -> 
  message_name:string -> 
  'a 

val invalid_first_enum_value_proto3 :
  ?message_name:string -> 
  enum_name:string -> 
  unit ->
  'a

val invalid_key_type_for_map : string -> 'a

val unsupported_wrapper_type : string -> 'a
