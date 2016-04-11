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
  | Unexpected_field_type 
  | No_type_found_for_id 
  | One_of_should_be_inlined_in_message

type error

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)


val add_loc : Loc.t -> exn  -> exn 

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

val invalid_import_qualifier : Loc.t -> 'a

val invalid_file_name : string -> 'a 

val invalid_message_declaration : string -> 'a 

val invalid_packed_option : string -> 'a 

val missing_semicolon_for_enum_value : string -> Loc.t -> 'a 

val invalid_enum_specification : string -> Loc.t -> 'a 

val invalid_mutable_option : ?field_name:string -> unit -> 'a

val missing_one_of_name : Loc.t -> 'a 

val invalid_field_label : Loc.t -> 'a 

val missing_field_label : Loc.t -> 'a 

val parsing_error : string -> int -> string -> 'a  

val syntax_error : unit  -> 'a 
