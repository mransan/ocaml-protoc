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

(** This module defines convenient function to create and manipulate
    the types define in the Ast module.
  *)

(** {2 Creators } *) 

val field : 
  ?options:Pbpt.field_options ->
  label:Pbpt.field_label-> 
  number:int -> 
  type_:string -> 
  string -> 
  Pbpt.field_label Pbpt.field

val map :
  ?options:Pbpt.field_options ->
  number:int ->
  key_type:string ->
  value_type:string ->
  string ->
  Pbpt.map

val oneof_field : 
  ?options:Pbpt.field_options ->
  number:int -> 
  type_:string -> 
  string -> 
  Pbpt.oneof_label Pbpt.field

val oneof :
  fields:Pbpt.oneof_label Pbpt.field list -> 
  string -> 
  Pbpt.oneof 

val message_body_field : 
  Pbpt.field_label Pbpt.field  -> 
  Pbpt.message_body_content  

val message_body_map_field : 
  Pbpt.map ->
  Pbpt.message_body_content  

val message_body_oneof_field  : 
  Pbpt.oneof -> 
  Pbpt.message_body_content 

val enum_value :
  int_value:int -> 
  string -> 
  Pbpt.enum_value 

val enum : 
  ?enum_values:Pbpt.enum_value list -> 
  string -> 
  Pbpt.enum 

val extension_range_single_number : int -> Pbpt.extension_range

val extension_range_range : int -> [ `Max | `Number of int ] -> Pbpt.extension_range 

val message_body_sub : 
  Pbpt.message -> 
  Pbpt.message_body_content

val message_body_enum: 
  Pbpt.enum -> 
  Pbpt.message_body_content

val message_body_extension: 
  Pbpt.extension_range list  -> 
  Pbpt.message_body_content

val message_body_option : 
  Pbpt.message_option -> 
  Pbpt.message_body_content

val message : 
  content:Pbpt.message_body_content list -> 
  string -> 
  Pbpt.message

val import : ?public:unit -> string -> Pbpt.import 

val extend : string -> Pbpt.field_label Pbpt.field list -> Pbpt.extend  

val proto: 
  ?syntax:string ->
  ?file_option:Pbpt.file_option -> 
  ?package:string -> 
  ?import:Pbpt.import -> 
  ?message:Pbpt.message ->
  ?enum:Pbpt.enum ->
  ?proto:Pbpt.proto -> 
  ?extend:Pbpt.extend -> 
  unit -> 
  Pbpt.proto

(** {2 Miscellaneous functionality } *)

val message_printer :?level:int -> Pbpt.message -> unit 

val file_option : Pbpt.file_option list -> string -> Pbpt.constant option 
(** [file_option file_options name] return [Some file_option] for the given
    [name]
 *)
