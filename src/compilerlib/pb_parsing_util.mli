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

(** Parse tree utilities *)

(** This module defines convenient function to create and manipulate
    the parse tree.  *)

module Pt = Pb_parsing_parse_tree

(** {2 Creators } *) 

val field : 
  ?options:Pb_option.set ->
  label:Pt.message_field_label-> 
  number:int -> 
  type_:string -> 
  string -> 
  Pt.message_field

val map_field :
  ?options:Pb_option.set ->
  number:int ->
  key_type:string ->
  value_type:string ->
  string ->
  Pt.map_field 

val oneof_field : 
  ?options:Pb_option.set ->
  number:int -> 
  type_:string -> 
  string -> 
  Pt.oneof_field

val oneof :
  fields:Pt.oneof_field list -> 
  string -> 
  Pt.oneof 

val message_body_field : 
  Pt.message_field  -> 
  Pt.message_body_content  

val message_body_map_field : 
  Pt.map_field ->
  Pt.message_body_content  

val message_body_oneof_field  : 
  Pt.oneof -> 
  Pt.message_body_content 

val enum_value :
  int_value:int -> 
  string -> 
  Pt.enum_body_content

val enum_option :
  Pb_option.t -> 
  Pt.enum_body_content

val enum : 
  ?enum_body:Pt.enum_body_content list -> 
  string -> 
  Pt.enum 

val extension_range_single_number : int -> Pt.extension_range

val extension_range_range : int -> [ `Max | `Number of int ] -> Pt.extension_range 

val message_body_sub : 
  Pt.message -> 
  Pt.message_body_content

val message_body_enum: 
  Pt.enum -> 
  Pt.message_body_content

val message_body_extension: 
  Pt.extension_range list  -> 
  Pt.message_body_content

val message_body_reserved: 
  Pt.extension_range list  -> 
  Pt.message_body_content

val message_body_option : 
  Pb_option.t -> 
  Pt.message_body_content

val message : 
  content:Pt.message_body_content list -> 
  string -> 
  Pt.message

val import : ?public:unit -> string -> Pt.import 

val extend : string -> Pt.message_field list -> Pt.extend  

val proto: 
  ?syntax:string ->
  ?file_option:Pb_option.t -> 
  ?package:string -> 
  ?import:Pt.import -> 
  ?message:Pt.message ->
  ?enum:Pt.enum ->
  ?proto:Pt.proto -> 
  ?extend:Pt.extend -> 
  unit -> 
  Pt.proto
(** [proto ~message ~proto ()] will add [message] to [proto] and return a 
    the updated proto value. *)

val finalize_proto_value : Pt.proto -> Pt.proto

(** {2 Miscellaneous functionality } *)

val message_printer :?level:int -> Pt.message -> unit 
