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

module Pt = Pb_parsing_parse_tree

(** Protobuf typed tree. 

    The typetree type is parametrized to allow for 2 phase compilation. 
 *)

(** Field definition. 
    
    {ul
    {- ['a] is for [unresolved] or [resolved]}
    {- ['b] is for [field_label] to account for both normal and one of fields.}
    } *)
type ('a, 'b) field = {
  field_parsed : 'b Pt.field; 
  field_type : 'a Pb_field_type.t; 
  field_default : Pb_option.constant option; 
  field_options : Pb_option.set; 
}

type 'a oneof_field = ('a, Pt.oneof_field_label) field 

type 'a message_field = ('a, Pt.message_field_label) field  

(** Map definition *)
type 'a map_field = {
  map_name : string;
  map_number : int;
  map_key_type : Pb_field_type.map_key_type;
  map_value_type : 'a Pb_field_type.t;
  map_options : Pb_option.set;
}

(** Oneof definition *)
type 'a oneof = {
  oneof_name : string; 
  oneof_fields : 'a oneof_field list; 
}

(** Type scope 
      
    The scope of a type (message or enum) is defined by the package 
    (defined in the top of the proto file as well as the messages above 
    it since a message definition can be nested *)
type type_scope = {
  packages : string list; 
  message_names : string list; 
}

(** item for the message body *)
type 'a message_body_content = 
  | Message_field       of 'a message_field 
  | Message_oneof_field of 'a oneof 
  | Message_map_field   of 'a map_field 

and 'a message = {
  extensions : Pt.extension_range list;
  message_options : Pb_option.set;
  message_name : string; 
  message_body : 'a message_body_content list; 
}

type enum_value = {
  enum_value_name: string; 
  enum_value_int : int;
}

type enum = {
  enum_name : string; 
  enum_values: enum_value list; 
  enum_options : Pb_option.set; 
}

type 'a proto_type_spec = 
  | Enum    of enum 
  | Message of 'a message

type 'a proto_type  = {
  scope : type_scope;
  id :  int; 
  file_name : string; 
  file_options : Pb_option.set;
  spec : 'a proto_type_spec;
}

type 'a proto = 'a proto_type list 

