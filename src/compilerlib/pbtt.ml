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

(** Protobuffer Typed tree. 

    The typetree type is parametrized to allow for 2 phase compilation. 
 *)

(** scope of the field type when the field is a message type. 
    
    For instance in the following field defintion:

    [required foo.bar.Msg1 f = 1]

    The [field_scope] would be [["foo"; "bar"]]
  *)
type field_scope = string list 


(** In the first phase of the compilation 
    the field of message type are not resolved but only 
    properly parsed. 
    
    The following type summarizes the information of a field
    type. 

    In the following field definition:
    
    [required foo.bar.Msg1 f = 1] 

    The unresolved type would be {
      scope=["foo";"bar"]; 
      type_name="Msg1"; 
      from_root = false
    }
 *)
type unresolved = {
  scope     : field_scope; 
  type_name : string; 
  from_root : bool;  (** from_root indicates that the scope for the type is
                         from the root of the type system. (ie starts with '.')
                      *) 
}

(** After phase 2 compilation the field type is resolved to an 
    known message which can be uniquely identified by its id.
  *)
type resolved = int 

(** field type. 
    
    The ['a] type is for re-using the same type 
    definition for the 2 compilation phases. 
    
    After Phase 1 ['a] is [unresolved] while after Phase2
    ['a] is [resolved].
  *)
type 'a field_type = 
  | Field_type_double 
  | Field_type_float 
  | Field_type_int32 
  | Field_type_int64 
  | Field_type_uint32 
  | Field_type_uint64
  | Field_type_sint32 
  | Field_type_sint64 
  | Field_type_fixed32 
  | Field_type_fixed64 
  | Field_type_sfixed32 
  | Field_type_sfixed64
  | Field_type_bool 
  | Field_type_string 
  | Field_type_bytes 
  | Field_type_type of 'a 

(** field definition. 
    
    ['a] is for [unresolved] or [resolved]
    ['b] is for [field_label] to account for both normal and one of fields. 
  *)
type ('a, 'b) field = {
  field_parsed : 'b Pbpt.field; 
  field_type : 'a field_type; 
  field_default : Pbpt.constant option; 
  field_options : Pbpt.field_options; 
}

(** oneof definition *)
type 'a oneof = {
  oneof_name : string; 
  oneof_fields : ('a, Pbpt.oneof_label) field list; 
}

type 'a map = {
  map_name : string;
  map_number : int;
  map_key_type : 'a field_type;
  map_value_type : 'a field_type;
}

(** type scope 
    
    The scope of a type (message or enum) is defined by the package (defined in the 
    top of the proto file as well as the messages above it since a 
    message definition can be nested
 *)
type type_scope = {
  packages : string list; 
  message_names : string list; 
}

(** item for the message body
 *)
type 'a message_body_content = 
  | Message_field       of ('a, Pbpt.field_label) field 
  | Message_oneof_field of 'a oneof 
  | Message_map_field   of 'a map

and 'a message = {
  extensions : Pbpt.extension_range list;
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
}

type 'a proto_type_spec = 
  | Enum    of enum 
  | Message of 'a message

type 'a proto_type  = {
  scope : type_scope;
  id :  int; 
  file_name : string; 
  file_options : Pbpt.file_option list;
  spec : 'a proto_type_spec;
}

type 'a proto = 'a proto_type list 
