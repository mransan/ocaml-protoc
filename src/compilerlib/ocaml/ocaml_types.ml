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

(** Generated OCaml type for protobuf messages *) 

type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

type field_encoding = {
  field_number : int; 
  payload_kind : payload_kind; 
  nested : bool;
  default: Pbpt.constant option;
  packed: bool; (**
  https://developers.google.com/protocol-buffers/docs/encoding#packed *)
}

type user_defined_type = {
  module_ : string option; 
  type_name : string; 
}

type field_type = 
  | Unit   (** currently being used for empty protobuf message *)
  | String 
  | Float 
  | Int 
  | Int32 
  | Int64
  | Bytes
  | Bool
  | User_defined_type of user_defined_type

type field_name = string 

type type_qualifier = 
  | No_qualifier
  | Option
  | List 
  | Repeated_field

(** the field is parametrized by the encoding_type with could either 
    [field_encoding] or [record_encoding_type] depending 
    if the field is used in a variant or record type
 *) 
type 'a afield = {
  field_type : field_type; 
  field_name : field_name; 
  type_qualifier : type_qualifier; 
  encoding : 'a;
  mutable_ : bool;
}

type variant_encoding = 
  | Inlined_within_message 
  | Standalone 
(** protobuf type system does not explicitely support standalone variant type since
    `one of` fields can only be within a `message` type. 

   However we support an optimization so that protobuf messages which only
   contain a single `one of` field are mapped to an OCaml variant rather 
   than a record with a single field of a variant type. This optimization allow
   cleaner and more consice generated code. 

   Therefore an OCaml variant can be encoded into 2 different way. Either it is 
   a standalone variant or it is actually part of a record. This information
   needs to be kept track of to generate the code accordingly. 
 *)

type variant = {
  variant_name : string; 
  variant_constructors : field_encoding afield list; 
  variant_encoding : variant_encoding; 
}

type const_variant = {
  cvariant_name : string; 
  cvariant_constructors : (string * int) list ;
}

type record_encoding_type = 
  | Regular_field of field_encoding
  | One_of        of variant  

type record = {
  record_name: string; 
  fields : record_encoding_type afield list; 
}

type type_spec = 
  | Record of record 
  | Variant of variant
  | Const_variant  of const_variant 

type type_ = {
  module_ : string; (* For now limit to a single module *)  
  spec : type_spec; 
}
