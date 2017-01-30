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
module Tt = Pb_typing_type_tree 

let field_name {Tt.field_parsed; _ } = 
  let {Pt.field_name; _ } = field_parsed in 
  field_name 

let field_number {Tt.field_parsed = {Pt.field_number;_}; _ } = 
  field_number 

let field_type {Tt.field_type; _ } = 
  field_type

let field_label {Tt.field_parsed = {Pt.field_label; _ }; _ } = 
  field_label 

let field_default {Tt.field_default; _ } = field_default 

let field_options {Tt.field_options; _ } = field_options 

let field_option {Tt.field_options; _ } option_name = 
  Pb_option.get field_options option_name 
  
let empty_scope  = { 
  Tt.packages = []; 
  message_names = [] 
} 

let type_id_of_type {Tt.id; _ } = id 

let type_of_id all_types id  = 
  List.find (fun t -> type_id_of_type t = id) all_types 

let string_of_type_scope {Tt.packages; message_names}  = 
  Printf.sprintf "scope:{packages:%s, message_names:%s}" 
    (Pb_util.string_of_string_list packages)
    (Pb_util.string_of_string_list message_names) 

let string_of_message id scope message = 
  let {Tt.message_name; message_body; _} = message in  
  Printf.sprintf "message: {id:%i, message_scope:%s, name:%s, field nb:%i}" 
    id 
    (string_of_type_scope scope) 
    message_name
    (List.length message_body)

let message_option {Tt.message_options; _ } option_name = 
  Pb_option.get message_options option_name 

let enum_option {Tt.enum_options; _ } option_name = 
  Pb_option.get enum_options option_name 

let type_scope_of_type {Tt.scope; _ } = scope 

let is_empty_message = function 
  | {Tt.spec = Tt.Message {Tt.message_body;_}; _} -> 
    0 = List.length message_body 
  | _ -> false

let type_name_of_type = function
  | {Tt.spec = Tt.Enum {Tt.enum_name; _ }; _} -> enum_name 
  | {Tt.spec = Tt.Message {Tt.message_name; _ }; _} -> message_name
