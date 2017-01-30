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

(** Typed tree construction and validation *)

(** This module focuses on building the Type tree from the parse tree
    and provides a number of validation in doing so. 

    Not that the field types are not resolved, and the returned type tree is
    hence of type [unresolved_field_type Pb_typing_type_tree.proto_type]. 
    The type resolution is done in [Pb_typing_resolution] *)

module Pt = Pb_parsing_parse_tree 
module Tt = Pb_typing_type_tree 

(** {2 Type tree construction} *) 

val validate : 
  Pt.proto ->
  Pb_field_type.unresolved Tt.proto
(** [validate file_name proto] makes a first phase compilation of the 
    parsed tree.  *)

(** {2 Testing Only} *) 

val validate_message : 
  ?parent_options:Pb_option.set ->
  string -> 
  Pb_option.set -> (* file options *) 
  Tt.type_scope -> 
  Pt.message ->
  Pb_field_type.unresolved Tt.proto

