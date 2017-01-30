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

(** Typed tree utilities *)

(** This module provides accessor/creator function to facilitate 
    the manipulation of the type tree. *)

module Pt = Pb_parsing_parse_tree
module Tt = Pb_typing_type_tree 

(** {2 Accessors for Tt.field type } *)

val field_name   : ('a, 'b)  Tt.field -> string 
(** [field_name field] returns the name [field] *)

val field_number : ('a, 'b)  Tt.field -> int
(** [field_number field] returns the number of [field] *)

val field_type   : ('a, 'b)  Tt.field -> 'a Pb_field_type.t
(** [field_type field] returns the type of [field] *)

val field_label  : ('a, 'b)  Tt.field -> 'b 
(** [field_label field] returns the label of [field] *)

val field_default : ('a, 'b)  Tt.field -> Pb_option.constant option
(** [field_default field] returns the default value of [field] *)

val field_options : ('a, 'b) Tt.field -> Pb_option.set

val field_option : ('a, 'b) Tt.field -> string -> Pb_option.constant option
(** [field_option field option_name] returns the constant associated with 
    [option_name]. If the fields options does not contain [option_name] [None]
    is returned.
  *)

val type_of_id : 'a Tt.proto -> int -> 'a Tt.proto_type 
(** [type_of_id all_types id] returns the type associated with the given id, 
    raise [Not_found] if the type is not in the all_types. 
  *)

val string_of_message : int -> Tt.type_scope -> 'a Tt.message -> string 

val message_option : 'a Tt.message -> string -> Pb_option.constant option 

val enum_option : Tt.enum -> string -> Pb_option.constant option 

(** {2 Accessor for Tt.type} *) 

val type_name_of_type : 'a Tt.proto_type -> string
(** [type_name_of_type t] returns the type name (as defined in the 
    message file) of [t].
 *)

val type_scope_of_type : 'a Tt.proto_type -> Tt.type_scope
(** [type_scope_of_type t] returns the scope of type [t]. *)

val is_empty_message : 'a Tt.proto_type -> bool 
(** [is_empty_message t] returns true if [t] is a message type and 
    has no fields defined. 
 *)

(** {2 Creator} *) 

val empty_scope : Tt.type_scope 
