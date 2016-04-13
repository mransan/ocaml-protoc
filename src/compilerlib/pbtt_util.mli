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

(** Type compilation. 
 
    This module is responsible for the typing of the parsed tree. 
    All field types are eventually resolved. 

    Additionally this module provide convenient function
    to manipulate the typed tree. 
  *)

(** {2 Accessors for Pbtt.field type} *)

val field_name   : ('a, 'b)  Pbtt.field -> string 
(** [field_name field] returns the name [field] *)

val field_number : ('a, 'b)  Pbtt.field -> int
(** [field_number field] returns the number of [field] *)

val field_type   : ('a, 'b)  Pbtt.field -> 'a Pbtt.field_type
(** [field_type field] returns the type of [field] *)

val field_label  : ('a, 'b)  Pbtt.field -> 'b 
(** [field_label field] returns the label of [field] *)

val field_default : ('a, 'b)  Pbtt.field -> Pbpt.constant option
(** [field_default field] returns the default value of [field] *)

val field_options : ('a, 'b) Pbtt.field -> Pbpt.field_options

val find_field_option : Pbpt.field_options -> string -> Pbpt.constant option 

val field_option : ('a, 'b) Pbtt.field -> string -> Pbpt.constant option
(** [field_option field option_name] returns the constant associated with 
    [option_name]. If the fields options does not contain [option_name] [None]
    is returned.
  *)

val type_of_id : 'a Pbtt.proto -> int -> 'a Pbtt.proto_type 
(** [type_of_id all_types id] returns the type associated with the given id, 
    @raise [Not_found] if the type is not in the all_types. 
  *)

val string_of_message : int -> Pbtt.type_scope -> 'a Pbtt.message -> string 

(** {2 Accessor for Pbtt.type *) 

val type_id_of_type : 'a Pbtt.proto_type -> int 
(** [type_id_of_type t] returns the type unique identifier. *)

val type_name_of_type : 'a Pbtt.proto_type -> string
(** [type_name_of_type t] returns the type name (as defined in the message file) 
    of [t].
 *)

val type_scope_of_type : 'a Pbtt.proto_type -> Pbtt.type_scope
(** [type_scope_of_type t] returns the scope of type [t]. *)

val is_empty_message : 'a Pbtt.proto_type -> bool 
(** [is_empty_message t] returns true if [t] is a message type and 
    has no fields defined. 
 *)

(** {2 Creator} *) 

val empty_scope : Pbtt.type_scope 

val scope_of_package : string option -> Pbtt.type_scope

(** {2 Compilation routines} *) 

(** Compilation is done in 2 phases. 
    {ul 
    {- Phase 1 focuses on flattenning the nested messages and doing a first round
    of type checking for the various message field. The field type will be
    either matched with a basic type or parsed into the [unresolved] data
    structure. This step simply verify that the type definition is well formed
    but does not check the field type is pointing to an existing type. }

    {- Phase 2 focuses on type resolution. This phase implement the scoping
    rules defined in the protocol buffer specification to resolve a field type 
    to a previously defined message. This phase additionally check that field
    numbers and field names are unique within a message but this logic should be
    moved to Phase 1}
    }
    (** TODO add the grouping phase *)
 *)

val compile_proto_p1  : 
  string -> 
  Pbpt.proto ->
  Pbtt.unresolved Pbtt.proto
(** [compile_proto_p1 file_name proto] makes a first phase compilation of the 
    parsed tree. 
 *)

val compile_proto_p2: 
  Pbtt.unresolved Pbtt.proto -> 
  Pbtt.unresolved Pbtt.proto_type -> 
  Pbtt.resolved Pbtt.proto_type

val group: Pbtt.resolved Pbtt.proto -> Pbtt.resolved Pbtt.proto list 
(** TODO *)

(** {2 For testing only} *) 

val compile_message_p2: 
  Pbtt.unresolved Pbtt.proto -> 
  Pbtt.type_scope -> 
  Pbtt.unresolved Pbtt.message -> 
  Pbtt.resolved Pbtt.message 

val compile_message_p1 : 
  string -> 
  Pbpt.file_option list ->
  Pbtt.type_scope -> 
  Pbpt.message ->
  Pbtt.unresolved Pbtt.proto

val compile_oneof_p1: Pbpt.oneof -> Pbtt.unresolved Pbtt.oneof

val compile_field_p1: 'a Pbpt.field -> (Pbtt.unresolved, 'a ) Pbtt.field 

val find_all_types_in_field_scope : 
  'a Pbtt.proto -> 
  Pbtt.field_scope-> 
  'a Pbtt.proto
