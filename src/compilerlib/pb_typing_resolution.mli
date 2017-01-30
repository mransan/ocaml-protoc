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

(** Type resolution *) 

(** This module resolves all the user defined for the protobuf message 
    fields; while built-in types were previously valided in 
    [Pb_typing_validation] module. 

    After this resolution is complete each user defined field type will
    be a link to a unique message type identify by its id; in other word 
    the type graph will is complete. 

    If the resolution fails then [Pb_exception.Compilation_error] is 
    raised.
  *)

module Tt = Pb_typing_type_tree 

(** Custom container for all the types (message or enums) which are 
    organized by their scope. This allow efficient search of a type given 
    its type path 
    
    The construction of this container is the first step in the type
    resolution. *)
module Types_by_scope : sig 

  type t 
  (** container type *)

  val empty : t 
  (** empty container *)

  val add : 
    t -> 
    Pb_field_type.unresolved Tt.proto_type -> 
    t 
  (** add a protobuf type *)
  
  val find : 
    t -> 
    Pb_field_type.type_path -> 
    string -> 
    Pb_field_type.unresolved Tt.proto_type  
  (** find a protobuf type given its type path *)

  val print : t -> unit 
  (** pretty print of the container *)

end (* Types_by_scope *) 

val resolve_types : 
  Pb_field_type.unresolved Tt.proto_type list ->
  Pb_field_type.resolved Tt.proto_type list  
(** [resolve_types types] resolves all the field types for all the [types]. 
    If a field cannot be resolved then [Pb_exception.Compilation_error] is 
    raised. *)
