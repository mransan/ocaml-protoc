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

(** Protobuffer Field type *)

type type_path = string list
(** Scope path of a type used for a message field.
    
    For instance in the following field defintion:

    [required foo.bar.Msg1 f = 1]

    The [type_path] would be [\["foo"; "bar"\]]
  *)

type unresolved = {
  type_path: type_path;
  type_name: string;
  from_root: bool;
      (** from_root indicates that the scope for the type is
                         from the root of the type system. (ie starts with '.')
                      *)
}
(** In the first phase of the compilation 
    the field of message type are not resolved but only 
    properly parsed. 
    
    The following type summarizes the information of a field
    type. 

    In the following field definition:
    
    [required foo.bar.Msg1 f = 1] 

    The unresolved type would be: [{
      scope=\["foo";"bar"\]; 
      type_name="Msg1"; 
      from_root = false
    }]
 *)

type resolved = int
(** After phase 2 compilation the field type is resolved to an 
    known message which can be uniquely identified by its id.
  *)

type builtin_type_floating_point =
  [ `Double
  | `Float
  ]
(** Floating point builtin types *)

type builtin_type_unsigned_int =
  [ `Uint32
  | `Uint64
  ]
(** Unsigned integer builtin types *)

type builtin_type_signed_int =
  [ `Int32
  | `Int64
  | `Sint32
  | `Sint64
  | `Fixed32
  | `Fixed64
  | `Sfixed32
  | `Sfixed64
  ]
(** Signed integer builtin types *)

type builtin_type_int =
  [ builtin_type_unsigned_int
  | builtin_type_signed_int
  ]
(** Integer builtin types *)

type map_key_type =
  [ builtin_type_int
  | `Bool
  | `String
  ]

type builtin_type =
  [ builtin_type_floating_point
  | builtin_type_int
  | `Bool
  | `String
  | `Bytes
  ]
(** Builtin type defined in protobuf *)

type 'a t =
  [ builtin_type
  | `User_defined of 'a (* Message or Enum type *)
  ]
(** field type. 
    
    The ['a] type is for re-using the same type 
    definition for the 2 compilation phases. 
    
    After Phase 1 ['a] is [unresolved] while after Phase2
    ['a] is [resolved].
  *)

type unresolved_t = unresolved t
type resolved_t = resolved t

val parse : string -> unresolved_t
val pp_type_path : Format.formatter -> type_path -> unit

val pp_builtin_type_floating_point :
  Format.formatter -> builtin_type_floating_point -> unit

val pp_builtin_type_unsigned_int :
  Format.formatter -> builtin_type_unsigned_int -> unit

val pp_builtin_type_signed_int :
  Format.formatter -> builtin_type_signed_int -> unit

val pp_builtin_type_int : Format.formatter -> builtin_type_int -> unit
val pp_map_key_type : Format.formatter -> map_key_type -> unit
val pp_builtin_type : Format.formatter -> builtin_type -> unit
val pp_unresolved : Format.formatter -> unresolved -> unit
val pp_resolved : Format.formatter -> resolved -> unit

val pp_type :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  [ builtin_type | `User_defined of 'a ] ->
  unit

val pp_unresolved_t :
  Format.formatter -> [ builtin_type | `User_defined of unresolved ] -> unit

val pp_resolved_t :
  Format.formatter -> [ builtin_type | `User_defined of resolved ] -> unit
