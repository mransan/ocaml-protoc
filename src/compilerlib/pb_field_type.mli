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

(** Scope path of a type used for a message field.
    
    For instance in the following field defintion:

    [required foo.bar.Msg1 f = 1]

    The [type_path] would be [\["foo"; "bar"\]]
  *)
type type_path = string list 

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
type unresolved = {
  type_path : type_path; 
  type_name : string; 
  from_root : bool;  (** from_root indicates that the scope for the type is
                         from the root of the type system. (ie starts with '.')
                      *) 
}

(** After phase 2 compilation the field type is resolved to an 
    known message which can be uniquely identified by its id.
  *)
type resolved = int 

(** Floating point builtin types *)
type builtin_type_floating_point = [ 
  | `Double 
  | `Float 
]

(** Unsigned integer builtin types *)
type builtin_type_unsigned_int  = [
  | `Uint32 
  | `Uint64
]

(** Signed integer builtin types *) 
type builtin_type_signed_int = [
  | `Int32 
  | `Int64 
  | `Sint32 
  | `Sint64 
  | `Fixed32 
  | `Fixed64 
  | `Sfixed32 
  | `Sfixed64 
]

(** Integer builtin types *)
type builtin_type_int= [ 
  |  builtin_type_unsigned_int 
  |  builtin_type_signed_int
]

type map_key_type = [
  | builtin_type_int
  | `Bool 
  | `String 
]

(** Builtin type defined in protobuf *)
type builtin_type = [
  | builtin_type_floating_point
  | builtin_type_int
  | `Bool 
  | `String 
  | `Bytes 
]

(** field type. 
    
    The ['a] type is for re-using the same type 
    definition for the 2 compilation phases. 
    
    After Phase 1 ['a] is [unresolved] while after Phase2
    ['a] is [resolved].
  *)
type 'a t = [ 
  | builtin_type          
  | `User_defined of 'a   (* Message or Enum type *)
]  

type unresolved_t = unresolved t 

type resolved_t = resolved t

val parse : string -> unresolved_t
