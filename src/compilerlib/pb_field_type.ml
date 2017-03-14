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

type type_path = string list 

type unresolved = {
  type_path : type_path; 
  type_name : string; 
  from_root : bool;  (** from_root indicates that the scope for the type is
                         from the root of the type system. (ie starts with '.')
                      *) 
}

type resolved = int 

type builtin_type_floating_point = [ 
  | `Double 
  | `Float 
]

type builtin_type_unsigned_int  = [
  | `Uint32 
  | `Uint64
]

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

type builtin_type_int= [ 
  |  builtin_type_unsigned_int 
  |  builtin_type_signed_int
]

type map_key_type = [
  | builtin_type_int
  | `Bool 
  | `String 
]

type builtin_type = [
  | builtin_type_floating_point
  | builtin_type_int
  | `Bool 
  | `String 
  | `Bytes 
]

type 'a t = [ 
  | builtin_type          
  | `User_defined of 'a   (* Message or Enum type *)
]  

type unresolved_t = unresolved t 

type resolved_t = resolved t

let unresolved_of_string s = 
  match Pb_util.rev_split_by_char '.' s with 
  | [] -> Pb_exception.programmatic_error Pb_exception.Invalid_string_split
  | hd :: tl -> {
    type_path = (List.rev tl); 
    type_name = hd;
    from_root = String.get s 0 = '.';
  }

let parse = function
 | "double"    -> `Double 
 | "float"     -> `Float 
 | "int32"     -> `Int32 
 | "int64"     -> `Int64 
 | "uint32"    -> `Uint32 
 | "uint64"    -> `Uint64
 | "sint32"    -> `Sint32 
 | "sint64"    -> `Sint64 
 | "fixed32"   -> `Fixed32 
 | "fixed64"   -> `Fixed64 
 | "sfixed32"  -> `Sfixed32 
 | "sfixed64"  -> `Sfixed64
 | "bool"      -> `Bool 
 | "string"    -> `String 
 | "bytes"     -> `Bytes 
 | s           -> `User_defined (unresolved_of_string s) 
