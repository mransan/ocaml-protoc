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
  type_path: type_path;
  type_name: string;
  from_root: bool;
      (** from_root indicates that the scope for the type is from the root of
          the type system. (ie starts with '.') *)
}

type resolved = int

type builtin_type_floating_point =
  [ `Double
  | `Float
  ]

type builtin_type_unsigned_int =
  [ `Uint32
  | `Uint64
  ]

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

type builtin_type_int =
  [ builtin_type_unsigned_int
  | builtin_type_signed_int
  ]

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

type 'a t =
  [ builtin_type
  | `User_defined of 'a (* Message or Enum type *)
  ]

type unresolved_t = unresolved t
type resolved_t = resolved t

let unresolved_of_string s =
  match Pb_util.rev_split_by_char '.' s with
  | [] -> Pb_exception.programmatic_error Pb_exception.Invalid_string_split
  | hd :: tl ->
    {
      type_path = List.rev tl;
      type_name = hd;
      from_root = String.get s 0 = '.';
    }

let parse = function
  | "double" -> `Double
  | "float" -> `Float
  | "int32" -> `Int32
  | "int64" -> `Int64
  | "uint32" -> `Uint32
  | "uint64" -> `Uint64
  | "sint32" -> `Sint32
  | "sint64" -> `Sint64
  | "fixed32" -> `Fixed32
  | "fixed64" -> `Fixed64
  | "sfixed32" -> `Sfixed32
  | "sfixed64" -> `Sfixed64
  | "bool" -> `Bool
  | "string" -> `String
  | "bytes" -> `Bytes
  | s -> `User_defined (unresolved_of_string s)

open Format

let rec pp_type_path ppf path =
  match path with
  | [] -> fprintf ppf "(empty)"
  | [ name ] -> fprintf ppf "%S" name
  | name :: rest ->
    fprintf ppf "%S." name;
    pp_type_path ppf rest

let pp_builtin_type_floating_point ppf t =
  match t with
  | `Double -> fprintf ppf "Double"
  | `Float -> fprintf ppf "Float"

let pp_builtin_type_unsigned_int ppf t =
  match t with
  | `Uint32 -> fprintf ppf "Uint32"
  | `Uint64 -> fprintf ppf "Uint64"

let pp_builtin_type_signed_int ppf t =
  match t with
  | `Int32 -> fprintf ppf "Int32"
  | `Int64 -> fprintf ppf "Int64"
  | `Sint32 -> fprintf ppf "Sint32"
  | `Sint64 -> fprintf ppf "Sint64"
  | `Fixed32 -> fprintf ppf "Fixed32"
  | `Fixed64 -> fprintf ppf "Fixed64"
  | `Sfixed32 -> fprintf ppf "Sfixed32"
  | `Sfixed64 -> fprintf ppf "Sfixed64"

let pp_builtin_type_int ppf t =
  match t with
  | #builtin_type_unsigned_int as unsigned_int ->
    pp_builtin_type_unsigned_int ppf unsigned_int
  | #builtin_type_signed_int as signed_int ->
    pp_builtin_type_signed_int ppf signed_int

let pp_map_key_type ppf t =
  match t with
  | #builtin_type_int as int_type -> pp_builtin_type_int ppf int_type
  | `Bool -> fprintf ppf "Bool"
  | `String -> fprintf ppf "String"

let pp_builtin_type ppf t =
  match t with
  | #builtin_type_floating_point as float_type ->
    pp_builtin_type_floating_point ppf float_type
  | #builtin_type_int as int_type -> pp_builtin_type_int ppf int_type
  | `Bool -> fprintf ppf "Bool"
  | `String -> fprintf ppf "String"
  | `Bytes -> fprintf ppf "Bytes"

let pp_unresolved ppf unresolved =
  fprintf ppf "{@[<v 2>@,type_path: %a;@,type_name: %S;@,from_root: %b@,@]}"
    pp_type_path unresolved.type_path unresolved.type_name unresolved.from_root

let pp_resolved ppf resolved = fprintf ppf "%d" resolved

let pp_type pp_user_defined ppf t =
  match t with
  | #builtin_type as built_in_type -> pp_builtin_type ppf built_in_type
  | `User_defined user_defined -> pp_user_defined ppf user_defined

let pp_unresolved_t ppf t = pp_type pp_unresolved ppf t
let pp_resolved_t ppf t = pp_type pp_resolved ppf t
