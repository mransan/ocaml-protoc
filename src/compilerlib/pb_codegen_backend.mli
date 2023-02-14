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

(** Compile protobuf parse tree to the ocaml type *)

(** This module focuses on the compilation steps which transforms a
    fully resolved Protobuf message into an OCaml representation.

    After compilation this module also expose code generation
    functionality.  *)

module Tt = Pb_typing_type_tree
module Ot = Pb_codegen_ocaml_type

(** {2 Compilation } *)

val compile :
  unsigned_tag:bool ->
  Pb_field_type.resolved Tt.proto ->
  Pb_field_type.resolved Tt.proto_type ->
  Ot.type_ list

(** Internal helpers.

    For now there are no guarantees of stability within this module.
    It's unlikely that the signatures will change, but the possibility
    is still there.

    @since 2.4 *)
module Internal : sig
  val is_mutable : ?field_name:string -> Pb_option.set -> bool

  val constructor_name : string -> string
  val module_name : string -> string
  val label_name_of_field_name : string -> string
  val type_name : string list -> string -> string

  val variant_of_oneof :
    ?include_oneof_name:unit ->
    outer_message_names:string list ->
    unsigned_tag:bool ->
    'a Tt.proto -> Pb_option.set -> string -> int Tt.oneof -> Ot.variant
end
