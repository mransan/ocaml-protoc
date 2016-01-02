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

(** Code generation routine for the OCaml types. *) 

val caml_file_name_of_proto_file_name : string -> string 
(** [caml_file_name_of_proto_file_name f] returns the 
      ocaml base file name corresponding to the proto file name [f]. The
      extension (.ml or .mli) is the responsability of the caller. 
      
      It's important to note that similarly to Google protoc compiler, 
      the generated file name is a hard coding mapping to the proto filename. This
      is needed in order to independently generate included proto files. 
 *)

val gen_type : ?and_:unit -> Ocaml_types.type_ -> string 
(** [gen_type_const_variant v] generates the OCaml type declaration for [v]
 *)

val gen_decode :  ?and_:unit -> Ocaml_types.type_ -> string option 
(** [gen_decode_record record]  generates the function implementation for
    decoding a message into the given record type. 
 *)

val gen_decode_sig : Ocaml_types.type_ -> string option 
(** [gen_decode_sig_record record] generates the function signature for
    decoding a message into the given record type.
  *) 

val gen_encode : ?and_:unit -> Ocaml_types.type_ -> string option
(** [gen_encode t] generates the function signature for 
    encoding the given type [t] type into a protobuffer. 
  *)

val gen_encode_sig : Ocaml_types.type_ -> string option 
(** [gen_encode_sig record] generates the function signature for 
    encoding the given [record] type into a protobuffer. 
  *)

val gen_string_of : ?and_:unit -> Ocaml_types.type_ -> string option
(** [gen_string_of record] generates the function implementation for 
    computing a debug string of the given record
  *)

val gen_string_of_sig : Ocaml_types.type_ -> string option 
(** [gen_string_of_sig record] generates the function signature for 
    computing a debug string of the given record
 *)

val gen_default : ?and_:unit -> Ocaml_types.type_ -> string option
(** [gen_default record] generates a default value for the given [record]
  *)

val gen_default_sig : Ocaml_types.type_ -> string option
(** [gen_default_sig record] generates the signature of the default value for 
    the given [record].
  *)

(* --- Testing purpose onlye --- *)
val gen_mappings_record : Ocaml_types.record -> string
