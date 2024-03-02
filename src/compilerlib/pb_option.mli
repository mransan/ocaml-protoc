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

(** Protobuf File/Message/Field options

    This module represents "compiled" option set, which is high level
    representation, as opposed to low-level representation in Pb_raw_option
    module.

    For the following "raw" set of options:

    {[
      option (google.api.http).custom.kind = "FETCH";
      option (google.api.http).custom.path = "/foo/bar/baz/{id}";
      option (google.api.http).additional_bindings = {
          get: "/foo/bar/baz/{id}"
      };
      option (google.api.http).additional_bindings = {
          post: "/foo/bar/baz/"
          body: "*"
      };
    ]}

    The "compiled" representation will have only one option [(google.api.http)],
    which is a message:

    {[
      option (google.api.http) = {
          custom: {
              kind: "FETCH"
              path: "/foo/bar/baz/{id}"
          }
          additional_bindings: [
            {
                get: "/foo/bar/baz/{id}"
            },
            {
                post: "/foo/bar/baz/"
                body: "*"
            }
          ]
      };
    ]}

    Option normalization is happening in
    [Pb_typing_validation.normalize_option], destructured field assigments are
    normalized back to nested messages. See
    [Pb_typing_validation.compile_option] to see the full process of option
    compilation. *)

(** Protobuf constant

    As defined in: {{:https://goo.gl/FzpQY2} Protobuf Language Spec}. *)
type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type message_literal = (string * value) list
and list_literal = value list

and value =
  | Scalar_value of constant
  | Message_literal of message_literal
  | List_literal of list_literal

(** Top level option name *)
type option_name =
  | Simple_name of string
  | Extension_name of string

type t = option_name * value

type set = t list
(** Compiled collection of options *)

val stringify_option_name : option_name -> string
val empty : set

val add : set -> option_name -> value -> set
(** [add set name value] adds option [(name, value)] into the [set]. Option name
    and value are expected to be normalized (see
    [Pb_typing_validation.normalize_option]). [add] is merging nested message
    literals within option value with the ones that were previously added to the
    [set]. *)

val get : set -> option_name -> value option

val get_ext : set -> string -> value option
(** [get_ext set name] is a helper that retrieves [Extension_name name] option
    from [set] *)

val pp_constant : Format.formatter -> constant -> unit
val pp_value : Format.formatter -> value -> unit
val pp_message_literal : Format.formatter -> message_literal -> unit
val pp_message_field : Format.formatter -> string * value -> unit
val pp_t : Format.formatter -> t -> unit
val pp_set : Format.formatter -> set -> unit
