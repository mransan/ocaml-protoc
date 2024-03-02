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

(** Protobuf File/Message/Field raw options (i.e. exactly as they parsed) *)

(*
Option names in Protobuf are complicated. See their syntax definition:


OptionName = ( SimpleName | ExtensionName ) [ dot OptionName ] .

SimpleName    = identifier .
ExtensionName = l_paren TypeName r_paren



Example of option names are below:

deprecated
json_name
(foo.bar)
abc.def.xyz
(foo).bar.(.baz.bob)


This module is low-level, it tracks list of options as they were parsed, option
name is a list of parts according to syntax specification. Option can be present
multiple times, which means it's a destructured list (see [group_list_values]).

Good read on Protobuf options: https://github.com/bufbuild/protobuf-language-spec/blob/main/language-spec.md#options
*)

type name_part =
  | Simple_name of string
  | Extension_name of string

type option_name = name_part list
type t = option_name * Pb_option.value
type set = t list

val stringify_option_name : option_name -> string
val empty : set
val add : set -> option_name -> Pb_option.value -> set

val merge : set -> set -> set
(** [merge s1 s2] adds all the options from [s2] to [s1]. This means
    than in case of duplicates [s2] options will override [s1] options. *)

val get : set -> option_name -> Pb_option.value option
val get_ext : set -> string -> Pb_option.value option
val get_simple : set -> string -> Pb_option.value option

val group_list_values : set -> set
(** [group_list_values set] groups options with the same name into Pb_option.List_literal

The following set of options:

{[
  option (google.api.http).additional_bindings = {
      get: "/foo/bar/baz/{id}"
  };
  option (google.api.http).additional_bindings = {
      post: "/foo/bar/baz/"
      body: "*"
  };
]}

Is equivalent to the below non-destructured version:

{[
  option (google.api.http) = {
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
*)

val pp_t : Format.formatter -> t -> unit
val pp_set : Format.formatter -> set -> unit
