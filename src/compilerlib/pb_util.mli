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

(** Standard library missing functions *)

val rev_split_by_char : char -> string -> string list
(** [rev_split_by_char c s] will split the string [s] using the delimieter [c]
    and return the component in reverse order (ie from right to left).

    For instance when splitting a filename with the '.' character, the file
    extension will be the head of the returned list. *)

val string_of_string_list : string list -> string
(** [string_of_string_list l] returns a debug string of [l] *)

val string_fold_lefti : ('a -> int -> char -> 'a) -> 'a -> string -> 'a
(** [string_fold_lefti f e0 s] will fold over each string character *)

val indentation_prefix : int -> string
(** [indentation_prefix level] returns a string of [2 * level] spaces *)

module String : sig
  val starts_with :
    prefix:(* comment thwarts tools/sync_stdlib_docs *) string -> string -> bool
  (** [starts_with ][~prefix s] is [true] if and only if [s] starts with
    [prefix].
    *)
end

module Option : sig
  val default : 'a -> 'a option -> 'a
  (** [option_default x o] returns [x] is [o] is [None] otherwise [y] when [o]
      is [Some y]. *)

  val min_value : 'a option -> 'a option -> 'a option
  (** [min_value x y] returns the min value of the x and y if x and y are of the
      form [Some _]. If [x] or [y] is [None] then [Invalid_argument] exception
      is raised *)

  val eq_value : 'a option -> 'a option -> bool
  (** [eq_value x y] returns [true] if both [x] and [y] are of the form
      [Some _]. If [x] or [y] is [None] then [Invalid_argument] exception is
      raised *)

  val string_of_option : ('a -> string) -> 'a option -> string
  (** [string_of_option f x] returns string representation of [x] using [f] when
      [x] is of the form [Some _]. *)
end

val read_file : string -> string
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a

module List : sig
  val pop_last : 'a list -> 'a list
  (** [pop_last l] removes the last element from the list *)

  val apply_until : ('a -> 'b option) -> 'a list -> 'b option
  (** [apply_until f l] applies [f ei] until it returns [Some x]

      If the end of the list is reached without [f] returning [Some _] then
      [None] is returned. *)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] returns the list of element [x] for which [f] returned
      [Some x]. The length of the returned list will be less or equal than the
      length of the input list [l]. *)

  val find_opt : ('a -> bool) -> 'a list -> 'a option
  val find_map : ('a -> 'b option) -> 'a list -> 'b option

  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  (** [equal eq [a1; ...; an] [b1; ..; bm]] holds when
    the two input lists have the same length, and for each
    pair of elements [ai], [bi] at the same position we have
    [eq ai bi].

    Note: the [eq] function may be called even if the
    lists have different length. If you know your equality

    function is costly, you may want to check {!compare_lengths}
    first.
*)
end

module Str_map : Map.S with type key = string
module Int_map : Map.S with type key = int
