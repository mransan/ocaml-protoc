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

(** Formatting utilities for code generation *)

(* TODO: add a little example of how to use this module as well
 * as a unit test for it *)

(** {2 types} *) 

type scope 
(** A scope is a formatting container which can contains line of text as well 
    as other nested scopes

    In other word a scope define an indentation section.
 *)

(** {2 Creation} *) 

val empty_scope : unit -> scope 
(** [empty_scope ()] returns a brand new scope *)

val line : scope -> string -> unit 
(** [line scope s] adds [s] to [scope] *)

val linep : scope -> ('a, unit, string, unit) format4 -> 'a  

val empty_line : scope -> unit 
(** [empty_line scope] adds an empty line to [scope] *)

val scope : scope -> (scope -> unit) -> unit 
(** [scope scope f] adds a sub scope and apply [f] to it. *)

(** {2 Printing} *) 

val print : scope -> string 
(** [print scope] returns the formatted scops with a 2 character 
    indentation for each scope. 
 *)
