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

(** Mutually recursive type resolution *) 

(** The protobuf syntax allows the definition of mutually recursive
    types, however this declaration is implicit (ie no dedicated syntax
    to group together types which are mutually recursive). 

    OCaml requires mutually recursive types to be defined with an explicit 
    syntax (using the [and] keyword), therefore it is necessary to 
    find all the mutually recursive protobuf types. 
    
    This module performs the recursion analysis using the Tarjan graph 
    algorithm to find all the Strongly Connnected Components. *)
     
module Tt = Pb_typing_type_tree 

val group : 
  Pb_field_type.resolved Tt.proto -> 
  Pb_field_type.resolved Tt.proto list 
  (** [group types] returns the list of all the mutually recursive group 
      of types in reverse order of dependency. In other the last group of 
      types of the returned list don't depend on any other types. *)
