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

(** Typing compilation step *) 

(** This module performs the typing step in the compilation process by 
    doing the following:
    {ul
    {- Type tree construction and validation}
    {- Type resolution}
    {- Recursion analysis by grouping together all the mutually recursive
       types.}
    } *) 

module Tt = Pb_typing_type_tree 

val perform_typing :
  Pb_parsing_parse_tree.proto list -> 
  Pb_field_type.resolved Tt.proto_type list list 
(** [perform_typing parsed_tree] returned the type tree organized in groups 
    of fully resolved types. Each group contains all the mutually recursive 
    types and the type group by reverse dependency order. *) 
