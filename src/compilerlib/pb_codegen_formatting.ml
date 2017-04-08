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

type scope = {
  mutable items : item list; 
} 

and item = 
  | Line of string 
  | Scope of scope 

let empty_scope () = 
  {items = []} 

let line scope s =
  scope.items <- (Line s)::scope.items  

let linep scope format = 
  Printf.ksprintf (line scope) format

let empty_line scope = 
  line scope "" 

let scope scope f = 
  let sub_scope = empty_scope () in 
  f sub_scope; 
  scope.items <- (Scope sub_scope)::scope.items  

let print scope = 
  let rec loop acc i = function
    | (Line s)::tl -> 
      loop ((Pb_util.indentation_prefix i ^ s)::acc) i tl  
    | (Scope {items})::tl -> 
      let sub = loop [] (i + 1) items in  
      loop (sub @ acc) i tl  
    | [] -> acc
  in 

  String.concat "\n" @@ loop [] 0 scope.items 
