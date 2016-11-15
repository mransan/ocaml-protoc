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

(** Graph algorithms to support the recursion analysis *)

(** {2 Types} *)

type node 
(** Node in a graph, each node is identified using a unique [int] id *)

type graph 
(** Graph. *)

(** {2 Creators } *) 

val create_node : int -> int list -> node 
(** [create_node id sub] create a node uniquely identified with [id] 
    and connections to other nodes in [sub]. 

    The client application is responsible to ensure that the graph is
    consistent, by adding all nodes identified in [sub] to the same 
    graph later. 
  *)

val empty_graph : graph 
(** [empty_graph ()] create a new empty graph.*)

val add_node : node -> graph -> graph 
(** [add_node node graph] add [node] to [graph] 
 *)

(** {2 Algorithms} *)

val tarjan : graph -> int list list  
(** [tarjan graph] compute the ordered list of strongly connected components of
    a graph. 

    The returned list is order in decreasing order of dependencies. This means
    the last component of the list does not link to any other components. 
 *)
