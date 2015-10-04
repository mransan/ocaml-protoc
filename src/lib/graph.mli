(** Light-weigth graph utilities to produce strongly connected component from 
    a graph using Tarjan algorithm once.
  *)

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
