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

module Int_map = Map.Make(struct 
  type t = int 
  let  compare (x:int) (y:int) = Stdlib.compare x y
end)

type node = {
  id  : int ;
  sub : int list; 
} 

let create_node id sub = {
  id; sub; 
} 

type graph = node Int_map.t

let empty_graph = Int_map.empty

let add_node ({id; _ } as n) g = 
  Int_map.add id n g 

module Tarjan = struct 

  type tnode = {
    core: node; 
    mutable index : int option; 
    mutable lowlink : int option; 
    mutable on_stack: bool
  }

  type tgraph = tnode Int_map.t 

  let reset g = 
    Int_map.map (fun core -> {
      core; 
      index  = None;
      lowlink = None; 
      on_stack = false;
    }) g 

  let rec strong_connect (g:tgraph) sccs stack (index:int) (v:tnode)  = 
  
    Pb_logger.log "[Graph] processing v [%i], index: %i\n" v.core.id index; 
    
    v.index <- Some index; 
    v.lowlink <- Some index; 
    let stack = v::stack in 
    v.on_stack <- true; 
  
    let sccs, stack, index = 
      List.fold_left (fun (sccs, stack, index) id -> 
        let (w:tnode)  = Int_map.find id g in 
  
        Pb_logger.log "[Graph] sub w [%i], w.index: %s\n" 
          w.core.id (Pb_util.Option.string_of_option string_of_int w.index);
        match w.index with 
        | Some _ -> 
          begin if w.on_stack 
          then v.lowlink <- Pb_util.Option.min_value v.lowlink w.index end;
          (sccs, stack, index) 
        | None ->  
          let sccs, stack, index = strong_connect g sccs stack (index + 1) w in  
          v.lowlink <- Pb_util.Option.min_value v.lowlink w.lowlink;
          (sccs, stack, index) 
      ) (sccs, stack, index) v.core.sub 
    in 
  
    Pb_logger.log "[Graph] after sub for v [%i], lowlink: %s, index: %s\n" 
      v.core.id 
      (Pb_util.Option.string_of_option string_of_int v.lowlink)
      (Pb_util.Option.string_of_option string_of_int v.index);
  
    Pb_logger.log "[Graph]   -> stack : %s\n" 
      ("[" ^ 
       (String.concat 
         ";" 
         (List.map (fun {core = {id; _ } ; _ } -> string_of_int id) stack)
       ) ^ 
       "]");

    if Pb_util.Option.eq_value v.lowlink v.index 
    then 
      let scc, stack, _ = List.fold_left (fun (scc, stack, splitted) n -> 
        if splitted 
        then 
          (scc, n::stack, splitted)
        else begin 
          n.on_stack <- false; 
          if n.core.id = v.core.id 
          then (n.core.id::scc, stack, true)
          else (n.core.id::scc, stack, false) 
        end
      ) ([], [], false) stack in 
      (scc::sccs, (List.rev stack), index) 
    else 
      (sccs, stack, index) 

  let tarjan g = 
    let g = reset g in 
    let sccs, _, _ = Int_map.fold (fun _ n (sccs, stack, index) -> 
      match n.index with 
      | Some _ -> (sccs, stack, index) 
      | None   -> strong_connect g sccs stack index n
    ) g ([], [], 0) in  
    sccs
end 

let tarjan = Tarjan.tarjan 
