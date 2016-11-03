open Pb_typing_graph

let create_node  = create_node 

let print_sccs sccs = 
  Pb_logger.endline @@ "[Test] " ^ "[" ^ (String.concat ";" (List.map (fun l -> 
    "[" ^ (String.concat ";" (List.map string_of_int l)) ^ "]"
    ) sccs )) ^ "]"
 
let () = 
  let g = 
    empty_graph
    |> add_node (create_node 2 [1;3]) 
    |> add_node (create_node 1 []) 
    |> add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[2]; [3]; [1]] = sccs)
  
let () = 
  let g = 
    empty_graph
    |> add_node (create_node 1 [2;]) 
    |> add_node (create_node 2 [3]) 
    |> add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1]; [2]; [3]] = sccs)
  
let () = 
  let g = 
    empty_graph
    |> add_node (create_node 1 [2;3]) 
    |> add_node (create_node 2 [1;]) 
    |> add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1;2]; [3]] = sccs)
  
let () = 
  let g = 
    empty_graph
    |> add_node (create_node 1 [2;3]) 
    |> add_node (create_node 2 [3;]) 
    |> add_node (create_node 3 [1;]) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1;2;3]] = sccs)
  
let () = 
  print_endline "Graph Test ... Ok"
