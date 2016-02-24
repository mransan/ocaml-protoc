open Graph

let create_node  = Graph.create_node 

let print_sccs sccs = 
  Logger.endline @@ "[Test] " ^ "[" ^ (String.concat ";" (List.map (fun l -> 
    "[" ^ (String.concat ";" (List.map string_of_int l)) ^ "]"
    ) sccs )) ^ "]"
 
let () = 
  let g = 
    Graph.empty_graph
    |> Graph.add_node (create_node 2 [1;3]) 
    |> Graph.add_node (create_node 1 []) 
    |> Graph.add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[2]; [3]; [1]] = sccs)
  
let () = 
  let g = 
    Graph.empty_graph
    |> Graph.add_node (create_node 1 [2;]) 
    |> Graph.add_node (create_node 2 [3]) 
    |> Graph.add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1]; [2]; [3]] = sccs)
  
let () = 
  let g = 
    Graph.empty_graph
    |> Graph.add_node (create_node 1 [2;3]) 
    |> Graph.add_node (create_node 2 [1;]) 
    |> Graph.add_node (create_node 3 []) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1;2]; [3]] = sccs)
  
let () = 
  let g = 
    Graph.empty_graph
    |> Graph.add_node (create_node 1 [2;3]) 
    |> Graph.add_node (create_node 2 [3;]) 
    |> Graph.add_node (create_node 3 [1;]) 
  in 
  let sccs = tarjan g in 
  print_sccs sccs; 
  assert ([[1;2;3]] = sccs)
  
let () = 
  print_endline "Graph Test ... Ok"
