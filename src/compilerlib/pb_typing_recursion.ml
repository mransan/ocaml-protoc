module Tt = Pb_typing_type_tree 
module Graph = Pb_typing_graph 

let node_of_proto_type = function 
  | {Tt.id; Tt.spec = Tt.Enum _ ; _ }  -> Graph.create_node id [] 
  | {Tt.id; Tt.spec = Tt.Message {Tt.message_body; _ }; _ } -> 
    let sub = List.flatten @@ List.map (function
      | Tt.Message_field {Tt.field_type; _ } -> 
        begin match field_type with
        | Tt.Field_type_type x -> [x]
        | _                      -> []
        end

      | Tt.Message_oneof_field {Tt.oneof_fields; _ } -> 
        List.flatten @@ List.map (fun {Tt.field_type; _ } ->
         match field_type with
         | Tt.Field_type_type x -> [x]
         | _ -> []
        ) oneof_fields  

      | Tt.Message_map_field {Tt.map_value_type; _ } -> 
         begin match map_value_type with
         | Tt.Field_type_type x -> [x]
         | _ -> []
         end

    ) message_body in 
    Graph.create_node id sub

let is_id input_id {Tt.id; _ } = (input_id = id) 

let group proto = 
  let g    = List.map node_of_proto_type proto  in 
  let g    = List.fold_left (fun m n -> 
    Graph.add_node n m 
  ) Graph.empty_graph g in 
  let sccs = Graph.tarjan g in 
  List.map (fun l -> 
    List.map (fun id -> List.find (is_id id) proto) l 
  ) sccs 
