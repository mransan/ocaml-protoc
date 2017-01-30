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

module Tt = Pb_typing_type_tree 
module Graph = Pb_typing_graph 

let node_of_proto_type = function 
  | {Tt.id; Tt.spec = Tt.Enum _ ; _ }  -> Graph.create_node id [] 
  | {Tt.id; Tt.spec = Tt.Message {Tt.message_body; _ }; _ } -> 

    let list_of_field_type = function
      | `User_defined x  -> [x]
      | #Pb_field_type.builtin_type -> []
    in
    (* TODO : this whole flatten thing is a bit hacky
     * we should develop a clearer solution *)

    let sub = List.flatten @@ List.map (function
      | Tt.Message_field {Tt.field_type; _ } -> 
        list_of_field_type field_type 

      | Tt.Message_oneof_field {Tt.oneof_fields; _ } -> 
        List.flatten @@ List.map (fun {Tt.field_type; _ } ->
         list_of_field_type field_type 
        ) oneof_fields  

      | Tt.Message_map_field {Tt.map_value_type; _ } -> 
         list_of_field_type map_value_type

    ) message_body in 
    Graph.create_node id sub

let is_id input_id {Tt.id; _ } = (input_id = id) 

let group proto = 
  (* construction *)
  let g    = List.map node_of_proto_type proto  in 
  let g    = List.fold_left (fun m n -> 
    Graph.add_node n m 
  ) Graph.empty_graph g in 

  (* Strongly Connected Components construction *)
  let sccs = Graph.tarjan g in 

  (* map back *)
  List.map (fun l -> 
    List.map (fun id -> List.find (is_id id) proto) l 
  ) sccs 
