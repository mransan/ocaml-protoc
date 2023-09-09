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

module Pt = Pb_parsing_parse_tree
module Tt = Pb_typing_type_tree

let perform_typing (protos : Pt.proto list) : Pb_field_type.resolved Tt.proto =
  let validated_types, validated_services =
    List.fold_left
      (fun (typed_protos, services) proto ->
        let val_proto = Pb_typing_validation.validate proto in
        ( typed_protos @ List.flatten val_proto.proto_types,
          services @ val_proto.proto_services ))
      ([], []) protos
  in

  let t, types = Pb_typing_resolution.resolve_types validated_types in
  let services = Pb_typing_resolution.resolve_services t validated_services in

  {
    Tt.proto_types = List.rev @@ Pb_typing_recursion.group types;
    proto_services = services;
  }
