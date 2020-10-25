module E = Pb_exception
module Pt = Pb_parsing_parse_tree 

let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

let expect asserted expected = 
  if (asserted = expected)
  then ()
  else failwith (Printf.sprintf "Expected %s, but got %s" expected asserted)

let validate_constant_string key const =
  let open Pb_option in
  match const with
  | Constant_string s -> s
  | _ -> failwith (Printf.sprintf "Expected string value for option key %s" key)

let get_option options key =
  match (Pb_option.get options key) with
  | None -> failwith (Printf.sprintf "Could not find option with key %s" key)
  | Some x -> validate_constant_string key x 

let test_services () =
  let s = {|
  message Shelf {}
  message GetShelfRequest {}
  message GetShelfResponse {}
  message ListShelvesRequest {}
  message ListShelvesResponse {}

  service ShelfService {}
  service ShelfServiceV2 {
    option (some.config) = "service_shelf";

    rpc ListShelves (ListShelvesRequest) returns (ListShelvesResponse);

    rpc GetShelf (GetShelfRequest) returns (GetShelfResponse) {
      option (const.config) = "rpc_shelf";
      option (google.api.http) = {
        post: "/v1/shelves/{shelf}"
        body: "body"
      };
    }

    rpc GetShelfWithSemicolon (GetShelfRequest) returns (GetShelfResponse) {};
  }
  |}
  in 
  let open Pt in
  let { messages; services; _ } = parse Pb_parsing_parser.proto_ s in 
  assert (List.length messages = 5);
  assert (List.length services = 2);
  let v1 = List.nth services 0 in
  expect v1.service_name  "ShelfService";
  assert (List.length v1.service_body = 0);
  let v2 = List.nth services 1 in
  let { service_name; service_body } = v2 in
  expect service_name  "ShelfServiceV2";
  assert (List.length service_body = 4);
  let opt = List.nth service_body 0 in
  let rpc_list = List.nth service_body 1 in
  let rpc_get = List.nth service_body 2 in
  let rpc_get_with_semicolon = List.nth service_body 3 in
  let () = match opt with
    | Service_rpc _ -> failwith "Expected option, but got rpc"
    | Service_option (key, value) -> (
        let value = validate_constant_string key value in
        expect key "some.config";
        expect value "service_shelf";
      )
  in
  let () = match rpc_get_with_semicolon with
    | Service_option _ -> failwith "Expected option, but got rpc";
      (* checking that ending semis parse as well *)
    | Service_rpc {rpc_name; _} -> expect rpc_name "GetShelfWithSemicolon";
  in
  let () = match rpc_list with
    | Service_option _ -> failwith "Expected option, but got rpc";
    | Service_rpc {rpc_name; rpc_options;rpc_req; rpc_res} ->
      expect rpc_name "ListShelves";
      let req = match rpc_req with | `User_defined r -> r | _ -> failwith "Unexpected rpc req type" in
      let res = match rpc_res with | `User_defined r -> r | _ -> failwith "Unexpected rpc res type" in
      expect req.Pb_field_type.type_name "ListShelvesRequest";
      expect res.Pb_field_type.type_name "ListShelvesResponse";
      assert (rpc_options = Pb_option.empty);
  in
  let () = match rpc_get with
    | Service_option _ -> failwith "Expected option, but got rpc";
    | Service_rpc {rpc_name; rpc_options;rpc_req; rpc_res} ->
      expect rpc_name "GetShelf";
      let req = match rpc_req with | `User_defined r -> r | _ -> failwith "Unexpected rpc req type" in
      let res = match rpc_res with | `User_defined r -> r | _ -> failwith "Unexpected rpc res type" in
      expect req.Pb_field_type.type_name "GetShelfRequest";
      expect res.Pb_field_type.type_name "GetShelfResponse";
      assert (not (rpc_options = Pb_option.empty));
      let const_config = get_option rpc_options "const.config" in
      let api_http = get_option rpc_options "google.api.http" in
      expect const_config "rpc_shelf";
      expect api_http "post:/v1/shelves/{shelf}\nbody:body"
  in
  ()

let () =
  Printexc.record_backtrace true;
  match (test_services ()) with
  | () -> print_endline "Parse Service ... Ok"
  | exception e ->
    Printf.eprintf "Parse Service ... FAIL\n%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr
