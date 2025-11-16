open Ocaml_protoc_compiler_lib
module E = Pb_exception
module Pt = Pb_parsing_parse_tree

let process_field_options ppf field =
  let options =
    field.Pt.field_options |> Pb_typing_validation.compile_options
  in
  if options <> Pb_option.empty then (
    let parsed =
      Pb_option.get_ext options "validate.rules"
      |> Option.map Validate.decode_pb_options_field_rules
    in
    Format.fprintf ppf
      "====================== <OPTIONS> \
       ======================@.%a@.===================== </OPTIONS> \
       ======================@.======================= <PARSED> \
       =======================@.%a@.====================== </PARSED> \
       =======================@.@.@."
      Pb_option.pp_set options
      (Format.pp_print_option Validate.pp_field_rules)
      parsed;
    ()
  )

let run proto =
  let protos =
    Pb_parsing.parse_file
      (fun f ->
        match f with
        | "test.proto" -> f, proto
        | _ -> f, "")
      "test.proto"
  in
  let ppf = Format.std_formatter in
  Format.set_margin 150;
  protos
  |> List.iter (fun proto ->
         proto.Pt.messages
         |> List.iter (fun message ->
                message.Pt.message_body
                |> List.iter (function
                     | Pt.Message_field field -> process_field_options ppf field
                     | _ -> ())))

let test_cases =
  [
    {|
      syntax = "proto3";
      
      package examplepb;
      
      import "validate/validate.proto";
      
      message Person {
        uint64 id = 1 [(validate.rules).uint64.gt = 999];
      
        string email = 2 [(validate.rules).string.email = true];
      
        string name = 3 [(validate.rules).string = {
          pattern:   "^[^[0-9]A-Za-z]+( [^[0-9]A-Za-z]+)*$",
          max_bytes: 256,
        }];
      
        Location home = 4 [(validate.rules).message.required = true];
      
        message Location {
          double lat = 1 [(validate.rules).double = {gte: -90,  lte: 90}];
          double lng = 2 [(validate.rules).double = {gte: -180, lte: 180}];
        }
      }
    |};
  ]

let () = List.iter run test_cases
