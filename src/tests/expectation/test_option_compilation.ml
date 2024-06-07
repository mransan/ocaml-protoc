module E = Pb_exception
module Pt = Pb_parsing_parse_tree

let pp_compiled ppf options =
  Format.fprintf ppf "RAW:@.%a@.@.COMPILED:@.%a" Pb_raw_option.pp_set options
    Pb_option.pp_set
    (Pb_typing_validation.compile_options options)

let run proto =
  let maybe_protos =
    try
      Pb_parsing.parse_file
        (fun f ->
          match f with
          | "test.proto" -> f, proto
          | _ -> f, "")
        "test.proto"
      |> Result.ok
    with e -> Error e
  in
  let pp_maybe_protos ppf = function
    | Ok [ (proto : Pt.proto) ] ->
      (match proto.messages with
      | [ message ] ->
        let raw_options =
          message.message_body
          |> List.filter_map (function
               | Pt.Message_option opt -> Some opt
               | Pt.Message_field field ->
                 Format.fprintf ppf "-- field options --@.%a@.@." pp_compiled
                   field.field_options;
                 None
               | _ -> None)
          |> List.fold_left
               (fun oneof_options (name, value) ->
                 Pb_raw_option.add oneof_options name value)
               Pb_raw_option.empty
        in
        Format.fprintf ppf "-- message options --@.%a" pp_compiled raw_options
      | _ -> Format.fprintf ppf "[!] Only one message is expected")
    | Ok _ -> Format.fprintf ppf "[!] Only one proto is expected"
    | Error exn -> Format.fprintf ppf "[!] EXN: %s" (Printexc.to_string exn)
  in
  let ppf = Format.std_formatter in
  Format.set_margin 149;
  Format.fprintf ppf
    "====================== <PROTO> \
     ======================@.%a@.===================== </PROTO> \
     ======================@.======================= <OPTIONS COMPILATION> \
     =======================@.%a@.====================== </OPTIONS \
     COMPILATION> =======================@.@.@."
    Format.pp_print_string proto pp_maybe_protos maybe_protos

let test_cases =
  [
    {|
      message M {
        option packed = true;
      }
    |};
    {|
      message PackedField {
        repeated float inner = 1 [packed=true];
      }
    |};
    {|
      syntax = "proto3";
      message PackedField {
        repeated float inner = 1 [packed=true];
      }
    |};
  ]

let () = List.iter run test_cases
