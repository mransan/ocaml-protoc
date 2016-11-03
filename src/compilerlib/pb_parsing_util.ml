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

module E = Pb_exception 
module Pt = Pb_parsing_parse_tree

let field ?options:(options =[]) ~label ~number ~type_ name = {
  Pt.field_name = name; 
  Pt.field_number = number;
  Pt.field_type = type_; 
  Pt.field_label = label;
  Pt.field_options = options;
} 

let map ?options:(map_options = []) ~number ~key_type ~value_type name = {
  Pt.map_name = name;
  Pt.map_number = number;
  Pt.map_key_type = key_type;
  Pt.map_value_type = value_type;
  Pt.map_options;
}

let oneof_field ?options:(options =[]) ~number ~type_ name = {
  Pt.field_name = name; 
  Pt.field_number = number;
  Pt.field_type = type_; 
  Pt.field_options = options;
  Pt.field_label = `Oneof
} 

let oneof ~fields name = {
  Pt.oneof_name = name;
  Pt.oneof_fields = fields;
}

let message_counter = ref 0

let enum_value ~int_value name = Pt.(Enum_value { 
  enum_value_name = name; 
  enum_value_int = int_value; 
})

let enum_option option_ = Pt.Enum_option option_ 

let enum ?enum_body:(enum_body= []) enum_name =
  incr message_counter; 
  {
    Pt.enum_id  = !message_counter; 
    Pt.enum_name; 
    Pt.enum_body; 
  } 

let extension_range_single_number number = 
  Pt.Extension_single_number number 

let extension_range_range from to_ = 
  let to_ = match to_ with
    | `Max      -> Pt.To_max 
    | `Number i -> Pt.To_number i  
  in 
  Pt.Extension_range (from, to_)

let message_body_field field =  Pt.Message_field field  

let message_body_map_field field = Pt.Message_map_field field

let message_body_oneof_field field =  Pt.Message_oneof_field   field  

let message_body_sub message  =  Pt.Message_sub message 

let message_body_enum enum = Pt.Message_enum enum

let message_body_extension extension_ranges = 
  Pt.Message_extension extension_ranges 

let message_body_reserved extension_ranges = 
  Pt.Message_extension extension_ranges 

let message_body_option option_ = Pt.Message_option option_ 

let message ~content message_name = 
  incr message_counter;
  Pt.({
    id = !message_counter;
    message_name;
    message_body = content;
  }) 

let import ?public file_name = {
  Pt.public = (match public with | Some _ -> true | None -> false); 
  Pt.file_name; 
}

let extend extend_name extend_body = 
  incr message_counter; 
  Pt.({
    id = !message_counter; 
    extend_name; 
    extend_body;
  }) 

let rec message_printer ?level:(level = 0) {
  Pt.message_name; 
  Pt.message_body; _ } = 

  let prefix () = 
    for _ =0 to level  - 1 do 
      Printf.printf " ";
    done; 
  in 
  prefix (); print_endline message_name; 
  List.iter (function 
    | Pt.Message_field {Pt.field_name; _ } -> 
        prefix (); Printf.printf "- field [%s]\n" field_name
    | Pt.Message_map_field {Pt.map_name; _ } -> 
        prefix (); Printf.printf "- map [%s]\n" map_name
    | Pt.Message_oneof_field {Pt.oneof_name ; _ } ->
        prefix (); Printf.printf "- one of field [%s]\n" oneof_name
    | Pt.Message_enum {Pt.enum_name; _ } ->
        prefix (); Printf.printf "- enum type [%s]\n" enum_name 
    | Pt.Message_sub m -> message_printer ~level:(level + 2) m
    | Pt.Message_extension _ -> () 
    | Pt.Message_reserved _ -> () 
    | Pt.Message_option _ -> ()
  ) message_body 

let proto ?syntax ?file_option ?package ?import ?message ?enum ?proto ?extend () = 

  let proto = match proto with 
    | None -> Pt.({
      proto_file_name = None;
      syntax;
      imports = [];
      package = None; 
      messages = []; 
      file_options = []; 
      enums = []; 
      extends = []; 
    }) 
    | Some proto -> proto
  in 
  
  let {
    Pt.messages; 
    imports; 
    file_options; 
    enums; 
    extends; _ } = proto in 
  
  let proto = match syntax with 
    | None   -> proto
    | Some _ -> Pt.({proto with syntax; })  
  in 

  let proto = match package with 
    | None   -> proto
    | Some _ -> Pt.({proto with package; })  
  in 

  let proto = match message with 
    | None   -> proto 
    | Some m -> Pt.({proto with messages = m :: messages})
  in 
  
  let proto = match enum with 
    | None   -> proto 
    | Some m -> Pt.({proto with enums = m :: enums})
  in 
  
  let proto = match import with 
    | None   -> proto 
    | Some i -> Pt.({proto with imports = i :: imports})
  in 

  let proto = match file_option with 
    | None   -> proto 
    | Some i -> Pt.({proto with file_options = i :: file_options})
  in 
  
  let proto = match extend with 
    | None   -> proto 
    | Some e -> Pt.({proto with extends = e :: extends })
  in 
  proto 
   
let file_option (file_options:Pt.file_option list) (name:string) =
  match List.assoc name file_options with
  | x -> Some x 
  | exception Not_found -> None  

let verify_syntax2 proto = 
  let {Pt.messages; _} = proto in 

  (* make sure there are no `Nolabel` fields in messages *)

  let rec verify_message m  = 
    let {Pt.message_name; message_body; _} = m in 

    List.iter (function
      | Pt.Message_field field -> 
        let {Pt.field_label; field_name; _} = field in
        begin match field_label with 
        | `Nolabel -> E.missing_field_label ~field_name ~message_name 
        | _ -> () 
        end 
      | Pt.Message_sub m' -> verify_message m'
      | _ -> ()
    ) message_body 

  in 
  List.iter verify_message messages; 
  ()

let verify_syntax3 proto = 
  let {Pt.messages; enums; _ } = proto in
  
  (* make sure there are no `Required` or `Optional`` fields 
   * in messages *)

  let verify_no_default_field_options field_name message_name field_options = 
    match List.assoc "default" field_options with
    | exception Not_found -> () 
    | _ -> E.default_field_option_not_supported ~field_name ~message_name 
  in

  let verify_enum ?message_name {Pt.enum_name; enum_body; _} = 
    let rec aux = function
      | (Pt.Enum_option _) :: tl -> aux tl 
      | (Pt.Enum_value {Pt.enum_value_int; _}) :: _ ->  
        if enum_value_int != 0 
        then E.invalid_first_enum_value_proto3 ?message_name ~enum_name () 
        else ()
      | [] -> assert(false)
    in
    aux enum_body
  in

  let rec verify_message m  = 
    let {Pt.message_name; message_body; _} = m in 

    List.iter (function
      | Pt.Message_field field -> 
        let {Pt.field_label; field_name; field_options; _} = field in
        begin match field_label with 
        | `Required 
        | `Optional -> E.invalid_proto3_field_label 
            ~field_name ~message_name 
        | _ -> () 
        end;
        verify_no_default_field_options field_name message_name field_options 
      | Pt.Message_sub m' -> verify_message m'
      | Pt.Message_enum enum -> verify_enum ~message_name enum 
      | _ -> ()
    ) message_body 
  in 
  List.iter verify_message messages;
  List.iter verify_enum enums;
  ()

let verify_syntax_invariants ({Pt.syntax; _} as proto) = 
  match syntax with
  | None 
  | Some "proto2" -> verify_syntax2 proto
  | Some "proto3" -> verify_syntax3 proto 
  | Some s -> E.invalid_protobuf_syntax s 
