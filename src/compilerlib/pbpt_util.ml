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

let field ?options:(options =[]) ~label ~number ~type_ name = {
  Pbpt.field_name = name; 
  Pbpt.field_number = number;
  Pbpt.field_type = type_; 
  Pbpt.field_label = label;
  Pbpt.field_options = options;
} 

let map ~number ~key_type ~value_type name = {
  Pbpt.map_name = name;
  Pbpt.map_number = number;
  Pbpt.map_key_type = key_type;
  Pbpt.map_value_type = value_type;
}

let oneof_field ?options:(options =[]) ~number ~type_ name = {
  Pbpt.field_name = name; 
  Pbpt.field_number = number;
  Pbpt.field_type = type_; 
  Pbpt.field_options = options;
  Pbpt.field_label = `Oneof
} 

let oneof ~fields name = {
  Pbpt.oneof_name = name;
  Pbpt.oneof_fields = fields;
}

let message_counter = ref 0

let enum_value ~int_value name = { 
  Pbpt.enum_value_name = name; 
  Pbpt.enum_value_int = int_value; 
}

let enum ?enum_values:(enum_values = []) enum_name =
  incr message_counter; 
  {
    Pbpt.enum_id  = !message_counter; 
    Pbpt.enum_name; 
    Pbpt.enum_values; 
  } 

let extension_range_single_number number = 
  Pbpt.Extension_single_number number 

let extension_range_range from to_ = 
  let to_ = match to_ with
    | `Max      -> Pbpt.To_max 
    | `Number i -> Pbpt.To_number i  
  in 
  Pbpt.Extension_range (from, to_)

let message_body_field field =  Pbpt.Message_field field  
let message_body_map_field field = Pbpt.Message_map_field field
let message_body_oneof_field field =  Pbpt.Message_oneof_field   field  
let message_body_sub message  =  Pbpt.Message_sub message 
let message_body_enum enum = Pbpt.Message_enum enum
let message_body_extension extension_ranges = Pbpt.Message_extension extension_ranges 

let message ~content message_name = 
  incr message_counter;
  Pbpt.({
    id = !message_counter;
    message_name;
    message_body = content;
  }) 

let import ?public file_name = {
  Pbpt.public = (match public with | Some _ -> true | None -> false); 
  Pbpt.file_name; 
}

let extend extend_name extend_body = 
  incr message_counter; 
  Pbpt.({
    id = !message_counter; 
    extend_name; 
    extend_body;
  }) 

let rec message_printer ?level:(level = 0) {
  Pbpt.message_name; 
  Pbpt.message_body; _ } = 

  let prefix () = 
    for _ =0 to level  - 1 do 
      Printf.printf " ";
    done; 
  in 
  prefix (); print_endline message_name; 
  List.iter (function 
    | Pbpt.Message_field {Pbpt.field_name; _ } -> 
        prefix (); Printf.printf "- field [%s]\n" field_name
    | Pbpt.Message_oneof_field {Pbpt.oneof_name ; _ } ->
        prefix (); Printf.printf "- one of field [%s]\n" oneof_name
    | Pbpt.Message_enum {Pbpt.enum_name; _ } ->
        prefix (); Printf.printf "- enum type [%s]\n" enum_name 
    | Pbpt.Message_sub m -> message_printer ~level:(level + 2) m
    | Pbpt.Message_extension _ -> () 
  ) message_body 

let proto ?syntax ?file_option ?package ?import ?message ?enum ?proto ?extend () = 

  let {Pbpt.messages; imports; file_options; enums; extends; _ } as proto = match proto with 
    | None -> Pbpt.({
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
  
  let proto = match syntax with 
    | None   -> proto
    | Some _ -> Pbpt.({proto with syntax; })  
  in 

  let proto = match package with 
    | None   -> proto
    | Some _ -> Pbpt.({proto with package; })  
  in 

  let proto = match message with 
    | None   -> proto 
    | Some m -> Pbpt.({proto with messages = m :: messages})
  in 
  
  let proto = match enum with 
    | None   -> proto 
    | Some m -> Pbpt.({proto with enums = m :: enums})
  in 
  
  let proto = match import with 
    | None   -> proto 
    | Some i -> Pbpt.({proto with imports = i :: imports})
  in 

  let proto = match file_option with 
    | None   -> proto 
    | Some i -> Pbpt.({proto with file_options = i :: file_options})
  in 
  
  let proto = match extend with 
    | None   -> proto 
    | Some e -> Pbpt.({proto with extends = e :: extends })
  in 
  proto 
   
let file_option (file_options:Pbpt.file_option list) (name:string) =
  match List.assoc name file_options with
  | x -> Some x 
  | exception Not_found -> None  
