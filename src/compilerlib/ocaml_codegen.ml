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

module P   = Printf
module E   = Exception 
module L   = Logger 
module T   = Ocaml_types
module Enc = Encoding_util
module F   = Fmt 

let constructor_name s =
  String.capitalize @@ String.lowercase s 

(** utility function used to generate decode/encode function names 
      which are implemented in [Backend_ocaml_static].
 *)
let fname_of_payload_kind = function 
  | Enc.Varint zigzag -> if zigzag then "varint_zigzag" else "varint"
  | Enc.Bits32        -> "bits32"
  | Enc.Bits64        -> "bits64"
  | Enc.Bytes         -> "bytes"

let string_of_field_type ?type_qualifier:(type_qualifier = T.No_qualifier) field_type = 
  let s = match field_type with 
    | T.Unit -> "unit"
    | T.String -> "string"
    | T.Float  -> "float"
    | T.Int    -> "int"
    | T.Int32  -> "int32"
    | T.Int64  -> "int64"
    | T.Bytes  -> "bytes"
    | T.Bool   -> "bool"
    | T.User_defined_type {T.module_ = None; T.type_name} -> type_name
    | T.User_defined_type {T.module_ = Some module_; T.type_name} -> module_ ^ "." ^ type_name
  in
  match type_qualifier with 
  | T.No_qualifier -> s 
  | T.Option       -> s ^ " option"
  | T.List         -> s ^ " list"

let caml_file_name_of_proto_file_name proto = 
  let splitted = Util.rev_split_by_char '.' proto in 
  if List.length splitted < 2 || 
     List.hd splitted <> "proto" 
  then failwith "Proto file has no valid extension"
  else 
    String.concat "_" @@ List.rev @@ ("pb" :: (List.tl splitted)) 

let sp x =  P.sprintf x 
(** [sp x] same as sprintf but prefixed with new line *)

let type_decl_of_and = function | Some _ -> "and" | None -> "type" 

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let gen_type_record ?mutable_ ?and_ {T.record_name; fields } = 
  let field_prefix = match mutable_ with
    | None -> ""
    | Some () -> "mutable "
  in
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s %s = {" (type_decl_of_and and_) record_name;
  F.scope sc (fun sc -> 
    List.iter (fun {T.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type ~type_qualifier field_type in 
      F.line sc @@ sp "%s%s : %s;" field_prefix field_name type_name
    ) fields;
  ); 
  F.line sc "}";
  F.print sc 

let gen_type_variant ?and_ variant =  
  let {T.variant_name; variant_constructors; variant_encoding = _ } = variant in
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) variant_name; 
  F.scope sc (fun sc -> 
    List.iter (fun {T.field_name; field_type; type_qualifier; _ } -> 
      match field_type with
      | T.Unit -> F.line sc @@ sp "| %s" field_name 
      | _ -> (
        let type_name = string_of_field_type ~type_qualifier field_type in 
         F.line sc @@ sp "| %s of %s" field_name type_name
      )
    ) variant_constructors;
  ); 
  F.print sc 

let gen_type_const_variant ?and_ {T.cvariant_name; cvariant_constructors } = 
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s " name
    ) cvariant_constructors;
  ); 
  F.print sc 

let gen_type ?and_ = function 
  | {T.spec = T.Record r; _ } -> gen_type_record ~mutable_:() ?and_ r 
  | {T.spec = T.Variant v; _ } -> gen_type_variant  ?and_ v 
  | {T.spec = T.Const_variant v; _ } -> gen_type_const_variant ?and_ v 
  
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)
let function_name_of_user_defined prefix = function 
  | {T.module_ = Some module_; T.type_name} -> 
    sp "%s.%s_%s" module_ prefix type_name
  | {T.module_ = None; T.type_name} -> 
    sp "%s_%s" prefix type_name 

let gen_decode_record ?and_ {T.record_name; fields} = 

  let decode_field_f field_type payload_kind nested = 
    match field_type with 
    | T.User_defined_type t -> 
        let f_name = function_name_of_user_defined "decode" t in
        if nested 
        then (f_name ^ " (Pbrt.Decoder.nested d)")
        else (f_name ^ " d") 
    | T.Unit -> 
        "Pbrt.Decoder.empty_nested d"
    | _ -> (Backend_ocaml_static.runtime_function (`Decode, payload_kind, field_type) ^ " d") 
  in

  (* list fields have a special treatement when decoding since each new element
     of a repeated field is appended to the front of the list. In order
     to retreive the right order efficiently we reverse all the repeated field
     lists values when the message is done being decoded. 
   *) 
  let all_lists = List.fold_left (fun acc {T.field_name; T.type_qualifier; _ } -> 
    match type_qualifier with
    | T.List -> field_name :: acc 
    | _ -> acc  
  ) [] fields in  

  (* let all_lists = [] in
   *)

  let process_regular_field sc field field_encoding =   
    let {T.encoding_type; T.field_type; T.field_name; T.type_qualifier;} = field in 
    let {Enc.field_number; payload_kind; nested; packed} = field_encoding in 
    let f = decode_field_f field_type payload_kind nested in 
    let rhs = match type_qualifier, packed with
      | T.No_qualifier, false -> sp "(%s)" f  
      | T.Option, false -> sp "Some (%s)" f
      | T.List , false  -> sp "(%s) :: v.%s" f field_name 
      | T.List , true -> sp "(Pbrt.Decoder.packed %s)" f
      | _ , true -> E.invalid_packed_option field_name 
    in
    F.line sc @@ sp "| Some (%i, Pbrt.%s) -> v.%s <- %s; loop ()"
      field_number (Enc.string_of_payload_kind ~capitalize:() payload_kind packed) field_name rhs
  in 

  let process_one_of sc field variant = 
    let {T.encoding_type; T.field_type; T.field_name; T.type_qualifier;} = field in 
    let {T.variant_name;variant_constructors;variant_encoding = _ } = variant in 
    List.iter (fun field ->
      let {
        T.encoding_type = {Enc.field_number; payload_kind; nested; packed} ;
        T.field_type; 
        T.field_name = constructor_name; 
        T.type_qualifier = _ ;
      } = field in 
      let f = decode_field_f field_type payload_kind nested in 
      let payload_kind = Enc.string_of_payload_kind ~capitalize:() payload_kind packed in 
      match field_type with
      | T.Unit -> (
        F.line sc @@ sp "| Some (%i, Pbrt.%s) -> v.%s <- %s; %s ; loop ()"
          field_number 
          payload_kind 
          field_name 
          constructor_name 
          f  
      )
      | _ -> (
        F.line sc @@ sp "| Some (%i, Pbrt.%s) -> v.%s <- %s (%s) ; loop ()"
          field_number 
          payload_kind 
          field_name 
          constructor_name 
          f  
      )
    ) variant_constructors;  
    ()
  in 

  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s decode_%s d =" (let_decl_of_and and_) record_name; 
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" record_name; 
    F.line sc "let rec loop () = "; 
    F.scope sc (fun sc -> 
      F.line sc "match Pbrt.Decoder.key d with";
      F.line sc "| None -> (";
      F.scope sc (fun sc -> 
        List.iter (fun field_name -> 
          F.line sc @@ sp "v.%s <- List.rev v.%s;" field_name field_name
        ) all_lists;   
      );
      F.line sc ")";
      List.iter (fun field -> 
        let {T.encoding_type; _ } = field in 
        match encoding_type with 
        | T.Regular_field field_encoding -> 
            process_regular_field sc field field_encoding 
        | T.One_of ({T.variant_encoding = T.Inlined_within_message; _} as variant)  -> 
            process_one_of sc field variant 
        | T.One_of {T.variant_encoding = T.Standalone; _ } -> 
          E.programmatic_error E.One_of_should_be_inlined_in_message
      ) fields; 
      F.line sc "| Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()";
    ); 
    F.line sc "in"; 
    F.line sc "loop ();";
    F.line sc "v"; 
  );
  F.print sc 

let gen_decode_variant ?and_ {T.variant_name; variant_constructors; variant_encoding = _ } = 

  let process_ctor sc ctor = 
    let {T.encoding_type; field_type; field_name; type_qualifier = _  } = ctor in  
    let {Enc.field_number; Enc.nested; Enc.payload_kind; _  } = encoding_type in 
    match field_type with 
    | T.User_defined_type t -> 
      let f_name = function_name_of_user_defined "decode" t in
      let decoding = 
        if nested 
        then (f_name ^ " (Pbrt.Decoder.nested d)")
        else (f_name ^ " d") in
      F.line sc @@ sp "| Some (%i, _) -> %s (%s)" field_number field_name decoding 
    | T.Unit -> 
      F.line sc @@ sp "| Some (%i, _) -> (Pbrt.Decoder.empty_nested d ; %s)" field_number field_name 
    | _ -> 
      let decoding = Backend_ocaml_static.runtime_function (`Decode, payload_kind, field_type) ^ " d" in
      F.line sc @@ sp "| Some (%i, _) -> %s (%s)" field_number field_name decoding
  in 

  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) variant_name;
  F.scope sc (fun sc ->
    F.line sc @@ sp "let rec loop () = "; 
    F.scope sc (fun sc ->
      F.line sc "match Pbrt.Decoder.key d with";
      F.line sc "| None -> failwith \"None of the known key is found\"";
      List.iter (fun ctor -> process_ctor sc ctor) variant_constructors; 
      F.line sc "| Some (n, payload_kind) -> (";
      F.line sc "  Pbrt.Decoder.skip d payload_kind; ";
      F.line sc "  loop () ";
      F.line sc ") ";
    ); 
    F.line sc "in"; 
    F.line sc "loop ()";
  );
  F.print sc  

let gen_decode_const_variant ?and_ {T.cvariant_name; cvariant_constructors; } = 
  let sc = F.empty_scope () in 
  
  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun (name, value) -> 
      F.line sc @@ sp "| %i -> %s" value name
    ) cvariant_constructors; 
    F.line sc @@ sp "| _ -> failwith \"Unknown value for enum %s\"" cvariant_name; 
  ); 
  F.print sc 

let gen_decode ?and_ = function 
  | {T.spec = T.Record r; _ } -> Some (gen_decode_record ?and_ r)
  | {T.spec = T.Variant v; _ } -> (match v.T.variant_encoding with 
    | T.Standalone -> Some (gen_decode_variant ?and_ v) 
    | _          -> None 
  )
  | {T.spec = T.Const_variant v; _ } -> Some (gen_decode_const_variant ?and_ v)

let gen_decode_sig t = 

  let f type_name = 
    let sc = F.empty_scope () in 
    F.line sc @@ sp "val decode_%s : Pbrt.Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)" type_name type_name; 
    F.print sc 
  in 

  match t with 
  | {T.spec = T.Record {T.record_name ; _ } } ->  Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name)
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> Some (f cvariant_name)

let gen_encode_field sc v_name encoding_type field_type = 
  let {
    Enc.field_number; 
    Enc.payload_kind; 
    Enc.nested; 
    Enc.packed} = encoding_type in 
  F.line sc @@ sp "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
      field_number (constructor_name @@ Enc.string_of_payload_kind payload_kind packed);
  match field_type, packed with 
  | T.User_defined_type t, false -> 
    let f_name = function_name_of_user_defined "encode" t in 
    if nested 
    then F.line sc @@ sp "Pbrt.Encoder.nested (%s %s) encoder;" f_name v_name 
    else F.line sc @@ sp "%s %s encoder;" f_name v_name 
  | T.User_defined_type _, true -> 
    E.invalid_packed_option v_name 
  | T.Unit, false -> 
    F.line sc "Pbrt.Encoder.empty_nested encoder;" 
  | T.Unit , true -> 
    E.invalid_packed_option v_name
  | _, false ->  
    let rt = Backend_ocaml_static.runtime_function (`Encode, payload_kind, field_type) in 
    F.line sc @@ sp "%s %s encoder;" rt v_name
  | _, true ->  
    let rt = Backend_ocaml_static.runtime_function (`Encode, payload_kind, field_type) in 
    F.line sc @@ sp "Pbrt.Encoder.packed %s %s encoder;" v_name rt 

let gen_encode_record ?and_ {T.record_name; fields } = 
  L.log "gen_encode_record record_name: %s\n" record_name; 

  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s encode_%s v encoder = " (let_decl_of_and and_) record_name;
  F.scope sc (fun sc -> 
    List.iter (fun field -> 
      let {T.encoding_type; field_type; field_name; type_qualifier ; } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        let {Enc.packed; _ } = encoding_type in
        match type_qualifier, packed with 
        | T.No_qualifier, false -> (
          let v_name = sp "v.%s" field_name in 
          gen_encode_field sc v_name encoding_type field_type
        )
        | T.Option, false -> (
          F.line sc @@ sp "(match v.%s with " field_name;
          F.line sc @@ sp "| Some x -> (";
          F.scope sc (fun sc ->
            gen_encode_field sc "x" encoding_type field_type;
          ); 
          F.line sc ")";
          F.line sc "| None -> ());";
        )
        | T.List, false -> (
          F.line sc "List.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field sc "x" encoding_type field_type;
          );
          F.line sc @@ sp ") v.%s;" field_name; 
        )
        | T.List, true -> (
          let v_name = sp "v.%s" field_name in 
          gen_encode_field sc v_name encoding_type field_type
        )
        | _ , true -> 
          E.invalid_packed_option field_name 
      )
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (  
        F.line sc @@ sp "(match v.%s with" field_name;
        List.iter (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
          (match field_type with 
          | T.Unit -> F.line sc @@ sp "| %s -> (" field_name
          | _ -> F.line sc @@ sp "| %s x -> (" field_name
          );
          F.scope sc (fun sc -> 
            gen_encode_field sc "x" encoding_type field_type
          ); 
          F.line sc ")";
        ) variant_constructors;
        F.line sc ");";
      ) (* one of        *)
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Standalone} -> (  
        E.programmatic_error E.One_of_should_be_inlined_in_message
      )
    ) fields; 
    F.line sc "()";
  ); (* encode function *)
  F.print sc ;; 

let gen_encode_variant ?and_ variant = 
  let {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = variant in  
  let sc = F.empty_scope () in
  F.line sc @@ sp "%s encode_%s v encoder = " (let_decl_of_and and_) variant_name;
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
      (match field_type with 
      | T.Unit -> F.line sc @@ sp "| %s -> (" field_name
      | _ -> F.line sc @@ sp "| %s x -> (" field_name
      );
      F.scope sc (fun sc -> 
        gen_encode_field sc "x" encoding_type field_type
      ); 
      F.line sc ")";
    ) variant_constructors;
  ); 
  F.print sc

let gen_encode_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s encode_%s v encoder =" (let_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, value) -> 
      F.line sc (
        if value > 0 
        then sp "| %s -> Pbrt.Encoder.int_as_varint %i encoder" name value
        else sp "| %s -> Pbrt.Encoder.int_as_varint (%i) encoder" name value
      )
    ) cvariant_constructors; 
  );
  F.print sc

let gen_encode ?and_ = function 
  | {T.spec = T.Record r } -> Some (gen_encode_record  ?and_ r)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with 
    | T.Standalone -> Some (gen_encode_variant ?and_ v)
    | _ -> None 
  )
  | {T.spec = T.Const_variant v } -> Some (gen_encode_const_variant ?and_ v)

let gen_encode_sig t = 
  let f type_name = 
    let sc = F.empty_scope () in 
    F.line sc @@ sp "val encode_%s : %s -> Pbrt.Encoder.t -> unit" type_name type_name;
    F.line sc @@ sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" type_name; 
    F.print sc 
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } }-> Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name) 
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> Some (f cvariant_name) 

let gen_pp_field field_type = 
  match field_type with 
  | T.User_defined_type t -> function_name_of_user_defined "pp" t 
  | _ ->  sp "pp_%s" (string_of_field_type field_type) 

let gen_pp_record  ?and_ {T.record_name; fields } = 
  L.log "gen_pp, record_name: %s\n" record_name; 

  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s pp_%s fmt v = " (let_decl_of_and and_) record_name;
  F.scope sc (fun sc ->
    F.line sc "let pp_i fmt () ="; 
    F.scope sc (fun sc -> 
      F.line sc "F.pp_open_vbox fmt 1;";
      List.iter (fun field -> 
        let {T.field_type; field_name; type_qualifier ; encoding_type} = field in 
        let x:string = sp "v.%s" field_name in 
        match encoding_type with 
        | T.Regular_field encoding_type -> ( 
          let field_string_of = gen_pp_field field_type in 
          match type_qualifier with
          | T.No_qualifier -> 
            F.line sc @@ sp "pp_equal \"%s\" %s fmt %s;" field_name field_string_of x
          | T.Option -> 
            F.line sc @@ sp "pp_equal \"%s\" (pp_option %s) fmt %s;" field_name field_string_of x
          | T.List -> 
            F.line sc @@ sp "pp_equal \"%s\" (pp_list %s) fmt %s;" field_name field_string_of x
        )
        | T.One_of {T.variant_constructors = _ ; variant_name; T.variant_encoding = T.Inlined_within_message} -> (
          F.line sc @@ sp "pp_equal \"%s\" %s fmt %s;" field_name ("pp_" ^ variant_name) x
        )                (* one of        *)
        | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone } -> (
          E.programmatic_error E.One_of_should_be_inlined_in_message
        )
      ) fields; 
      F.line sc "F.pp_close_box fmt ()";
    );
    F.line sc "in";
    F.line sc "pp_brk pp_i fmt ()";
  );
  F.print sc 

let gen_pp_variant ?and_ {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = 
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s pp_%s fmt = function " (let_decl_of_and and_) variant_name;
  F.scope sc (fun sc -> 
    List.iter (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
      let field_string_of = gen_pp_field field_type in 
      match field_type with
      | T.Unit -> 
        F.line sc @@ sp "| %s  -> F.fprintf fmt \"%s\"" field_name field_name 
      | _ -> 
        F.line sc @@ sp  "| %s x -> F.fprintf fmt \"@[%s(%%a)@]\" %s x" field_name field_name field_string_of 
    ) variant_constructors ;
  );
  F.print sc 

let gen_pp_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s pp_%s fmt = function" (let_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s -> F.fprintf fmt \"%s\"" name name
    ) cvariant_constructors; 
  ); 
  F.print sc 

let gen_pp ?and_ = function 
  | {T.spec = T.Record r  } -> Some (gen_pp_record ?and_ r) 
  | {T.spec = T.Variant v } -> Some (gen_pp_variant ?and_ v) 
  | {T.spec = T.Const_variant v } -> Some (gen_pp_const_variant ?and_ v)

let gen_pp_sig t = 
  let f type_name =  
    let sc = F.empty_scope () in 
    F.line sc @@ sp "val pp_%s : Format.formatter -> %s -> unit " type_name type_name;
    F.line sc @@ sp "(** [pp_%s v] formats v] *)" type_name;
    F.print sc 
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } } -> Some (f record_name) 
  | {T.spec = T.Variant v } -> Some (f v.T.variant_name) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ ; } } -> Some (f cvariant_name) 

let gen_default_field field_name field_type field_default = 
  match field_type, field_default with 
  | T.User_defined_type t, _ -> 
    function_name_of_user_defined "default" t  ^ " ()"
  | T.Unit  , None -> "()"
  | T.String, None -> "\"\""
  | T.String, Some (Pbpt.Constant_string s) -> sp "\"%s\"" s 
  | T.Float , None -> "0." 
  | T.Float , Some (Pbpt.Constant_float f) -> string_of_float f
  | T.Int   , None -> "0"
  | T.Int   , Some (Pbpt.Constant_int i) -> string_of_int i
  | T.Int32 , None -> "0l"
  | T.Int32 , Some (Pbpt.Constant_int i) -> sp "%il" i
  | T.Int64 , None -> "0L"
  | T.Int64 , Some (Pbpt.Constant_int i) -> sp "%iL" i
  | T.Bytes , None -> "Bytes.create 64"  
  | T.Bytes , Some (Pbpt.Constant_string s) -> sp "Bytes.of_string \"%s\"" s  
  | T.Bool  , None -> "false"
  | T.Bool  , Some (Pbpt.Constant_bool b) -> string_of_bool b
  | _ -> E.invalid_default_value 
    ~field_name ~info:"invalid default type" ()

let gen_default_record  ?and_ {T.record_name; fields } = 
  L.log "gen_pp, record_name: %s\n" record_name; 

  let sc = F.empty_scope () in 
  F.line sc @@ sp "%s default_%s () = {" (let_decl_of_and and_) record_name;
  F.scope sc (fun sc -> 
    List.iter (fun field -> 
      L.log "gen_pp field_name: %s\n" field.T.field_name;
     
      let { T.field_type; field_name; type_qualifier ; encoding_type } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        match type_qualifier with
        | T.No_qualifier -> 
          let field_default_of = gen_default_field field_name field_type encoding_type.Enc.default in 
          F.line sc @@ sp "%s = %s;" field_name field_default_of; 
        | T.Option -> (
          match encoding_type.Enc.default with
          | None -> F.line sc @@ sp "%s = None;" field_name
          | Some _ -> 
            let field_default_of = gen_default_field field_name field_type encoding_type.Enc.default in 
            F.line sc @@ sp "%s = Some (%s);" field_name field_default_of; 
        )  
        | T.List -> 
          F.line sc @@ sp "%s = [];" field_name; 
      )
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (
        match variant_constructors with
        | []     -> failwith "programmatic TODO error" 
        | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
          match field_type with 
          | T.Unit -> F.line sc @@ sp "%s = %s;" field_name constructor_name 
          | _ -> (
            F.line sc @@ sp "%s = %s (%s);" 
              field_name constructor_name (gen_default_field field_name field_type encoding_type.Encoding_util.default)  
          )
      ) (* one of        *)
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone} -> (
        E.programmatic_error E.One_of_should_be_inlined_in_message
      )
    ) fields;          (* record fields *) 
  ); (* end of function *)
  F.line sc "}";
  F.print sc 

let gen_default_variant ?and_ {T.variant_name; T.variant_constructors ; T.variant_encoding = _ } = 
  match variant_constructors with
  | []     -> failwith "programmatic TODO error" 
  | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
    let decl = let_decl_of_and and_ in 
    match field_type with
    | T.Unit -> 
      sp "%s default_%s () = %s" decl variant_name constructor_name 
    |  _ -> 
      let default_field = gen_default_field variant_name field_type encoding_type.Encoding_util.default in
      sp "%s default_%s () = %s (%s)" 
         decl variant_name constructor_name default_field 

let gen_default_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  let first_constructor_name = match cvariant_constructors with
    | []            -> failwith "programmatic TODO error"
    | (name, _) ::_ -> name 
  in  
  sp "%s default_%s () = %s" (let_decl_of_and and_) cvariant_name first_constructor_name

let gen_default ?and_ = function 
  | {T.spec = T.Record r  } -> Some (gen_default_record ?and_ r) 
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (gen_default_variant ?and_ v)
    | T.Inlined_within_message -> None
  )
  | {T.spec = T.Const_variant v } -> Some (gen_default_const_variant v)

let gen_default_sig t = 
  let f type_name =  
    let sc = F.empty_scope () in 
    F.line sc @@ sp "val default_%s : unit -> %s" type_name type_name;
    F.line sc @@ sp "(** [default_%s ()] is the default value for type [%s] *)" type_name type_name;
    F.print sc
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } }-> Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name)
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name ; _ ; } } -> Some (f cvariant_name) 
