
module T   = Ocaml_types 
module Enc = Encoding_util
module E   = Exception 
module F   = Fmt

open Codegen_util 

let gen_decode_record ?and_ {T.record_name; fields} sc = 

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
    let has_assignment, rhs = match type_qualifier, packed with
      | T.No_qualifier, false -> true, sp "(%s)" f  
      | T.Option, false -> true, sp "Some (%s)" f
      | T.List , false  -> true, sp "(%s) :: v.%s" f field_name 
      | T.List , true -> true, sp "(Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d)" f
      | T.Repeated_field, false -> false, sp "Pbrt.Repeated_field.add (%s) v.%s " f field_name 
      | T.Repeated_field, true  -> false, sp "(Pbrt.Decoder.packed_fold (fun () d -> Pbrt.Repeated_field.add (%s) v.%s) () d)" f field_name
      | _ , true -> E.invalid_packed_option field_name 
    in
    if has_assignment
    then 
      F.line sc @@ sp "| Some (%i, Pbrt.%s) -> v.%s <- %s; loop ()"
        field_number (Enc.string_of_payload_kind ~capitalize:() payload_kind packed) field_name rhs
    else 
      F.line sc @@ sp "| Some (%i, Pbrt.%s) -> %s; loop ()"
        field_number (Enc.string_of_payload_kind ~capitalize:() payload_kind packed) rhs
      
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

  let mutable_record_name = Codegen_util.mutable_record_name record_name in 

  F.line sc @@ sp "%s decode_%s d =" (let_decl_of_and and_) record_name; 
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" mutable_record_name;
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
    F.line sc @@ sp "let v:%s = Obj.magic v in" record_name; 
    F.line sc "v";
  )

let gen_decode_variant ?and_ {T.variant_name; variant_constructors; variant_encoding = _ } sc = 

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

  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) variant_name;
  F.scope sc (fun sc ->
    F.line sc @@ sp "let rec loop () = "; 
    F.scope sc (fun sc ->
      F.line sc @@ sp "let ret:%s = match Pbrt.Decoder.key d with" variant_name;
      F.scope sc (fun sc -> 
        F.line sc "| None -> failwith \"None of the known key is found\"";
        List.iter (fun ctor -> process_ctor sc ctor) variant_constructors; 
        F.line sc "| Some (n, payload_kind) -> (";
        F.line sc "  Pbrt.Decoder.skip d payload_kind; ";
        F.line sc "  loop () ";
        F.line sc ")";
      );
      F.line sc "in";
      F.line sc "ret";
    ); 
    F.line sc "in"; 
    F.line sc "loop ()";
  )

let gen_decode_const_variant ?and_ {T.cvariant_name; cvariant_constructors; } sc = 

  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun (name, value) -> 
      F.line sc @@ sp "| %i -> (%s:%s)" value name cvariant_name
    ) cvariant_constructors; 
    F.line sc @@ sp "| _ -> failwith \"Unknown value for enum %s\"" cvariant_name; 
  )

let gen_struct ?and_ t sc = 
  let (), has_encoded =  match t with 
    | {T.spec = T.Record r; _ } -> gen_decode_record ?and_ r sc, true
    | {T.spec = T.Variant v; _ } -> (match v.T.variant_encoding with 
      | T.Standalone -> gen_decode_variant ?and_ v sc, true
      | _            -> (), false 
    )
    | {T.spec = T.Const_variant v; _ } -> gen_decode_const_variant ?and_ v sc, true
  in
  has_encoded

let gen_sig ?and_ t sc = 

  let f type_name = 
    F.line sc @@ sp "val decode_%s : Pbrt.Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)" type_name type_name; 
  in 

  let (), has_encoded = match t with 
    | {T.spec = T.Record {T.record_name ; _ } } -> f record_name, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
      | T.Standalone -> f v.T.variant_name, true
      | T.Inlined_within_message -> (), false
    ) 
    | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> f cvariant_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Decoding"
