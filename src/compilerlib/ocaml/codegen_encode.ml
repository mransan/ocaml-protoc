
module T = Ocaml_types
module F = Fmt 
module Enc = Encoding_util
module E = Exception
module L = Logger

open Codegen_util

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let gen_encode_field_key sc {Enc.field_number; Enc.payload_kind; Enc.packed; _ } = 
  F.line sc @@ sp "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
      field_number (constructor_name @@ Enc.string_of_payload_kind payload_kind packed)

let gen_encode_field sc v_name encoding_type field_type = 
  let {
    Enc.field_number; 
    Enc.payload_kind; 
    Enc.nested; 
    Enc.packed} = encoding_type in 
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
  | _, _ ->  
    let rt = Backend_ocaml_static.runtime_function (`Encode, payload_kind, field_type) in 
    F.line sc @@ sp "%s %s encoder;" rt v_name

let gen_encode_record ?and_ {T.record_name; fields } sc = 
  L.log "gen_encode_record record_name: %s\n" record_name; 

  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) record_name record_name;
  F.scope sc (fun sc -> 
    List.iter (fun field -> 
      let {T.encoding_type; field_type; field_name; type_qualifier ; } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        let {Enc.packed; _ } = encoding_type in
        match type_qualifier, packed with 
        | T.No_qualifier, false -> (
          let v_name = sp "v.%s" field_name in 
          gen_encode_field_key sc encoding_type;
          gen_encode_field sc v_name encoding_type field_type
        )
        | T.Option, false -> (
          F.line sc @@ sp "(match v.%s with " field_name;
          F.line sc @@ sp "| Some x -> (";
          F.scope sc (fun sc ->
            gen_encode_field_key sc encoding_type;
            gen_encode_field sc "x" encoding_type field_type;
          ); 
          F.line sc ")";
          F.line sc "| None -> ());";
        )
        | T.Option, true 
        | T.No_qualifier, true ->
          E.invalid_packed_option field_name
        | T.List, false -> (
          F.line sc "List.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_key sc encoding_type;
            gen_encode_field sc "x" encoding_type field_type;
          );
          F.line sc @@ sp ") v.%s;" field_name; 
        )
        | T.Repeated_field, false -> (
          F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_key sc encoding_type;
            gen_encode_field sc "x" encoding_type field_type;
          );
          F.line sc @@ sp ") v.%s;" field_name; 
        )
        | T.List, true -> (
          gen_encode_field_key sc encoding_type;
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "List.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field sc "x" encoding_type field_type;
            );
            F.line sc @@ sp ") v.%s;" field_name; 
          );
          F.line sc") encoder;";
        )
        | T.Repeated_field , true -> (
          gen_encode_field_key sc encoding_type;
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field sc "x" encoding_type field_type;
            );
            F.line sc @@ sp ") v.%s;" field_name; 
          );
          F.line sc") encoder;";
        )
      )
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (  
        F.line sc @@ sp "(match v.%s with" field_name;
        List.iter (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
          (match field_type with 
          | T.Unit -> F.line sc @@ sp "| %s -> (" field_name
          | _ -> F.line sc @@ sp "| %s x -> (" field_name
          );
          F.scope sc (fun sc -> 
            gen_encode_field_key sc encoding_type;
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
  ) (* encode function *)

let gen_encode_variant ?and_ variant sc = 
  let {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = variant in  
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) variant_name variant_name;
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
      (match field_type with 
      | T.Unit -> F.line sc @@ sp "| %s -> (" field_name
      | _ -> F.line sc @@ sp "| %s x -> (" field_name
      );
      F.scope sc (fun sc -> 
        gen_encode_field_key sc encoding_type;
        gen_encode_field sc "x" encoding_type field_type
      ); 
      F.line sc ")";
    ) variant_constructors;
  ) 

let gen_encode_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } sc = 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder =" (let_decl_of_and and_) cvariant_name cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, value) -> 
      F.line sc (
        if value > 0 
        then sp "| %s -> Pbrt.Encoder.int_as_varint %i encoder" name value
        else sp "| %s -> Pbrt.Encoder.int_as_varint (%i) encoder" name value
      )
    ) cvariant_constructors; 
  )

let gen_struct ?and_ t sc  = 
  let (), has_encoded = match t with 
    | {T.spec = T.Record r } -> gen_encode_record  ?and_ r sc, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with 
      | T.Standalone -> gen_encode_variant ?and_ v sc, true
      | _ -> (), false 
    )
    | {T.spec = T.Const_variant v } ->
      gen_encode_const_variant ?and_ v sc, true
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let f type_name = 
    F.line sc @@ sp "val encode_%s : %s -> Pbrt.Encoder.t -> unit" type_name type_name;
    F.line sc @@ sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" type_name; 
  in 
  let (), has_encoded = match t with 
    | {T.spec = T.Record {T.record_name ; _ } }-> f record_name, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
      | T.Standalone -> f v.T.variant_name, true 
      | T.Inlined_within_message -> (), false
    ) 
    | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> f cvariant_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Encoding"
