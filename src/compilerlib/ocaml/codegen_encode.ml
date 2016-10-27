
module T = Ocaml_types
module F = Fmt 
module E = Exception
module L = Logger

open Codegen_util

let constructor_name s = 
  (String.capitalize @@ String.lowercase s) [@@ocaml.warning "-3"] 

let gen_encode_field_key sc number pk is_packed = 
  F.line sc @@ sp "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
    number (constructor_name @@ Codegen_util.string_of_payload_kind pk is_packed)
  
let encode_basic_type bt pk = 
  Backend_ocaml_static.runtime_function (`Encode, pk, bt) 

let gen_encode_field_type ?with_key sc var_name encoding_number pk is_packed field_type = 

  let encode_key sc = 
    match with_key with
    | Some () -> gen_encode_field_key sc encoding_number pk is_packed 
    | None -> ()
  in 

  match field_type with 
  | T.Ft_user_defined_type ud -> (
    encode_key sc;
    let f_name = function_name_of_user_defined "encode" ud in 
    if ud.T.udt_nested 
    then F.line sc @@ sp "Pbrt.Encoder.nested (%s %s) encoder;" f_name var_name
    else F.line sc @@ sp "%s %s encoder;" f_name var_name
  )
  | T.Ft_unit -> (
    encode_key sc;
    F.line sc "Pbrt.Encoder.empty_nested encoder;" 
  )
  | T.Ft_basic_type bt -> ( 
    encode_key sc;
    let rt = encode_basic_type bt pk in 
    F.line sc @@ sp "%s %s encoder;" rt var_name
  )

let gen_encode_record ?and_ {T.r_name; r_fields } sc = 
  L.log "gen_encode_record record_name: %s\n" r_name;

  let rn = r_name in 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) rn rn;
  F.scope sc (fun sc -> 
    List.iter (fun record_field -> 
      let {T.rf_label; rf_field_type; _ } = record_field in  

      match rf_field_type with 
      | T.Rft_required (field_type, encoding_number, pk, _) -> ( 
        let var_name = sp "v.%s" rf_label in 
        gen_encode_field_type ~with_key:() sc var_name encoding_number pk false (* packed *) field_type
      )
      | T.Rft_optional (field_type, encoding_number, pk, _) -> (
        F.line sc "(";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "match v.%s with " rf_label;
          F.line sc @@ sp "| Some x -> (";
          F.scope sc (fun sc ->
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk false field_type;
          ); 
          F.line sc ")";
          F.line sc "| None -> ();";
        );
        F.line sc ");";
      )
      | T.Rft_repeated_field (rt, field_type, encoding_number, pk, is_packed) -> (
        match rt, is_packed with
        | T.Rt_list, false -> (
          F.line sc "List.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk is_packed field_type;
          );
          F.line sc @@ sp ") v.%s;" rf_label; 
        ) 
        | T.Rt_repeated_field, false -> ( 
          F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk is_packed field_type;
          );
          F.line sc @@ sp ") v.%s;" rf_label; 
        ) 
        | T.Rt_list, true -> (
          gen_encode_field_key sc encoding_number pk is_packed;
            (* When packed the key is encoded once. 
             *)
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "List.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
            );
            F.line sc @@ sp ") v.%s;" rf_label; 
          );
          F.line sc ") encoder;";
        ) 
        | T.Rt_repeated_field, true -> (
          gen_encode_field_key sc encoding_number pk is_packed;
            (* When packed the key is encoded once. 
             *)
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
            );
            F.line sc @@ sp ") v.%s;" rf_label; 
          );
          F.line sc") encoder;";
        ) 
      ) 
      | T.Rft_variant_field {T.v_constructors; _} -> (
        F.line sc  "(";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "match v.%s with" rf_label;
          List.iter (fun {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind} -> 
            begin match vc_field_type with
            | T.Vct_nullary -> ( 
              F.line sc @@ sp "| %s -> (" vc_constructor; 
              F.scope sc (fun sc -> 
                gen_encode_field_key sc vc_encoding_number vc_payload_kind false;
                F.line sc "Pbrt.Encoder.empty_nested encoder";
              );
              F.line sc ")";
            )
            | T.Vct_non_nullary_constructor field_type -> (
              F.line sc @@ sp "| %s x -> (" vc_constructor;
              F.scope sc (fun sc -> 
                gen_encode_field_type sc ~with_key:() "x" vc_encoding_number vc_payload_kind false field_type  
              ); 
              F.line sc  ")";
            )
            end;
          ) v_constructors;
        );
        F.line sc ");"
      ) 
      | T.Rft_associative_field (at, encoding_number, (key_type, key_pk), (value_type, value_pk)) -> (
        F.line sc @@ sp "let encode_key = %s in" (encode_basic_type key_type key_pk);
        F.line sc "let encode_value = (fun x encoder ->";
        F.scope sc (fun sc -> 
          gen_encode_field_type sc "x" (-1 (* TODO *)) value_pk false value_type;
        );
        F.line sc ") in";

        begin match at with
        | T.At_list -> (
           F.line sc "List.iter (fun (k, v) ->"; 
        )
        | T.At_hashtable -> (
          F.line sc "Hashtbl.iter (fun k v ->";
        )
        end; 
        F.scope sc (fun sc ->
          gen_encode_field_key sc encoding_number T.Pk_bytes false;
          F.line sc @@ sp "let map_entry = (k, Pbrt.%s), (v, Pbrt.%s) in"
            (Codegen_util.string_of_payload_kind ~capitalize:() key_pk false) 
            (Codegen_util.string_of_payload_kind ~capitalize:() value_pk false); 
          F.line sc "Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder"
        );
        F.line sc @@ sp ") v.%s;" rf_label;
      ) 
      
    ) r_fields (* List.iter *); 
    F.line sc "()";
  ) (* encode function *)

let gen_encode_variant ?and_ variant sc = 
  let {T.v_name; T.v_constructors} = variant in 
  let vn = v_name in  
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) vn vn;
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind} -> 
      begin match vc_field_type with
      | T.Vct_nullary -> ( 
        F.line sc @@ sp "| %s -> (" vc_constructor; 
        F.scope sc (fun sc -> 
          gen_encode_field_key sc vc_encoding_number vc_payload_kind false;
          F.line sc "Pbrt.Encoder.empty_nested encoder";
        );
        F.line sc ")";
      )
      | T.Vct_non_nullary_constructor field_type -> (
        F.line sc @@ sp "| %s x -> (" vc_constructor;
        F.scope sc (fun sc -> 
          gen_encode_field_type sc ~with_key:() "x" vc_encoding_number vc_payload_kind false field_type  
        ); 
        F.line sc  ")";
      )
      end;
    ) v_constructors;
  ) 

let gen_encode_const_variant ?and_ {T.cv_name; T.cv_constructors; } sc = 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder =" (let_decl_of_and and_) cv_name cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, value) -> 
      F.line sc (
        if value > 0 
        then sp "| %s -> Pbrt.Encoder.int_as_varint %i encoder" name value
        else sp "| %s -> Pbrt.Encoder.int_as_varint (%i) encoder" name value
      )
    ) cv_constructors; 
  )

let gen_struct ?and_ t sc  = 
  let (), has_encoded = 
    match t with 
    | {T.spec = T.Record r; _ } -> gen_encode_record  ?and_ r sc, true
    | {T.spec = T.Variant v; _ } -> gen_encode_variant ?and_ v sc, true 
    | {T.spec = T.Const_variant v; _ } ->
      gen_encode_const_variant ?and_ v sc, true
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let _ = and_ in
  let f type_name = 
    F.line sc @@ sp "val encode_%s : %s -> Pbrt.Encoder.t -> unit" type_name type_name;
    F.line sc @@ sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" type_name; 
  in 
  let (), has_encoded = 
    match t with 
    | {T.spec = T.Record {T.r_name; _ }; _}-> f r_name, true
    | {T.spec = T.Variant v; _ } -> f v.T.v_name, true 
    | {T.spec = T.Const_variant {T.cv_name; _ }; _ } -> f cv_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Toding"
