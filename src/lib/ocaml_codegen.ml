module P   = Printf
module E   = Exception 
module L   = Logger 
module T   = Ocaml_types
module Enc = Encoding_util

let printf_string_of_field_type = function
  | T.String    -> "s" 
  | T.Float     -> "f"
  | T.Int       -> "i"
  | T.Int32     -> "ld"
  | T.Int64     -> "Ld"
  | T.Bytes     -> "s"
  | T.Bool      -> "b"
  | T.User_defined_type _ -> "s"

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let tag_name s =
  String.capitalize @@ String.lowercase (String.map (function | '.' -> '_' | c -> c ) s) 
  
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

let sp x =  P.sprintf ("\n" ^^ x)  
(** [sp x] same as sprintf but prefixed with new line *)

let nl s = "\n" ^ s  
(** [nl s] appends new line *)

let concat = Util.concat 

let add_indentation n s = 
  Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  
(** [add_indentation n s] adds a multiple of 2 spaces indentation to [s] *)

let type_decl_of_and = function | Some _ -> "and" | None -> "type" 

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let gen_type_record ?and_ {T.record_name; fields } = 
  concat [
    P.sprintf "%s %s = {" (type_decl_of_and and_) record_name;
    concat @@ List.map (fun {T.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type ~type_qualifier field_type in 
      sp "  %s : %s;" field_name type_name
    ) fields;
    "\n}"
  ]

let gen_type_variant ?and_ {T.variant_name; variant_constructors; variant_encoding = _ } = 
  concat [
    P.sprintf "%s %s =" (type_decl_of_and and_) variant_name; 
    concat @@ List.map (fun {T.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type ~type_qualifier field_type in 
      sp "  | %s of %s" field_name type_name
    ) variant_constructors;
  ]

let gen_type_const_variant ?and_ {T.cvariant_name; cvariant_constructors } = 
  concat [
    P.sprintf "%s %s =" (type_decl_of_and and_) cvariant_name; 
    concat @@ List.map (fun (name, _ ) -> 
      sp "  | %s " name
    ) cvariant_constructors;
  ]

let gen_type ?and_ = function 
  | {T.spec = T.Record r; _ } -> gen_type_record ?and_ r 
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
    P.sprintf "%s.%s_%s" module_ prefix type_name
  | {T.module_ = None; T.type_name} -> 
    P.sprintf "%s_%s" prefix type_name 

(** [gen_mappings_record r] generates a per record variable to hold the 
    mapping between a field number and the associated decoding routine. 

    Because the order of fields inside the protobuffer message is not
    guaranteed, the decoding cannot be done in one step.   
    The decoding code must therefore first collect all the record fields 
    values and then create the value of the OCaml type. 
  *)
let gen_mappings_record {T.record_name; fields} =

  let match_cases ?constructor field_number tag_name decode_statement = 
    let left, right = match constructor with
      | Some x -> P.sprintf "(%s " x, ")" 
      | None   -> "", ""
    in 
    concat [
      sp "| %i, `%s l -> `%s (%s (%s)%s::l)"
        field_number 
        tag_name
        tag_name
        left 
        decode_statement 
        right; 
      sp "| %i, `Default -> `%s (%s (%s)%s::[])"
        field_number 
        tag_name
        left
        decode_statement
        right;
    ] 
  in  

  let tag_name_of_user_defined = function  
    | {T.module_ = Some module_; T.type_name} -> 
      P.sprintf "%s_%s" module_ type_name
    | {T.module_ = None; T.type_name} -> 
      P.sprintf "%s" (tag_name type_name) 
  in 

  concat [
    P.sprintf "let %s_mappings d = function " record_name;
    concat @@ List.map (fun {T.encoding_type;field_type;_ } -> 
      match encoding_type with 
      | T.Regular_field {
        Enc.field_number; 
        Enc.payload_kind;
        Enc.nested } -> (
        let decoding = match field_type with 
          | T.User_defined_type t -> 
            let f_name = function_name_of_user_defined "decode" t in
            if nested 
            then  
              match_cases field_number (tag_name_of_user_defined t) (f_name ^ " (Pbrt.Decoder.nested d)")
            else 
              match_cases field_number (tag_name_of_user_defined t) (f_name ^ " d") 
          | _ -> 
             match_cases field_number (tag_name (string_of_field_type field_type)) 
               (Backend_ocaml_static.runtime_function (`Decode, payload_kind, field_type) ^ " d")
        in 
        P.sprintf "  %s" decoding 
      )
      | T.One_of {T.variant_name ; variant_constructors; T.variant_encoding = T.Inlined_within_message } -> (
        concat @@ List.map (fun {T.encoding_type; field_type; field_name; type_qualifier = _ } -> 
          let {
            Enc.field_number; 
            Enc.payload_kind;
            Enc.nested;} = encoding_type in 
          let decoding  =  match field_type with 
            | T.User_defined_type t -> 
              let f_name = function_name_of_user_defined "decode" t in
              if nested 
              then 
                match_cases ~constructor:field_name 
                  field_number 
                  (tag_name variant_name) 
                  (f_name ^ " (Pbrt.Decoder.nested d)")
              else 
                match_cases ~constructor:field_name
                  field_number 
                  (tag_name variant_name) 
                  (f_name ^ " d")
            | _ -> 
              match_cases ~constructor:field_name  
                field_number 
                (tag_name variant_name) 
                (Backend_ocaml_static.runtime_function (`Decode, payload_kind, field_type) ^ " d")
          in 
          P.sprintf "  %s" decoding 
        ) variant_constructors (* All variant constructors *) 
      )                        (* One_of record field *)    
      | T.One_of {T.variant_name ; variant_constructors; T.variant_encoding = T.Standalone} -> (
        failwith "Programmatic error -> violation of invariant (1)"
      )
    ) fields ;
    sp "| _ -> raise Not_found ";
    "\n";
  ]

let max_field_number fields = 
  List.fold_left (fun max_so_far {T.encoding_type; _ } -> 
    match encoding_type with
    | T.Regular_field {Enc.field_number; _ } -> max max_so_far field_number 
    | T.One_of {T.variant_constructors; _ } -> 
        List.fold_left (fun max_so_far {T.encoding_type = {Enc.field_number; _ } ; _ } -> 
          max field_number max_so_far 
        ) max_so_far variant_constructors
  ) (- 1) fields

let gen_decode_record ?and_ ({T.record_name; fields } as record) = 
  concat [
    P.sprintf "%s decode_%s =" (let_decl_of_and and_) record_name;
    sp "%s" (add_indentation 1 @@ gen_mappings_record record); 
    sp "  in";
    sp "  (fun d ->"; 
    sp "    let a = Array.make %i (`Default) in " (max_field_number fields  + 1); 
    sp "    Pbrt.Codegen.decode d %s_mappings a; {" record_name;
    add_indentation 3 @@ concat @@ List.map (fun field -> 
      let {
        T.encoding_type;
        T.field_type; 
        T.field_name; 
        T.type_qualifier;
      } = field in 
      match encoding_type with 
      | T.Regular_field {Enc.field_number; _ } -> ( 
        let constructor = tag_name (string_of_field_type field_type) in  
        match type_qualifier with
        | T.No_qualifier -> 
          sp "%s = Pbrt.Codegen.required %i a (function | `%s __v -> __v | _ -> Pbrt.Codegen.programatic_error %i);"
            field_name field_number constructor field_number 
        | T.Option -> 
          sp "%s = Pbrt.Codegen.optional %i a (function | `%s __v -> __v | _ -> Pbrt.Codegen.programatic_error %i);"
            field_name field_number constructor field_number 
        | T.List -> 
          sp "%s = Pbrt.Codegen.list_ %i a (function | `%s __v -> __v | _ -> Pbrt.Codegen.programatic_error %i);"
            field_name field_number constructor field_number
      )
      | T.One_of {T.variant_constructors; T.variant_name; T.variant_encoding = T.Inlined_within_message } -> 
        let all_numbers = concat @@ List.map (fun {T.encoding_type= {Enc.field_number; _ } ; _ } -> 
          (P.sprintf "%i;" field_number)
        ) variant_constructors in 
        let all_numbers = concat ["["; all_numbers; "]"] in 
        sp "%s = Pbrt.Codegen.oneof %s a (function | `%s __v -> __v | _ -> Pbrt.Codegen.programatic_error (- 1));"
          field_name all_numbers (tag_name variant_name) 
      | T.One_of {T.variant_constructors; T.variant_name; T.variant_encoding = T.Standalone } -> (
        failwith "Programmatic error -> violation of invariant (2)"
      )  
    ) fields;
    sp "    }";
    sp "  )";
  ]

let gen_decode_variant ?and_ {T.variant_name; variant_constructors; variant_encoding = _ } = 
  concat [
    sp "%s decode_%s d = " (let_decl_of_and and_) variant_name;
    sp "  let rec loop () = "; 
    sp "    match Pbrt.Decoder.key d with";
    sp "    | None -> failwith \"None of the known key is found\"";
    concat @@ List.map (fun {T.encoding_type = {Enc.field_number; Enc.nested; Enc.payload_kind; _  }; field_type; field_name; type_qualifier = _ } -> 
      let decoding  =  match field_type with 
        | T.User_defined_type t -> 
          let f_name = function_name_of_user_defined "decode" t in
          if nested 
          then 
            (f_name ^ " (Pbrt.Decoder.nested d)")
          else 
            (f_name ^ " d")
        | _ -> 
            (Backend_ocaml_static.runtime_function (`Decode, payload_kind, field_type) ^ " d")
      in 
      sp "    | Some (%i, _) -> %s (%s)" field_number field_name decoding; 
    ) variant_constructors; 

    sp "    | Some (n, payload_kind) -> (";
    sp "      Pbrt.Decoder.skip d payload_kind; ";
    sp "      loop () ";
    sp "    ) ";
    sp "  in";
    sp "  loop () ";
  ]

let gen_decode_const_variant ?and_ {T.cvariant_name; cvariant_constructors; } = 
  concat [
    P.sprintf "%s decode_%s d = " (let_decl_of_and and_) cvariant_name; 
    sp "  match Pbrt.Decoder.int_as_varint d with";
    concat @@ List.map (fun (name, value) -> 
      sp "  | %i -> %s" value name
    ) cvariant_constructors; 
    sp "  | _ -> failwith \"Unknown value for enum %s\"" cvariant_name; 
  ] 

let gen_decode ?and_ = function 
  | {T.spec = T.Record r; _ } -> Some (gen_decode_record ?and_ r)
  | {T.spec = T.Variant v; _ } -> (match v.T.variant_encoding with 
    | T.Standalone -> Some (gen_decode_variant ?and_ v) 
    | _          -> None 
  )
  | {T.spec = T.Const_variant v; _ } -> Some (gen_decode_const_variant ?and_ v)

let gen_decode_sig t = 
  
  let f type_name = 
    concat [
      P.sprintf "val decode_%s : Pbrt.Decoder.t -> %s" 
        type_name type_name ;
      sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)"
        type_name type_name; 
    ]
  in 

  match t with 
  | {T.spec = T.Record {T.record_name ; _ } } ->  Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name)
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> Some (f cvariant_name)

let gen_encode_field ?indent v_name encoding_type field_type = 
  let {
    Enc.field_number; 
    Enc.payload_kind; 
    Enc.nested} = encoding_type in 
  let s = concat [
    sp "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
      field_number (constructor_name @@ Enc.string_of_payload_kind payload_kind);
    match field_type with 
    | T.User_defined_type t -> 
      let f_name = function_name_of_user_defined "encode" t in 
      if nested
      then 
        sp "Pbrt.Encoder.nested (%s %s) encoder;" f_name v_name 
      else 
        sp "%s %s encoder;" f_name v_name 
    | _ ->  
      let rt = Backend_ocaml_static.runtime_function (`Encode, payload_kind, field_type) in 
      sp "%s %s encoder;" rt v_name ;
  ] in 
  match indent with 
  | Some _ -> add_indentation 1 @@ s 
  | None   -> s 

let gen_encode_record ?and_ {T.record_name; fields } = 
  L.log "gen_encode_record record_name: %s\n" record_name; 

  concat [
    P.sprintf "%s encode_%s v encoder = " (let_decl_of_and and_) record_name;
    add_indentation 1 @@ concat @@ List.map (fun field -> 
      L.log "gen_code field_name: %s\n" field.T.field_name;

      let { T.encoding_type; field_type; field_name; type_qualifier ; } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        match type_qualifier with 
        | T.No_qualifier -> (
          let v_name = P.sprintf "v.%s" field_name in 
          gen_encode_field v_name encoding_type field_type
        )
        | T.Option -> concat [
          sp "(match v.%s with " field_name;
          sp "| Some x -> (%s)"
          (gen_encode_field ~indent:() "x" encoding_type field_type) ;
          sp "| None -> ());" ;
        ]
        | T.List -> concat [ 
          sp "List.iter (fun x -> ";
          gen_encode_field ~indent:() "x" encoding_type field_type;
          sp ") v.%s;" field_name; 
        ]
      )
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (  
        concat [
          sp "(match v.%s with" field_name;
          concat @@ List.map (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
              let encode_field  = gen_encode_field ~indent:() "x" encoding_type field_type in 
              sp "| %s x -> (%s\n)" field_name encode_field
          ) variant_constructors;
          ");";
        ]
      )           (* one of        *)
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Standalone} -> (  
        failwith "Programmatic error -> violation of invariant (3)"
      )
    ) fields;  (* record fields *) 
  "\n  ()"
  ]

let gen_encode_variant ?and_ {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = 
  concat [
    P.sprintf "%s encode_%s v encoder = " (let_decl_of_and and_) variant_name;
    sp "match v with" ;
    concat @@ List.map (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
        let encode_field  = gen_encode_field ~indent:() "x" encoding_type field_type in 
        sp "| %s x -> (%s\n)" field_name encode_field
    ) variant_constructors;
  ]

let gen_encode_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  concat [
    P.sprintf "%s encode_%s v encoder =" (let_decl_of_and and_) cvariant_name; 
    sp "  match v with";
    concat @@ List.map (fun (name, value) -> 
      sp "  | %s -> Pbrt.Encoder.int_as_varint %i encoder" name value
    ) cvariant_constructors; 
  ] 

let gen_encode ?and_ = function 
  | {T.spec = T.Record r }        -> Some (gen_encode_record  ?and_ r)
  | {T.spec = T.Variant v }       -> (match v.T.variant_encoding with 
    | T.Standalone -> Some (gen_encode_variant ?and_ v)
    | _ -> None 
  )
  | {T.spec = T.Const_variant v } -> Some (gen_encode_const_variant ?and_ v)

let gen_encode_sig t = 
  let f type_name = 
  concat [
    P.sprintf "val encode_%s : %s -> Pbrt.Encoder.t -> unit"
      type_name
      type_name;
    sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" 
      type_name  
  ]
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } }-> Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name) 
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> Some (f cvariant_name) 

let gen_string_of_field field_name field_type encoding_type = 
  match field_type with 
  | T.User_defined_type t -> 
    P.sprintf "P.sprintf \"\\n%s: %%s\" @@ %s x" field_name (function_name_of_user_defined "string_of" t) 
  | _ ->  
    P.sprintf "P.sprintf \"\\n%s: %%%s\" x"  
      field_name 
      (printf_string_of_field_type field_type)

let gen_string_of_record  ?and_ {T.record_name; fields } = 
  L.log "gen_string_of, record_name: %s\n" record_name; 

  concat [
    P.sprintf "%s string_of_%s v = " (let_decl_of_and and_) record_name;
    "\n  add_indentation 1 @@ String.concat \"\" [";
    add_indentation 2 @@ concat @@ List.map (fun field -> 
      L.log "gen_string_of field_name: %s\n" field.T.field_name;
     
      let { T.field_type; field_name; type_qualifier ; encoding_type} = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        match type_qualifier with
        | T.No_qualifier -> 
          let field_string_of = gen_string_of_field field_name field_type encoding_type in 
          sp "(let x = v.%s in %s);" field_name field_string_of 
        | T.Option -> 
          concat [
            sp "(match v.%s with " field_name;
            sp "| Some x -> (%s)"  (gen_string_of_field field_name field_type encoding_type);
            sp "| None -> \"\\n%s: None\");" field_name;
          ]
        | T.List -> 
          concat [
            sp "String.concat \"\" @@ List.map (fun x ->";
            nl @@ gen_string_of_field field_name field_type encoding_type; 
            sp ") v.%s;" field_name
          ]
      )
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (
        concat [
          sp "(match v.%s with" field_name;
          concat @@ List.map (fun {T.encoding_type; field_type; field_name;
          type_qualifier= _ } ->
            let field_string_of = gen_string_of_field field_name field_type encoding_type in 
            sp "| %s x -> (%s)" field_name (add_indentation 1 field_string_of)
          ) variant_constructors ;
          "\n);"       (* one of fields *) 
        ]
      )                (* one of        *)
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone } -> (
        failwith "Programmatic error -> violation of invariant (4)"
      )
    ) fields;          (* record fields *) 
    "\n  ]";
  ]

let gen_string_of_variant ?and_ {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = 
  concat [
    P.sprintf "%s string_of_%s v = " (let_decl_of_and and_) variant_name;
    sp "  match v with" ;
    concat @@ List.map (fun {T.encoding_type; field_type; field_name; type_qualifier= _ } ->
      let field_string_of = gen_string_of_field field_name field_type encoding_type in 
      sp "| %s x -> (%s)" field_name (add_indentation 1 field_string_of)
    ) variant_constructors ;
  ]

let gen_string_of_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  concat [
    P.sprintf "%s string_of_%s v =" (let_decl_of_and and_) cvariant_name; 
    sp "  match v with";
    concat @@ List.map (fun (name, _ ) -> 
      sp "  | %s -> \"%s\"" name name
    ) cvariant_constructors; 
  ] 

let gen_string_of ?and_ = function 
  | {T.spec = T.Record r  } -> Some (gen_string_of_record ?and_ r) 
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with 
    | T.Standalone  -> Some (gen_string_of_variant ?and_ v) 
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant v } -> Some (gen_string_of_const_variant ?and_ v)

let gen_string_of_sig t = 
  let f type_name =  
     concat [
       P.sprintf "val string_of_%s : %s -> string " type_name type_name;
       sp "(** [string_of_%s v] returns a debugging string for [v] *)" type_name;
     ]
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } }-> Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name)
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name; _ ; } } -> Some (f cvariant_name) 

let gen_default_field field_name field_type field_default = 
  match field_type, field_default with 
  | T.User_defined_type t, _ -> 
    function_name_of_user_defined "default" t
  | T.String, None -> "\"\""
  | T.String, Some (Pbpt.Constant_string s) -> P.sprintf "\"%s\"" s 
  | T.Float , None -> "0." 
  | T.Float , Some (Pbpt.Constant_float f) -> string_of_float f
  | T.Int   , None -> "0"
  | T.Int   , Some (Pbpt.Constant_int i) -> string_of_int i
  | T.Int32 , None -> "0l"
  | T.Int32 , Some (Pbpt.Constant_int i) -> P.sprintf "%il" i
  | T.Int64 , None -> "0L"
  | T.Int64 , Some (Pbpt.Constant_int i) -> P.sprintf "%iL" i
  | T.Bytes , None -> "Bytes.create 64"  
  | T.Bytes , Some (Pbpt.Constant_string s) -> P.sprintf "Bytes.of_string \"%s\"" s  
  | T.Bool  , None -> "false"
  | T.Bool  , Some (Pbpt.Constant_bool b) -> string_of_bool b
  | _ -> raise @@ E.invalid_default_value 
    ~field_name ~info:"invalid default type" ()

let gen_default_record  ?and_ {T.record_name; fields } = 
  L.log "gen_string_of, record_name: %s\n" record_name; 


  concat [
    P.sprintf "%s default_%s = {" (let_decl_of_and and_) record_name;
    add_indentation 1 @@ concat @@ List.map (fun field -> 
      L.log "gen_string_of field_name: %s\n" field.T.field_name;
     
      let { T.field_type; field_name; type_qualifier ; encoding_type } = field in 
      match encoding_type with 
      | T.Regular_field encoding_type -> ( 
        match type_qualifier with
        | T.No_qualifier -> 
          let field_default_of = gen_default_field field_name field_type encoding_type.Encoding_util.default in 
          sp "%s = %s;" field_name field_default_of; 
        | T.Option -> 
          sp "%s = None;" field_name; 
        | T.List -> 
          sp "%s = [];" field_name; 
      )
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Inlined_within_message} -> (
        match variant_constructors with
        | []     -> failwith "programmatic TODO error" 
        | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
          sp "%s = %s %s;" field_name constructor_name (gen_default_field field_name field_type encoding_type.Encoding_util.default)  
      )                (* one of        *)
      | T.One_of {T.variant_constructors; variant_name = _; T.variant_encoding = T.Standalone} -> (
        failwith "Programmatic error -> violation of invariant (5)"
      )
    ) fields;          (* record fields *) 
    "\n}";
  ]

let gen_default_variant ?and_ {T.variant_name; T.variant_constructors ; T.variant_encoding = _ } = 
  match variant_constructors with
  | []     -> failwith "programmatic TODO error" 
  | {T.field_type; field_name = constructor_name ; encoding_type; _ } :: _ ->
    sp "%s default_%s = %s %s" 
       (let_decl_of_and and_) 
       variant_name 
       constructor_name 
       (gen_default_field variant_name field_type encoding_type.Encoding_util.default)  

let gen_default_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } = 
  let first_constructor_name = match cvariant_constructors with
    | []            -> failwith "programmatic TODO error"
    | (name, _) ::_ -> name 
  in  
  P.sprintf "%s default_%s = %s" (let_decl_of_and and_) cvariant_name first_constructor_name

let gen_default ?and_ = function 
  | {T.spec = T.Record r  } -> Some (gen_default_record ?and_ r) 
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (gen_default_variant ?and_ v)
    | T.Inlined_within_message -> None
  )
  | {T.spec = T.Const_variant v } -> Some (gen_default_const_variant v)

let gen_default_sig t = 
  let f type_name =  
     concat [
       P.sprintf "val default_%s : %s" type_name type_name;
       sp "(** [default_%s] is the default value for type [%s] *)" type_name type_name;
     ]
  in 
  match t with 
  | {T.spec = T.Record {T.record_name ; _ } }-> Some (f record_name)
  | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
    | T.Standalone -> Some (f v.T.variant_name)
    | T.Inlined_within_message -> None
  ) 
  | {T.spec = T.Const_variant {T.cvariant_name ; _ ; } } -> Some (f cvariant_name) 
