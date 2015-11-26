module P = Printf
module E  = Exception 
module L  = Logger 
module OCaml_types = Ocaml_types

let printf_char_of_field_type = function
  | OCaml_types.String    -> 's' 
  | OCaml_types.Float     -> 'f'
  | OCaml_types.Int       -> 'i'
  | OCaml_types.Bytes     -> 's'
  | OCaml_types.Bool      -> 'b'
  | OCaml_types.User_defined_type _ -> 's'

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let tag_name s =
  String.capitalize @@ String.lowercase (String.map (function | '.' -> '_' | c -> c ) s) 
  
(** utility function used to generate decode/encode function names 
      which are implemented in [Backend_ocaml_static].
 *)
let fname_of_payload_kind = function 
  | Encoding_util.Varint zigzag -> if zigzag then "varint_zigzag" else "varint"
  | Encoding_util.Bits32        -> "bits32"
  | Encoding_util.Bits64        -> "bits64"
  | Encoding_util.Bytes         -> "bytes"

let string_of_field_type type_qualifier field_type = 
  let s = match field_type with 
    | OCaml_types.String -> "string"
    | OCaml_types.Float  -> "float"
    | OCaml_types.Int    -> "int"
    | OCaml_types.Bytes  -> "bytes"
    | OCaml_types.Bool   -> "bool"
    | OCaml_types.User_defined_type t -> t
  in
  match type_qualifier with 
  | OCaml_types.No_qualifier -> s 
  | OCaml_types.Option       -> s ^ " option"
  | OCaml_types.List         -> s ^ " list"

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

let gen_type_record ?and_ {OCaml_types.record_name; fields } = 
  concat [
    P.sprintf "%s %s = {" (type_decl_of_and and_) record_name;
    concat @@ List.map (fun {OCaml_types.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type type_qualifier field_type in 
      sp "  %s : %s;" field_name type_name
    ) fields;
    "\n}"
  ]

let gen_type_variant ?and_ {OCaml_types.variant_name; constructors } = 
  concat [
    P.sprintf "%s %s =" (type_decl_of_and and_) variant_name; 
    concat @@ List.map (fun {OCaml_types.field_name; field_type; type_qualifier; _ } -> 
      let type_name = string_of_field_type type_qualifier field_type in 
      sp "  | %s of %s" field_name type_name
    ) constructors;
  ]

let gen_type_const_variant ?and_ {OCaml_types.variant_name; constructors } = 
  concat [
    P.sprintf "%s %s =" (type_decl_of_and and_) variant_name; 
    concat @@ List.map (fun (name, _ ) -> 
      sp "  | %s " name
    ) constructors;
  ]

let gen_type ?and_ = function 
  | {Ocaml_types.spec = Record r; _ } -> gen_type_record ?and_ r 
  | {Ocaml_types.spec = Variant v; _ } -> gen_type_variant  ?and_ v 
  | {Ocaml_types.spec = Const_variant v; _ } -> gen_type_const_variant ?and_ v 

(** [gen_mappings_record r] generates a per record variable to hold the 
    mapping between a field number and the associated decoding routine. 

    Because the order of fields inside the protobuffer message is not
    guaranteed, the decoding cannot be done in one step.   
    The decoding code must therefore first collect all the record fields 
    values and then create the value of the OCaml type. 
  *)
let gen_mappings_record {OCaml_types.record_name; fields} =

  concat [
    P.sprintf "let %s_mappings d = function " record_name;
    concat @@ List.map (fun {OCaml_types.encoding_type;field_type;_ } -> 
      match encoding_type with 
      | OCaml_types.Regular_field {
        Encoding_util.field_number; 
        Encoding_util.payload_kind;
        Encoding_util.location;
        Encoding_util.nested } -> (
        let decoding = match field_type with 
          | OCaml_types.User_defined_type t -> 
            let f_name = match location with 
              | Encoding_util.Other_file {file_name; type_name} -> 
                let module_ = Backend_ocaml.module_of_file_name file_name in 
                P.sprintf "%s.decode_%s" module_ (String.lowercase type_name) 
              | _ -> 
                P.sprintf "decode_%s" t 
            in 
            if nested 
            then  
             P.sprintf "`%s (%s (Pc.Decoder.nested d))" (tag_name t) f_name
            else 
             P.sprintf "`%s (%s d)" (tag_name t) f_name 
          | _ -> 
             let field_type = string_of_field_type OCaml_types.No_qualifier field_type in 
             P.sprintf "`%s (decode_%s_as_%s d)" 
               (tag_name field_type)
               (fname_of_payload_kind payload_kind)
               field_type 
        in 
        sp "  | %i -> %s " field_number decoding 
      )
      | OCaml_types.One_of {OCaml_types.variant_name ; constructors; } -> (
        concat @@ List.map (fun {OCaml_types.encoding_type; field_type; field_name; type_qualifier = _ } -> 
          let {
            Encoding_util.field_number; 
            Encoding_util.payload_kind;
            Encoding_util.nested; 
            Encoding_util.location; } = encoding_type in 
          let decoding  =  match field_type with 
            | OCaml_types.User_defined_type t -> 
              let f_name = match location with 
                | Encoding_util.Other_file {file_name; type_name} -> 
                  let module_ = Backend_ocaml.module_of_file_name file_name in 
                  P.sprintf "%s.decode_%s" module_ (String.lowercase type_name) 
                | _ -> 
                  P.sprintf "decode_%s" t 
              in 

              if nested 
              then 
                P.sprintf "`%s (%s (%s (Pc.Decoder.nested d)))" 
                  (tag_name variant_name) field_name f_name
              else 
                P.sprintf "`%s (%s (%s d)" 
                  (tag_name variant_name) field_name f_name 
            | _ -> 
              let field_type = string_of_field_type OCaml_types.No_qualifier field_type in 
              P.sprintf "`%s (%s (decode_%s_as_%s d))" 
                (tag_name variant_name)
                field_name
                (fname_of_payload_kind payload_kind)
                field_type 
          in 
          sp "  | %i -> %s" field_number decoding 
        ) constructors (* All variant constructors *) 
      )                (* One_of record field *)    
    ) fields ;
    sp "  | _ -> raise Not_found ";
    "\n";
  ]

let max_field_number fields = 
  List.fold_left (fun max_so_far {OCaml_types.encoding_type; _ } -> 
    match encoding_type with
    | OCaml_types.Regular_field {Encoding_util.field_number; _ } -> max max_so_far field_number 
    | OCaml_types.One_of {OCaml_types.constructors; _ } -> 
        List.fold_left (fun max_so_far {OCaml_types.encoding_type = {Encoding_util.field_number; _ } ; _ } -> 
          max field_number max_so_far 
        ) max_so_far constructors 
  ) (- 1) fields

let gen_decode_record ?and_ ({OCaml_types.record_name; fields } as record) = 
  concat [
    P.sprintf "%s decode_%s =" (let_decl_of_and and_) record_name;
    sp "%s" (add_indentation 1 @@ gen_mappings_record record); 
    sp "  in";
    sp "  (fun d ->"; 
    sp "    let a = decode d %s_mappings (Array.make %i []) in {" record_name (max_field_number fields + 1);
    add_indentation 3 @@ concat @@ List.map (fun field -> 
      let {
        OCaml_types.encoding_type;
        OCaml_types.field_type; 
        OCaml_types.field_name; 
        OCaml_types.type_qualifier;
      } = field in 
      match encoding_type with 
      | OCaml_types.Regular_field {Encoding_util.field_number; _ } -> ( 
          let constructor = tag_name (string_of_field_type OCaml_types.No_qualifier field_type) in  
          match type_qualifier with
          | OCaml_types.No_qualifier -> 
            sp "%s = required %i a (function | `%s __v -> __v | _ -> e());"
              field_name field_number constructor
          | OCaml_types.Option -> 
            sp "%s = optional %i a (function | `%s __v -> __v | _ -> e());"
              field_name field_number constructor
          | OCaml_types.List -> 
            sp "%s = list_ %i a (function | `%s __v -> __v | _ -> e());"
              field_name field_number constructor
      )
      | OCaml_types.One_of {OCaml_types.constructors; variant_name} -> 
          let all_numbers = concat @@ List.map (fun {OCaml_types.encoding_type= {Encoding_util.field_number; _ } ; _ } -> 
            (P.sprintf "%i;" field_number)
          ) constructors in 
          let all_numbers = concat ["["; all_numbers; "]"] in 
          sp "%s = (match oneof %s a with | `%s __v -> __v | _ -> e());"
            field_name all_numbers (tag_name variant_name)
    ) fields;
    sp "    }";
    sp "  )";
  ]

let gen_decode_const_variant ?and_ {OCaml_types.variant_name; constructors; } = 
  concat [
    P.sprintf "%s decode_%s d = " (let_decl_of_and and_) variant_name; 
    sp "  match decode_varint_as_int d with";
    concat @@ List.map (fun (name, value) -> 
      sp "  | %i -> %s" value name
    ) constructors; 
    sp "  | _ -> failwith \"Unknown value for enum %s\"" variant_name; 
  ] 

let gen_decode ?and_ = function 
  | {OCaml_types.spec = Record r; _ } -> Some (gen_decode_record ?and_ r)
  | {OCaml_types.spec = Variant _; _ } -> None
  | {OCaml_types.spec = Const_variant v; _ } -> Some (gen_decode_const_variant ?and_ v)

let gen_decode_sig t = 
  let f type_name = 
    concat [
      P.sprintf "val decode_%s : Protobuf_codec.Decoder.t -> %s" 
        type_name type_name ;
      sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)"
        type_name type_name; 
    ]
  in 

  match t with 
  | {OCaml_types.spec = Record {OCaml_types.record_name ; _ } } ->  Some (f record_name)
  | {OCaml_types.spec = Variant _ } -> None
  | {OCaml_types.spec = Const_variant {OCaml_types.variant_name; _ } } -> Some (f variant_name)

let gen_encode_record ?and_ {OCaml_types.record_name; fields } = 
  L.log "gen_encode_record record_name: %s\n" record_name; 

  let gen_field ?indent v_name encoding_type field_type = 
    let {
      Encoding_util.field_number; 
      Encoding_util.payload_kind; 
      Encoding_util.location; 
      Encoding_util.nested} = encoding_type in 
    let s = concat [
      sp "Pc.Encoder.key (%i, Pc.%s) encoder; " 
        field_number (constructor_name @@ Encoding_util.string_of_payload_kind payload_kind);
      match field_type with 
      | OCaml_types.User_defined_type t -> 
        let f_name = match location with 
          | Encoding_util.Other_file {file_name; type_name} -> 
            let module_ = Backend_ocaml.module_of_file_name file_name in 
            P.sprintf "%s.encode_%s" module_ (String.lowercase type_name) 
          | _ -> 
            P.sprintf "encode_%s" t 
        in 
        if nested
        then 
          sp "Pc.Encoder.nested (%s %s) encoder;" f_name v_name 
        else 
          sp "%s %s encoder;" f_name v_name 
      | _ ->  
        sp "encode_%s_as_%s %s encoder;"
          (string_of_field_type OCaml_types.No_qualifier field_type) 
          (fname_of_payload_kind payload_kind) 
          v_name ;
    ] in 
    match indent with 
    | Some _ -> add_indentation 1 @@ s 
    | None   -> s 
  in

  concat [
    P.sprintf "%s encode_%s v encoder = " (let_decl_of_and and_) record_name;
    add_indentation 1 @@ concat @@ List.map (fun field -> 
      L.log "gen_code field_name: %s\n" field.OCaml_types.field_name;

      let { OCaml_types.encoding_type; field_type; field_name; type_qualifier ; } = field in 
      match encoding_type with 
      | OCaml_types.Regular_field encoding_type -> ( 
        match type_qualifier with 
        | OCaml_types.No_qualifier -> (
          let v_name = P.sprintf "v.%s" field_name in 
          gen_field v_name encoding_type field_type
        )
        | OCaml_types.Option -> concat [
          sp "(match v.%s with " field_name;
          sp "| Some x -> (%s)"
          (gen_field ~indent:() "x" encoding_type field_type) ;
          sp "| None -> ());" ;
        ]
        | OCaml_types.List -> concat [ 
          sp "List.iter (fun x -> ";
          gen_field ~indent:() "x" encoding_type field_type;
          sp ") v.%s;" field_name; 
        ]
      )
      | OCaml_types.One_of {OCaml_types.constructors; variant_name = _} -> (  
        concat [
          sp "(match v.%s with" field_name;
          concat @@ List.map (fun {OCaml_types.encoding_type; field_type; field_name; type_qualifier= _ } ->
              let encode_field  = gen_field ~indent:() "x" encoding_type field_type in 
              sp "| %s x -> (%s\n)" field_name encode_field
          ) constructors;
          ");";
        ]
      )           (* one of        *)
    ) fields;  (* record fields *) 
  "\n  ()"
  ]

let gen_encode_const_variant ?and_ {OCaml_types.variant_name; constructors; } = 
  concat [
    P.sprintf "%s encode_%s v encoder =" (let_decl_of_and and_) variant_name; 
    sp "  match v with";
    concat @@ List.map (fun (name, value) -> 
      sp "  | %s -> encode_int_as_varint %i encoder" name value
    ) constructors; 
  ] 

let gen_encode ?and_ = function 
  | {OCaml_types.spec = Record r }        -> Some (gen_encode_record  ?and_ r)
  | {OCaml_types.spec = Variant _ }       -> None 
  | {OCaml_types.spec = Const_variant v } -> Some (gen_encode_const_variant ?and_ v)

let gen_encode_sig t = 
  let f type_name = 
  concat [
    P.sprintf "val encode_%s : %s -> Protobuf_codec.Encoder.t -> unit"
      type_name
      type_name;
    sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" 
      type_name  
  ]
  in 
  match t with 
  | {OCaml_types.spec = Record {OCaml_types.record_name ; _ } }-> Some (f record_name)
  | {OCaml_types.spec = Variant _ } -> None
  | {OCaml_types.spec = Const_variant {OCaml_types.variant_name; _ } } -> Some (f variant_name) 

let gen_string_of_record  ?and_ {OCaml_types.record_name; fields } = 
  L.log "gen_string_of, record_name: %s\n" record_name; 

  let gen_field field_name field_type encoding_type = 
    match field_type with 
    | OCaml_types.User_defined_type t -> (
      match encoding_type with
      | {Encoding_util.location = Other_file {file_name;type_name} ; _ } -> 
        let module_   = Backend_ocaml.module_of_file_name file_name in 
        let type_name = String.lowercase type_name in  
        P.sprintf "P.sprintf \"\\n%s: %%s\" @@ %s.string_of_%s x" field_name module_ type_name 
      | _ -> 
        P.sprintf "P.sprintf \"\\n%s: %%s\" @@ string_of_%s x" field_name t  
    )
    | _ ->  
      P.sprintf "P.sprintf \"\\n%s: %%%c\" x"  
        field_name 
        (printf_char_of_field_type field_type)
  in

  concat [
    P.sprintf "%s string_of_%s v = " (let_decl_of_and and_) record_name;
    "\n  add_indentation 1 @@ String.concat \"\" [";
    add_indentation 2 @@ concat @@ List.map (fun field -> 
      L.log "gen_string_of field_name: %s\n" field.OCaml_types.field_name;
     
      let { OCaml_types.field_type; field_name; type_qualifier ; encoding_type} = field in 
      match encoding_type with 
      | OCaml_types.Regular_field encoding_type -> ( 
        match type_qualifier with
        | OCaml_types.No_qualifier -> 
          let field_string_of = gen_field field_name field_type encoding_type in 
          sp "(let x = v.%s in %s);" field_name field_string_of 
        | OCaml_types.Option -> 
          concat [
            sp "(match v.%s with " field_name;
            sp "| Some x -> (%s)"  (gen_field field_name field_type encoding_type);
            sp "| None -> \"\\n%s: None\");" field_name;
          ]
        | OCaml_types.List -> 
          concat [
            sp "String.concat \"\" @@ List.map (fun x ->";
            nl @@ gen_field field_name field_type encoding_type; 
            sp ") v.%s;" field_name
          ]
      )
      | OCaml_types.One_of {OCaml_types.constructors; variant_name = _} -> (
        concat [
          sp "(match v.%s with" field_name;
          concat @@ List.map (fun {OCaml_types.encoding_type; field_type; field_name;
          type_qualifier= _ } ->
            let field_string_of = gen_field field_name field_type encoding_type in 
            sp "| %s x -> (%s)" field_name (add_indentation 1 field_string_of)
          ) constructors ;
          "\n);"       (* one of fields *) 
        ]
      )                (* one of        *)
    ) fields;          (* record fields *) 
    "\n  ]";
  ]

let gen_string_of_const_variant ?and_ {OCaml_types.variant_name; constructors; } = 
  concat [
    P.sprintf "%s string_of_%s v =" (let_decl_of_and and_) variant_name; 
    sp "  match v with";
    concat @@ List.map (fun (name, _ ) -> 
      sp "  | %s -> \"%s\"" name name
    ) constructors; 
  ] 

let gen_string_of ?and_ = function 
  | {OCaml_types.spec = Record r  } -> Some (gen_string_of_record ?and_ r) 
  | {OCaml_types.spec = Variant _ } -> None
  | {OCaml_types.spec = Const_variant v } -> Some (gen_string_of_const_variant ?and_ v)

let gen_string_of_sig t = 
  let f type_name =  
     concat [
       P.sprintf "val string_of_%s : %s -> string " type_name type_name;
       sp "(** [string_of_%s v] returns a debugging string for [v] *)" type_name;
     ]
  in 
  match t with 
  | {OCaml_types.spec = Record {OCaml_types.record_name ; _ } }-> Some (f record_name)
  | {OCaml_types.spec = Variant _ } -> None
  | {OCaml_types.spec = Const_variant {OCaml_types.variant_name; _ ; } } -> Some (f variant_name) 
