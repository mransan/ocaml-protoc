
module Pc = Protobuf_codec
module E  = Exception 
module L  = Logger 

type field_type = 
  | String 
  | Float 
  | Int 
  | Bytes
  | Bool
  | User_defined_type of string 

let printf_char_of_field_type = function
  | String    -> 's' 
  | Float     -> 'f'
  | Int       -> 'i'
  | Bytes     -> 's'
  | Bool      -> 'b'
  | User_defined_type _ -> 's'

(** utility function used to generate decode/encode function names 
    which are implemented in [Backend_ocaml_static].
 *)
let fname_of_payload_kind = function 
  | Encoding_util.Varint zigzag -> if zigzag then "varint_zigzag" else "varint"
  | Encoding_util.Bits32        -> "bits32"
  | Encoding_util.Bits64        -> "bits64"
  | Encoding_util.Bytes         -> "bytes"

type field_name = string 

type type_qualifier = 
  | No_qualifier
  | Option
  | List 

let string_of_field_type type_qualifier field_type = 
  let s = match field_type with 
    | String -> "string"
    | Float  -> "float"
    | Int    -> "int"
    | Bytes  -> "bytes"
    | Bool   -> "bool"
    | User_defined_type t -> t
  in
  match type_qualifier with 
  | No_qualifier -> s 
  | Option       -> s ^ " option"
  | List         -> s ^ " list"

type 'a ivariant= {
  variant_name : string; 
  constructors : 'a list;
}

type 'a ifield = {
  field_type : field_type; 
  field_name : field_name; 
  type_qualifier : type_qualifier; 
  encoding_type : 'a;
}

type const_variant_constructor = string * int  

type const_variant = const_variant_constructor ivariant 

type variant_constructor = Encoding_util.field_encoding ifield 

type variant = variant_constructor ivariant 

type record_encoding_type = 
  | Regular_field of Encoding_util.field_encoding
  | One_of        of variant  

and record = {
  record_name: string; 
  fields : record_encoding_type ifield list; 
}

type type_ = 
  | Record of record 
  | Variant of variant
  | Const_variant of const_variant 

let type_name message_scope name = 
  let module S = String in  
  let all_names =  message_scope @ [name] in 
  match all_names with 
  | []     -> failwith "Programmatic error"
  | hd::[] -> S.lowercase hd 
  | _      -> S.concat "_" @@ List.map S.lowercase all_names

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let record_field_name s =
  String.lowercase s 

let empty = function | [] -> true | _ -> false 

let type_name_of_message field_message_scope message_scope message_name = 
  let module S = String in 

  let message_scope = 
    (* TODO this is a brute force method which only works for 
        field which are in the same namespaces as their types. 
     *) 
    if field_message_scope.Pbtt.namespaces = message_scope.Pbtt.namespaces 
    then {message_scope with 
      Pbtt.namespaces = [] 
    }
    else message_scope in  

  let {Pbtt.namespaces; Pbtt.message_names} = message_scope in 

  if empty namespaces && empty message_names 
  then S.lowercase message_name 
  else 

    let namespaces = List.map (fun s -> 
      S.capitalize @@ S.lowercase s
    ) namespaces in 
    let message_names = List.map S.lowercase message_names in 

    let module_prefix = match namespaces with
      | [] -> ""
      | _  -> 
        S.concat "." namespaces  ^ "."
    in 
    match message_names with
    | [] -> module_prefix ^ (S.lowercase message_name)
    | _  -> 
      module_prefix ^ 
      S.concat "_" message_names ^ 
      "_" ^
      S.lowercase message_name 


let get_type_name_from_all_messages field_message_scope all_types i = 
  let module S = String in 
  try 
    let t = Pbtt_util.type_of_id all_types i  in 
    let type_scope = Pbtt_util.type_scope_of_type t in 
    let type_name  = Pbtt_util.type_name_of_type  t in 
    type_name_of_message field_message_scope type_scope type_name 
  with | Not_found -> failwith "Programmatic error could not find type"

let compile_field ?as_constructor f type_qualifier message_scope all_types field = 
  let field_name = Pbtt_util.field_name field in 
  let encoding_type = Pbtt_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

  let field_encoding = Encoding_util.encoding_of_field_type all_types field in 
  let field_type   = match encoding_type with
    | Pbtt.Field_type_double  -> Float
    | Pbtt.Field_type_float  ->  Float
    | Pbtt.Field_type_int32  ->  Int
    | Pbtt.Field_type_int64  ->  Int
    | Pbtt.Field_type_uint32  -> Int
    | Pbtt.Field_type_uint64 -> Int
    | Pbtt.Field_type_sint32  -> Int
    | Pbtt.Field_type_sint64  -> Int
    | Pbtt.Field_type_fixed32  -> Int
    | Pbtt.Field_type_fixed64  -> Int
    | Pbtt.Field_type_sfixed32  -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed32" ~backend_name:"OCaml" () 
    | Pbtt.Field_type_sfixed64 -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed64" ~backend_name:"OCaml" () 
    | Pbtt.Field_type_bool  -> Bool
    | Pbtt.Field_type_string  -> String
    | Pbtt.Field_type_bytes  -> Bytes
    | Pbtt.Field_type_type id -> 
      let name = get_type_name_from_all_messages message_scope all_types id in 
      User_defined_type name 
  in 
  {
    field_type; 
    field_name; 
    type_qualifier; 
    encoding_type = f field_encoding ; 
  }

let compile_oneof all_types message_scope outer_message_name {Pbtt.oneof_name ; Pbtt.oneof_fields } = 
  let {Pbtt.message_names; _ } = message_scope in 
  let variant_name = type_name (message_names @ [outer_message_name]) oneof_name in 
  let constructors = List.map (fun field -> 
    (* TODO fix hard coding the empty_scope and rather
        pass down the appropriate scope.
      *)
    compile_field ~as_constructor:() (fun x -> x)  No_qualifier message_scope all_types field 
  ) oneof_fields in 
  {variant_name; constructors; }

let compile_message  
  (all_types: Pbtt.resolved Pbtt.proto) 
  (message: Pbtt.resolved Pbtt.message ) :
  type_ list   = 

  let {
    Pbtt.message_scope;
    Pbtt.message_name; 
    Pbtt.message_body; 
    Pbtt.id = _ ; 
  } = message in 

  let {Pbtt.message_names; Pbtt.namespaces = _ } = message_scope in 
  let record_name = type_name message_names message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Pbtt.Message_field f -> (
      let type_qualifier = match Pbtt_util.field_label f with 
        | `Optional -> Option 
        | `Required -> No_qualifier
        | `Repeated -> List
      in 
      (variants, (compile_field (fun x -> Regular_field x) type_qualifier message_scope all_types f)::fields)
    )
    | Pbtt.Message_oneof_field f -> (
      let variant = compile_oneof all_types message_scope message_name f in 
      let field   = {
        field_type =  User_defined_type (variant.variant_name); 
        field_name =  record_field_name f.Pbtt.oneof_name;
        type_qualifier = No_qualifier;
        encoding_type = One_of variant; 
      } in 
      ((Variant variant)::variants, field::fields) 
    )
  ) ([], []) message_body in 

  List.rev (Record {
    record_name; 
    fields = List.rev fields;
  } :: variants)

let compile_enum {Pbtt.enum_name; Pbtt.enum_values; Pbtt.enum_scope; Pbtt.enum_id = _ } = 
  let {Pbtt.message_names; Pbtt.namespaces = _ } = enum_scope in 
  let variant_name = type_name message_names enum_name in 
  let constructors = List.map (fun {Pbtt.enum_value_name; Pbtt.enum_value_int} -> 
    (constructor_name enum_value_name,  enum_value_int)
  ) enum_values in 
  Const_variant {
    variant_name; 
    constructors;  
  }

let compile all_types = function 
  | Pbtt.Message m -> compile_message all_types m 
  | Pbtt.Enum    e -> [compile_enum e] 

module Codegen = struct 
  module P = Printf
  
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

  let gen_type_record ?and_ {record_name; fields } = 
    concat [
      P.sprintf "%s %s = {" (type_decl_of_and and_) record_name;
      concat @@ List.map (fun {field_name; field_type; type_qualifier; _ } -> 
        let type_name = string_of_field_type type_qualifier field_type in 
        sp "  %s : %s;" field_name type_name
      ) fields;
      "\n}"
    ]
  
  let gen_type_variant ?and_ {variant_name; constructors } = 
    concat [
      P.sprintf "%s %s =" (type_decl_of_and and_) variant_name; 
      concat @@ List.map (fun {field_name; field_type; type_qualifier; _ } -> 
        let type_name = string_of_field_type type_qualifier field_type in 
        sp "  | %s of %s" field_name type_name
      ) constructors;
    ]
  
  let gen_type_const_variant ?and_ {variant_name; constructors } = 
    concat [
      P.sprintf "%s %s =" (type_decl_of_and and_) variant_name; 
      concat @@ List.map (fun (name, _ ) -> 
        sp "  | %s " name
      ) constructors;
    ]

  let gen_type ?and_ = function 
    | Record r        -> gen_type_record ?and_ r 
    | Variant v       -> gen_type_variant  ?and_ v 
    | Const_variant v -> gen_type_const_variant ?and_ v 

  (** [gen_mappings_record r] generates a per record variable to hold the 
      mapping between a field number and the associated decoding routine. 

      Because the order of fields inside the protobuffer message is not
      guaranteed, the decoding cannot be done in one step.   
      The decoding code must therefore first collect all the record fields 
      values and then create the value of the OCaml type. 
    *)
  let gen_mappings_record {record_name; fields} =

    concat [
      P.sprintf "let %s_mappings = [" record_name;
      concat @@ List.map (fun {encoding_type;field_type;_ } -> 
        match encoding_type with 
        | Regular_field {
          Encoding_util.field_number; 
          Encoding_util.payload_kind;
          Encoding_util.nested } -> (
          let decoding = match field_type with 
            | User_defined_type t -> 
              if nested 
              then  
               P.sprintf "(fun d -> `%s (decode_%s (Pc.Decoder.nested d)))" (constructor_name t) t  
              else 
               P.sprintf "(fun d -> `%s (decode_%s d))" (constructor_name t) t  
            | _ -> 
               let field_type = string_of_field_type No_qualifier field_type in 
               P.sprintf "(fun d -> `%s (decode_%s_as_%s d))" 
                 (constructor_name field_type)
                 (fname_of_payload_kind payload_kind)
                 field_type 
          in 
          sp "  (%i, %s);" field_number decoding 
        )
        | One_of {variant_name ; constructors; } -> (
          concat @@ List.map (fun {encoding_type; field_type; field_name; type_qualifier = _ } -> 
            let {
              Encoding_util.field_number; 
              Encoding_util.payload_kind;
              Encoding_util.nested} = encoding_type in 
            let decoding  =  match field_type with 
              | User_defined_type t -> 
                if nested 
                then 
                  P.sprintf "(fun d -> `%s (%s (decode_%s (Pc.Decoder.nested d))))" 
                    (constructor_name variant_name) field_name t  
                else 
                  P.sprintf "(fun d -> `%s (%s (decode_%s d))" 
                    (constructor_name variant_name) field_name t  
              | _ -> 
                let field_type = string_of_field_type No_qualifier field_type in 
                P.sprintf "(fun d -> `%s (%s (decode_%s_as_%s d)))" 
                  (constructor_name variant_name)
                  field_name
                  (fname_of_payload_kind payload_kind)
                  field_type 
            in 
            sp "  (%i, %s);" field_number decoding 
          ) constructors (* All variant constructors *) 
        )                (* One_of record field *)    
      ) fields ;
      "\n]";
    ]

  let max_field_number fields = 
    List.fold_left (fun max_so_far {encoding_type; _ } -> 
      match encoding_type with
      | Regular_field {Encoding_util.field_number; _ } -> max max_so_far field_number 
      | One_of {constructors; _ } -> 
          List.fold_left (fun max_so_far {encoding_type = {Encoding_util.field_number; _ } ; _ } -> 
          max field_number max_so_far 
      ) max_so_far constructors 
    ) (- 1) fields

  let gen_decode_record ?and_ ({record_name; fields } as record) = 
    concat [
      P.sprintf "%s decode_%s =" (let_decl_of_and and_) record_name;
      sp "%s" (add_indentation 1 @@ gen_mappings_record record); 
      sp "  in";
      sp "  (fun d ->"; 
      sp "    let a = decode d %s_mappings (Array.make %i []) in {" record_name (max_field_number fields + 1);
      add_indentation 3 @@ concat @@ List.map (fun field -> 
        let {
          encoding_type;
          field_type; 
          field_name; 
          type_qualifier;
        } = field in 
        match encoding_type with 
        | Regular_field {Encoding_util.field_number; _ } -> ( 
            let constructor = constructor_name (string_of_field_type No_qualifier field_type) in  
            match type_qualifier with
            | No_qualifier -> 
              sp "%s = required %i a (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
            | Option -> 
              sp "%s = optional %i a (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
            | List -> 
              sp "%s = list_ %i a (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
        )
        | One_of {constructors; variant_name} -> 
            let all_numbers = concat @@ List.map (fun {encoding_type= {Encoding_util.field_number; _ } ; _ } -> 
              (P.sprintf "%i;" field_number)
            ) constructors in 
            let all_numbers = concat ["["; all_numbers; "]"] in 
            sp "%s = (match oneof %s a with | `%s __v -> __v | _ -> e());"
              field_name all_numbers (constructor_name variant_name)
      ) fields;
      sp "    }";
      sp "  )";
    ]


  let gen_decode_const_variant ?and_ {variant_name; constructors; } = 
    concat [
      P.sprintf "%s decode_%s d = " (let_decl_of_and and_) variant_name; 
      sp "  match decode_varint_as_int d with";
      concat @@ List.map (fun (name, value) -> 
        sp "  | %i -> %s" value name
      ) constructors; 
      sp "  | _ -> failwith \"Unknown value for enum %s\"" variant_name; 
    ] 

  let gen_decode ?and_ = function 
    | Record r        -> Some (gen_decode_record ?and_ r)
    | Variant _       -> None
    | Const_variant v -> Some (gen_decode_const_variant ?and_ v)
  
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
      | Record {record_name ; _ } ->  Some (f record_name)
      | Variant _ -> None
      | Const_variant {variant_name; _ } -> Some (f variant_name)
  
  let gen_encode_record ?and_ {record_name; fields } = 
    L.log "gen_encode_record record_name: %s\n" record_name; 

    let gen_field ?indent v_name encoding_type field_type = 
      let {
        Encoding_util.field_number; 
        Encoding_util.payload_kind; 
        Encoding_util.nested} = encoding_type in 
      let s = concat [
        sp "Pc.Encoder.key (%i, Pc.%s) encoder; " 
          field_number (constructor_name @@ Encoding_util.string_of_payload_kind payload_kind);
        match field_type with 
        | User_defined_type t -> 
          if nested
          then 
            sp "Pc.Encoder.nested (encode_%s %s) encoder;" t v_name 
          else 
            sp "encode_%s %s encoder;" t v_name 
        | _ ->  
          sp "encode_%s_as_%s %s encoder;"
            (string_of_field_type No_qualifier field_type) 
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
        L.log "gen_code field_name: %s\n" field.field_name;

        let { encoding_type; field_type; field_name; type_qualifier ; } = field in 
        match encoding_type with 
        | Regular_field encoding_type -> ( 
          match type_qualifier with 
          | No_qualifier -> (
            let v_name = P.sprintf "v.%s" field_name in 
            gen_field v_name encoding_type field_type
          )
          | Option -> concat [
            sp "(match v.%s with " field_name;
            sp "| Some x -> (%s)"
            (gen_field ~indent:() "x" encoding_type field_type) ;
            sp "| None -> ());" ;
          ]
          | List -> concat [ 
            sp "List.iter (fun x -> ";
            gen_field ~indent:() "x" encoding_type field_type;
            sp ") v.%s;" field_name; 
          ]
        )
        | One_of {constructors; variant_name = _} -> (  
          concat [
            sp "(match v.%s with" field_name;
            concat @@ List.map (fun {encoding_type; field_type; field_name; type_qualifier= _ } ->
                let encode_field  = gen_field ~indent:() "x" encoding_type field_type in 
                sp "| %s x -> (%s\n)" field_name encode_field
            ) constructors;
            ");";
          ]
        )           (* one of        *)
      ) fields;  (* record fields *) 
    "\n  ()"
    ]

  let gen_encode_const_variant ?and_ {variant_name; constructors; } = 
    concat [
      P.sprintf "%s encode_%s v encoder =" (let_decl_of_and and_) variant_name; 
      sp "  match v with";
      concat @@ List.map (fun (name, value) -> 
        sp "  | %s -> encode_int_as_varint %i encoder" name value
      ) constructors; 
    ] 
  
  let gen_encode ?and_ = function 
    | Record r        -> Some (gen_encode_record  ?and_ r)
    | Variant _       -> None 
    | Const_variant v -> Some (gen_encode_const_variant ?and_ v)

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
    | Record {record_name ; _ } -> Some (f record_name)
    | Variant _ -> None
    | Const_variant {variant_name; _ } -> Some (f variant_name) 
  
  let gen_string_of_record  ?and_ {record_name; fields } = 
    L.log "gen_string_of, record_name: %s\n" record_name; 

    let gen_field field_name field_type = 
      match field_type with 
      | User_defined_type t -> 
        P.sprintf "P.sprintf \"\\n%s: %%s\" @@ string_of_%s x" field_name t  
      | _ ->  
        P.sprintf "P.sprintf \"\\n%s: %%%c\" x"  
          field_name 
          (printf_char_of_field_type field_type)
    in

    concat [
      P.sprintf "%s string_of_%s v = " (let_decl_of_and and_) record_name;
      "\n  add_indentation 1 @@ String.concat \"\" [";
      add_indentation 2 @@ concat @@ List.map (fun field -> 
        L.log "gen_string_of field_name: %s\n" field.field_name;
       
        let { field_type; field_name; type_qualifier ; encoding_type} = field in 
        match encoding_type with 
        | Regular_field _ -> ( 
          match type_qualifier with
          | No_qualifier -> 
            let field_string_of = gen_field field_name field_type in 
            sp "(let x = v.%s in %s);" field_name field_string_of 
          | Option -> 
            concat [
              sp "(match v.%s with " field_name;
              sp "| Some x -> (%s)"  (gen_field field_name field_type);
              sp "| None -> \"\\n%s: None\");" field_name;
            ]
          | List -> 
            concat [
              sp "String.concat \"\" @@ List.map (fun x ->";
              nl @@ gen_field field_name field_type; 
              sp ") v.%s;" field_name
            ]
        )
        | One_of {constructors; variant_name = _} -> (
          concat [
            sp "(match v.%s with" field_name;
            concat @@ List.map (fun {encoding_type=_; field_type; field_name;
            type_qualifier= _ } ->
              let field_string_of = gen_field field_name field_type in 
              sp "| %s x -> (%s)" field_name (add_indentation 1 field_string_of)
            ) constructors ;
            "\n);"       (* one of fields *) 
          ]
        )                (* one of        *)
      ) fields;          (* record fields *) 
      "\n  ]";
    ]

  let gen_string_of_const_variant ?and_ {variant_name; constructors; } = 
    concat [
      P.sprintf "%s string_of_%s v =" (let_decl_of_and and_) variant_name; 
      sp "  match v with";
      concat @@ List.map (fun (name, _ ) -> 
        sp "  | %s -> \"%s\"" name name
      ) constructors; 
    ] 

  let gen_string_of ?and_ = function 
    | Record r   -> Some (gen_string_of_record ?and_ r) 
    | Variant _  -> None
    | Const_variant v -> Some (gen_string_of_const_variant ?and_ v)

  let gen_string_of_sig t = 
    let f type_name =  
       concat [
         P.sprintf "val string_of_%s : %s -> string " type_name type_name;
         sp "(** [string_of_%s v] returns a debugging string for [v] *)" type_name;
       ]
    in 
    match t with 
    | Record {record_name ; _ } -> Some (f record_name)
    | Variant _ -> None
    | Const_variant {variant_name; _ ; } -> Some (f variant_name) 

end  
