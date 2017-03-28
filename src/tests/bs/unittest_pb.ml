[@@@ocaml.warning "-27-30-39"]

type all_basic_types = {
  field01 : float;
  field02 : float;
  field03 : int32;
  field04 : int64;
  field05 : int32;
  field06 : int64;
  field07 : int32;
  field08 : int64;
  field09 : int32;
  field10 : int64;
  field13 : bool;
  field14 : string;
  repeated01 : float list;
  repeated02 : float list;
  repeated03 : int32 list;
  repeated04 : int64 list;
  repeated05 : int32 list;
  repeated06 : int64 list;
  repeated07 : int32 list;
  repeated08 : int64 list;
  repeated09 : int32 list;
  repeated10 : int64 list;
  repeated13 : bool list;
  repeated14 : string list;
}

and all_basic_types_mutable = {
  mutable field01 : float;
  mutable field02 : float;
  mutable field03 : int32;
  mutable field04 : int64;
  mutable field05 : int32;
  mutable field06 : int64;
  mutable field07 : int32;
  mutable field08 : int64;
  mutable field09 : int32;
  mutable field10 : int64;
  mutable field13 : bool;
  mutable field14 : string;
  mutable repeated01 : float list;
  mutable repeated02 : float list;
  mutable repeated03 : int32 list;
  mutable repeated04 : int64 list;
  mutable repeated05 : int32 list;
  mutable repeated06 : int64 list;
  mutable repeated07 : int32 list;
  mutable repeated08 : int64 list;
  mutable repeated09 : int32 list;
  mutable repeated10 : int64 list;
  mutable repeated13 : bool list;
  mutable repeated14 : string list;
}

type small_message = {
  sm_string : string;
}

and small_message_mutable = {
  mutable sm_string : string;
}

type enum =
  | Value0 
  | Value1 
  | Value_two 

type single_one_of =
  | String_value of string
  | Int_value of int32
  | Enum_value of enum
  | Small_message of small_message
  | Recursive_value of single_one_of

type test = {
  all_basic_types : all_basic_types option;
  test_enum0 : enum;
  test_enum1 : enum;
  test_enum2 : enum;
  single_one_of_string : single_one_of option;
  single_one_of_int : single_one_of option;
  single_one_of_enum : single_one_of option;
  single_one_of_small_message : single_one_of option;
  single_one_of_recursive : single_one_of option;
}

and test_mutable = {
  mutable all_basic_types : all_basic_types option;
  mutable test_enum0 : enum;
  mutable test_enum1 : enum;
  mutable test_enum2 : enum;
  mutable single_one_of_string : single_one_of option;
  mutable single_one_of_int : single_one_of option;
  mutable single_one_of_enum : single_one_of option;
  mutable single_one_of_small_message : single_one_of option;
  mutable single_one_of_recursive : single_one_of option;
}

let rec default_all_basic_types 
  ?field01:((field01:float) = 0.)
  ?field02:((field02:float) = 0.)
  ?field03:((field03:int32) = 0l)
  ?field04:((field04:int64) = 0L)
  ?field05:((field05:int32) = 0l)
  ?field06:((field06:int64) = 0L)
  ?field07:((field07:int32) = 0l)
  ?field08:((field08:int64) = 0L)
  ?field09:((field09:int32) = 0l)
  ?field10:((field10:int64) = 0L)
  ?field13:((field13:bool) = false)
  ?field14:((field14:string) = "")
  ?repeated01:((repeated01:float list) = [])
  ?repeated02:((repeated02:float list) = [])
  ?repeated03:((repeated03:int32 list) = [])
  ?repeated04:((repeated04:int64 list) = [])
  ?repeated05:((repeated05:int32 list) = [])
  ?repeated06:((repeated06:int64 list) = [])
  ?repeated07:((repeated07:int32 list) = [])
  ?repeated08:((repeated08:int64 list) = [])
  ?repeated09:((repeated09:int32 list) = [])
  ?repeated10:((repeated10:int64 list) = [])
  ?repeated13:((repeated13:bool list) = [])
  ?repeated14:((repeated14:string list) = [])
  () : all_basic_types  = {
  field01;
  field02;
  field03;
  field04;
  field05;
  field06;
  field07;
  field08;
  field09;
  field10;
  field13;
  field14;
  repeated01;
  repeated02;
  repeated03;
  repeated04;
  repeated05;
  repeated06;
  repeated07;
  repeated08;
  repeated09;
  repeated10;
  repeated13;
  repeated14;
}

and default_all_basic_types_mutable () : all_basic_types_mutable = {
  field01 = 0.;
  field02 = 0.;
  field03 = 0l;
  field04 = 0L;
  field05 = 0l;
  field06 = 0L;
  field07 = 0l;
  field08 = 0L;
  field09 = 0l;
  field10 = 0L;
  field13 = false;
  field14 = "";
  repeated01 = [];
  repeated02 = [];
  repeated03 = [];
  repeated04 = [];
  repeated05 = [];
  repeated06 = [];
  repeated07 = [];
  repeated08 = [];
  repeated09 = [];
  repeated10 = [];
  repeated13 = [];
  repeated14 = [];
}

let rec default_small_message 
  ?sm_string:((sm_string:string) = "")
  () : small_message  = {
  sm_string;
}

and default_small_message_mutable () : small_message_mutable = {
  sm_string = "";
}

let rec default_enum () = (Value0:enum)

let rec default_single_one_of () : single_one_of = String_value ("")

let rec default_test 
  ?all_basic_types:((all_basic_types:all_basic_types option) = None)
  ?test_enum0:((test_enum0:enum) = default_enum ())
  ?test_enum1:((test_enum1:enum) = default_enum ())
  ?test_enum2:((test_enum2:enum) = default_enum ())
  ?single_one_of_string:((single_one_of_string:single_one_of option) = None)
  ?single_one_of_int:((single_one_of_int:single_one_of option) = None)
  ?single_one_of_enum:((single_one_of_enum:single_one_of option) = None)
  ?single_one_of_small_message:((single_one_of_small_message:single_one_of option) = None)
  ?single_one_of_recursive:((single_one_of_recursive:single_one_of option) = None)
  () : test  = {
  all_basic_types;
  test_enum0;
  test_enum1;
  test_enum2;
  single_one_of_string;
  single_one_of_int;
  single_one_of_enum;
  single_one_of_small_message;
  single_one_of_recursive;
}

and default_test_mutable () : test_mutable = {
  all_basic_types = None;
  test_enum0 = default_enum ();
  test_enum1 = default_enum ();
  test_enum2 = default_enum ();
  single_one_of_string = None;
  single_one_of_int = None;
  single_one_of_enum = None;
  single_one_of_small_message = None;
  single_one_of_recursive = None;
}

let rec decode_all_basic_types json =
  let v = default_all_basic_types_mutable () in
  let keys = Js_dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "field01" -> 
      let json = Js_dict.unsafeGet json "field01" in
      v.field01 <- Pbrt_bs.float json "all_basic_types" "field01"
    | "field02" -> 
      let json = Js_dict.unsafeGet json "field02" in
      v.field02 <- Pbrt_bs.float json "all_basic_types" "field02"
    | "field03" -> 
      let json = Js_dict.unsafeGet json "field03" in
      v.field03 <- Pbrt_bs.int32 json "all_basic_types" "field03"
    | "field04" -> 
      let json = Js_dict.unsafeGet json "field04" in
      v.field04 <- Pbrt_bs.int64 json "all_basic_types" "field04"
    | "field05" -> 
      let json = Js_dict.unsafeGet json "field05" in
      v.field05 <- Pbrt_bs.int32 json "all_basic_types" "field05"
    | "field06" -> 
      let json = Js_dict.unsafeGet json "field06" in
      v.field06 <- Pbrt_bs.int64 json "all_basic_types" "field06"
    | "field07" -> 
      let json = Js_dict.unsafeGet json "field07" in
      v.field07 <- Pbrt_bs.int32 json "all_basic_types" "field07"
    | "field08" -> 
      let json = Js_dict.unsafeGet json "field08" in
      v.field08 <- Pbrt_bs.int64 json "all_basic_types" "field08"
    | "field09" -> 
      let json = Js_dict.unsafeGet json "field09" in
      v.field09 <- Pbrt_bs.int32 json "all_basic_types" "field09"
    | "field10" -> 
      let json = Js_dict.unsafeGet json "field10" in
      v.field10 <- Pbrt_bs.int64 json "all_basic_types" "field10"
    | "field13" -> 
      let json = Js_dict.unsafeGet json "field13" in
      v.field13 <- Pbrt_bs.bool json "all_basic_types" "field13"
    | "field14" -> 
      let json = Js_dict.unsafeGet json "field14" in
      v.field14 <- Pbrt_bs.string json "all_basic_types" "field14"
    | "repeated01" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated01" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated01"
      in
      v.repeated01 <- Array.map (fun json -> 
        Pbrt_bs.float json "all_basic_types" "repeated01"
      ) a |> Array.to_list;
    end
    | "repeated02" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated02" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated02"
      in
      v.repeated02 <- Array.map (fun json -> 
        Pbrt_bs.float json "all_basic_types" "repeated02"
      ) a |> Array.to_list;
    end
    | "repeated03" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated03" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated03"
      in
      v.repeated03 <- Array.map (fun json -> 
        Pbrt_bs.int32 json "all_basic_types" "repeated03"
      ) a |> Array.to_list;
    end
    | "repeated04" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated04" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated04"
      in
      v.repeated04 <- Array.map (fun json -> 
        Pbrt_bs.int64 json "all_basic_types" "repeated04"
      ) a |> Array.to_list;
    end
    | "repeated05" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated05" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated05"
      in
      v.repeated05 <- Array.map (fun json -> 
        Pbrt_bs.int32 json "all_basic_types" "repeated05"
      ) a |> Array.to_list;
    end
    | "repeated06" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated06" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated06"
      in
      v.repeated06 <- Array.map (fun json -> 
        Pbrt_bs.int64 json "all_basic_types" "repeated06"
      ) a |> Array.to_list;
    end
    | "repeated07" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated07" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated07"
      in
      v.repeated07 <- Array.map (fun json -> 
        Pbrt_bs.int32 json "all_basic_types" "repeated07"
      ) a |> Array.to_list;
    end
    | "repeated08" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated08" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated08"
      in
      v.repeated08 <- Array.map (fun json -> 
        Pbrt_bs.int64 json "all_basic_types" "repeated08"
      ) a |> Array.to_list;
    end
    | "repeated09" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated09" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated09"
      in
      v.repeated09 <- Array.map (fun json -> 
        Pbrt_bs.int32 json "all_basic_types" "repeated09"
      ) a |> Array.to_list;
    end
    | "repeated10" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated10" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated10"
      in
      v.repeated10 <- Array.map (fun json -> 
        Pbrt_bs.int64 json "all_basic_types" "repeated10"
      ) a |> Array.to_list;
    end
    | "repeated13" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated13" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated13"
      in
      v.repeated13 <- Array.map (fun json -> 
        Pbrt_bs.bool json "all_basic_types" "repeated13"
      ) a |> Array.to_list;
    end
    | "repeated14" -> begin
      let a = 
        let a = Js_dict.unsafeGet json "repeated14" in 
        Pbrt_bs.array_ a "all_basic_types" "repeated14"
      in
      v.repeated14 <- Array.map (fun json -> 
        Pbrt_bs.string json "all_basic_types" "repeated14"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    field01 = v.field01;
    field02 = v.field02;
    field03 = v.field03;
    field04 = v.field04;
    field05 = v.field05;
    field06 = v.field06;
    field07 = v.field07;
    field08 = v.field08;
    field09 = v.field09;
    field10 = v.field10;
    field13 = v.field13;
    field14 = v.field14;
    repeated01 = v.repeated01;
    repeated02 = v.repeated02;
    repeated03 = v.repeated03;
    repeated04 = v.repeated04;
    repeated05 = v.repeated05;
    repeated06 = v.repeated06;
    repeated07 = v.repeated07;
    repeated08 = v.repeated08;
    repeated09 = v.repeated09;
    repeated10 = v.repeated10;
    repeated13 = v.repeated13;
    repeated14 = v.repeated14;
  } : all_basic_types)

let rec decode_small_message json =
  let v = default_small_message_mutable () in
  let keys = Js_dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "smString" -> 
      let json = Js_dict.unsafeGet json "smString" in
      v.sm_string <- Pbrt_bs.string json "small_message" "sm_string"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    sm_string = v.sm_string;
  } : small_message)

let rec decode_enum (json:Js_json.t) =
  match Pbrt_bs.string json "enum" "value" with
  | "VALUE0" -> Value0
  | "VALUE1" -> Value1
  | "VALUE_TWO" -> Value_two
  | "" -> Value0
  | _ -> Pbrt_json.E.malformed_variant "enum"

let rec decode_single_one_of json =
  let keys = Js_dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_json.E.malformed_variant "single_one_of"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "stringValue" -> 
        let json = Js_dict.unsafeGet json "stringValue" in
        String_value (Pbrt_bs.string json "single_one_of" "String_value")
      | "intValue" -> 
        let json = Js_dict.unsafeGet json "intValue" in
        Int_value (Pbrt_bs.int32 json "single_one_of" "Int_value")
      | "enumValue" -> 
        let json = Js_dict.unsafeGet json "enumValue" in
        Enum_value ((decode_enum json))
      | "smallMessage" -> 
        let json = Js_dict.unsafeGet json "smallMessage" in
        Small_message ((decode_small_message (Pbrt_bs.object_ json "single_one_of" "Small_message")))
      | "recursiveValue" -> 
        let json = Js_dict.unsafeGet json "recursiveValue" in
        Recursive_value ((decode_single_one_of (Pbrt_bs.object_ json "single_one_of" "Recursive_value")))
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_test json =
  let v = default_test_mutable () in
  let keys = Js_dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "allBasicTypes" -> 
      let json = Js_dict.unsafeGet json "allBasicTypes" in
      v.all_basic_types <- Some ((decode_all_basic_types (Pbrt_bs.object_ json "test" "all_basic_types")))
    | "testEnum0" -> 
      let json = Js_dict.unsafeGet json "testEnum0" in
      v.test_enum0 <- (decode_enum json)
    | "testEnum1" -> 
      let json = Js_dict.unsafeGet json "testEnum1" in
      v.test_enum1 <- (decode_enum json)
    | "testEnum2" -> 
      let json = Js_dict.unsafeGet json "testEnum2" in
      v.test_enum2 <- (decode_enum json)
    | "singleOneOfString" -> 
      let json = Js_dict.unsafeGet json "singleOneOfString" in
      v.single_one_of_string <- Some ((decode_single_one_of (Pbrt_bs.object_ json "test" "single_one_of_string")))
    | "singleOneOfInt" -> 
      let json = Js_dict.unsafeGet json "singleOneOfInt" in
      v.single_one_of_int <- Some ((decode_single_one_of (Pbrt_bs.object_ json "test" "single_one_of_int")))
    | "singleOneOfEnum" -> 
      let json = Js_dict.unsafeGet json "singleOneOfEnum" in
      v.single_one_of_enum <- Some ((decode_single_one_of (Pbrt_bs.object_ json "test" "single_one_of_enum")))
    | "singleOneOfSmallMessage" -> 
      let json = Js_dict.unsafeGet json "singleOneOfSmallMessage" in
      v.single_one_of_small_message <- Some ((decode_single_one_of (Pbrt_bs.object_ json "test" "single_one_of_small_message")))
    | "singleOneOfRecursive" -> 
      let json = Js_dict.unsafeGet json "singleOneOfRecursive" in
      v.single_one_of_recursive <- Some ((decode_single_one_of (Pbrt_bs.object_ json "test" "single_one_of_recursive")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    all_basic_types = v.all_basic_types;
    test_enum0 = v.test_enum0;
    test_enum1 = v.test_enum1;
    test_enum2 = v.test_enum2;
    single_one_of_string = v.single_one_of_string;
    single_one_of_int = v.single_one_of_int;
    single_one_of_enum = v.single_one_of_enum;
    single_one_of_small_message = v.single_one_of_small_message;
    single_one_of_recursive = v.single_one_of_recursive;
  } : test)

let rec encode_all_basic_types (v:all_basic_types) json = 
  Js_dict.set json "field01" (Js_json.string (string_of_float v.field01));
  Js_dict.set json "field02" (Js_json.number v.field02);
  Js_dict.set json "field03" (Js_json.number (Int32.to_float v.field03));
  Js_dict.set json "field04" (Js_json.string (Int64.to_string v.field04));
  Js_dict.set json "field05" (Js_json.number (Int32.to_float v.field05));
  Js_dict.set json "field06" (Js_json.string (Int64.to_string v.field06));
  Js_dict.set json "field07" (Js_json.number (Int32.to_float v.field07));
  Js_dict.set json "field08" (Js_json.string (Int64.to_string v.field08));
  Js_dict.set json "field09" (Js_json.number (Int32.to_float v.field09));
  Js_dict.set json "field10" (Js_json.string (Int64.to_string v.field10));
  Js_dict.set json "field13" (Js_json.boolean (Js_boolean.to_js_boolean v.field13));
  Js_dict.set json "field14" (Js_json.string v.field14);
  let a = v.repeated01 |> List.map string_of_float |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated01" (Js_json.array_ a);
  let a = v.repeated02 |> Array.of_list |> Array.map Js_json.number in
  Js_dict.set json "repeated02" (Js_json.array_ a);
  let a = v.repeated03 |> List.map Int32.to_float |> Array.of_list |> Array.map Js_json.number in
  Js_dict.set json "repeated03" (Js_json.array_ a);
  let a = v.repeated04 |> List.map Int64.to_string |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated04" (Js_json.array_ a);
  let a = v.repeated05 |> List.map Int32.to_float |> Array.of_list |> Array.map Js_json.number in
  Js_dict.set json "repeated05" (Js_json.array_ a);
  let a = v.repeated06 |> List.map Int64.to_string |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated06" (Js_json.array_ a);
  let a = v.repeated07 |> List.map Int32.to_float |> Array.of_list |> Array.map Js_json.number in
  Js_dict.set json "repeated07" (Js_json.array_ a);
  let a = v.repeated08 |> List.map Int64.to_string |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated08" (Js_json.array_ a);
  let a = v.repeated09 |> List.map Int32.to_float |> Array.of_list |> Array.map Js_json.number in
  Js_dict.set json "repeated09" (Js_json.array_ a);
  let a = v.repeated10 |> List.map Int64.to_string |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated10" (Js_json.array_ a);
  let a = v.repeated13 |> List.map Js_boolean.to_js_boolean |> Array.of_list |> Array.map Js_json.boolean in
  Js_dict.set json "repeated13" (Js_json.array_ a);
  let a = v.repeated14 |> Array.of_list |> Array.map Js_json.string in
  Js_dict.set json "repeated14" (Js_json.array_ a);
  ()

let rec encode_small_message (v:small_message) json = 
  Js_dict.set json "smString" (Js_json.string v.sm_string);
  ()

let rec encode_enum (v:enum) : string = 
  match v with
  | Value0 -> "VALUE0"
  | Value1 -> "VALUE1"
  | Value_two -> "VALUE_TWO"

let rec encode_single_one_of (v:single_one_of) json = 
  begin match v with
    | String_value v ->
    Js_dict.set json "stringValue" (Js_json.string v);
    | Int_value v ->
    Js_dict.set json "intValue" (Js_json.number (Int32.to_float v));
    | Enum_value v ->
    Js_dict.set json "enumValue" (Js_json.string (encode_enum v));
    | Small_message v ->
    begin (* smallMessage field *)
      let json' = Js_dict.empty () in
      encode_small_message v json';
      Js_dict.set json "smallMessage" (Js_json.object_ json');
    end;
    | Recursive_value v ->
    begin (* recursiveValue field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "recursiveValue" (Js_json.object_ json');
    end;
  end

let rec encode_test (v:test) json = 
  begin match v.all_basic_types with
    | None -> ()
    | Some v ->
    begin (* allBasicTypes field *)
      let json' = Js_dict.empty () in
      encode_all_basic_types v json';
      Js_dict.set json "allBasicTypes" (Js_json.object_ json');
    end;
  end;
  Js_dict.set json "testEnum0" (Js_json.string (encode_enum v.test_enum0));
  Js_dict.set json "testEnum1" (Js_json.string (encode_enum v.test_enum1));
  Js_dict.set json "testEnum2" (Js_json.string (encode_enum v.test_enum2));
  begin match v.single_one_of_string with
    | None -> ()
    | Some v ->
    begin (* singleOneOfString field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "singleOneOfString" (Js_json.object_ json');
    end;
  end;
  begin match v.single_one_of_int with
    | None -> ()
    | Some v ->
    begin (* singleOneOfInt field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "singleOneOfInt" (Js_json.object_ json');
    end;
  end;
  begin match v.single_one_of_enum with
    | None -> ()
    | Some v ->
    begin (* singleOneOfEnum field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "singleOneOfEnum" (Js_json.object_ json');
    end;
  end;
  begin match v.single_one_of_small_message with
    | None -> ()
    | Some v ->
    begin (* singleOneOfSmallMessage field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "singleOneOfSmallMessage" (Js_json.object_ json');
    end;
  end;
  begin match v.single_one_of_recursive with
    | None -> ()
    | Some v ->
    begin (* singleOneOfRecursive field *)
      let json' = Js_dict.empty () in
      encode_single_one_of v json';
      Js_dict.set json "singleOneOfRecursive" (Js_json.object_ json');
    end;
  end;
  ()
