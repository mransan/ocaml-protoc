open Yojson

type json = Basic.json 

module type Encoder_sig = 
    Pbrt_js.Encoder_sig with type t = (string * json) list ref

module Encoder : Encoder_sig = struct

  type t = (string * json) list ref 

  let empty () = ref [] 

  let set_string t key value = 
    t := (key, `String value) :: !t
  
  let set_float t key value = 
    t := (key, `Float value) :: !t
  
  let set_int t key value = 
    t := (key, `Int value) :: !t
  
  let set_bool t key value = 
    t := (key, `Bool value) :: !t
  
  let set_object t key value = 
    t := (key, `Assoc !value) :: !t

  let set_string_list t key value = 
    t := (key, `List (List.map (fun v -> `String v) value)) :: !t
  
  let set_float_list t key value = 
    t := (key, `List (List.map (fun v -> `Float v) value)) :: !t
  
  let set_int_list t key value = 
    t := (key, `List (List.map (fun v -> `Int v) value)) :: !t
  
  let set_bool_list t key value = 
    t := (key, `List (List.map (fun v -> `Bool v) value)) :: !t
  
  let set_object_list t key value = 
    t := (key, `List (List.map (fun v -> `Assoc !v) value)) :: !t

end 


module type Decoder_sig = 
  Pbrt_js.Decoder_sig with type t = (string * json) list ref 

module Decoder : Decoder_sig = struct 

  type t = (string * json) list ref 
  
  type value = 
    | String of string 
    | Float of float 
    | Int of int 
    | Object of t 
    | Array_as_array of value array 
    | Bool of bool 
    | Null

  let rec map = function
    | `Null -> Null 
    | `Bool b -> Bool b 
    | `Int i -> Int i 
    | `Float f -> Float f 
    | `String s -> String s 
    | `Assoc a -> Object (ref a)
    | `List l -> Array_as_array (List.map map l |> Array.of_list)  

  let key t = 
    match !t with
    | [] -> None
    | (key, value)::tl -> begin
      t := tl; 
      Some (key, map value)
    end
end 

let () = 
  let t = Encoder.empty () in 
  Encoder.set_string t "name" "Maxime Ransan"; 
  Encoder.set_int t "age" 35; 
  Encoder.set_bool t "married" true;

  let json = `Assoc !t in 
  print_endline @@ Basic.to_string json;

  let name = ref "" in 
  let age = ref (-1) in 
  let married = ref false in 

  let continue = ref true in 
  while !continue do 
    let open Decoder in
    match key t with
    | None -> continue := false
    | Some ("name", String s) -> name := s
    | Some ("name", _ ) -> assert(false)
    | Some ("age", Int i) -> age := i
    | Some ("age", Float f) -> age := int_of_float f
    | Some ("age", String s) -> age := int_of_string s
    | Some ("age", _) -> assert(false)
    | Some ("married", Bool b) -> married := b
    | Some ("married", _) -> assert(false)
    | Some _ -> () (* skip *)
  done;

  assert(!name = "Maxime Ransan"); 
  assert(!age = 35);
  assert(!married); 
  ()
