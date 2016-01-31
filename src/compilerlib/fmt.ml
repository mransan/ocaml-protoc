type scope = {
  mutable items : item list; 
} 

and item = 
  | Line of string 
  | Scope of scope 

let empty_scope () = 
  {items = []} 

let line scope s =
  scope.items <- (Line s)::scope.items  

let scope scope f = 
  let sub_scope = empty_scope () in 
  f sub_scope; 
  scope.items <- (Scope sub_scope)::scope.items  

let indentation_prefix = function 
  | 0 -> ""
  | 1 -> "  "
  | 2 -> "    "
  | 3 -> "      "
  | 4 -> "        "
  | 5 -> "          "
  | 6 -> "            "
  | 7 -> "              "
  | 8 -> "                "
  | n -> (String.make n ' ')

let print scope = 

  let rec loop acc i = function
    | (Line s)::tl -> 
      loop ((indentation_prefix i ^ s)::acc) i tl  
    | (Scope {items})::tl -> 
      let sub = loop [] (i + 1) items in  
      loop (sub @ acc) i tl  
    | [] -> acc
  in 
  String.concat "\n" @@ loop [] 0 scope.items 
