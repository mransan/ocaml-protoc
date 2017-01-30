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

let rev_split_by_char c s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i c  in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  loop 0 []
  
let string_of_string_list l = 
  Printf.sprintf "[%s]" (String.concat "," l)

let string_fold_lefti f e0 s =
  let len = String.length s in 
  let rec loop acc = function
    | i when i = len -> acc 
    | i -> loop (f acc i (String.unsafe_get s i))  (i + 1) 
  in 
  loop e0 0 

module Option = struct 

  let default x = function
    | Some y -> y 
    | None -> x 

  let some v = Some v 
  
  let min_value x y = 
    match x, y with 
    | None   , None 
    | Some _ , None 
    | None   , Some _ -> invalid_arg "Util.Option.min_value"
    | Some x , Some y -> some @@ min x y  

  let eq_value x y = 
    match x, y with
    | None   , None 
    | Some _ , None 
    | None   , Some _ -> invalid_arg "Util.Option.eq_value"
    | Some x , Some y -> x = y

  let string_of_option f = function 
    | None -> "None"
    | Some x -> Printf.sprintf "Some(%s)" (f x)

end (* Option *)

let indentation_prefix =
  let h = Hashtbl.create 16 in 
  fun level  ->  
    match Hashtbl.find h level with 
    | s -> s 
    | exception Not_found -> 
      let s = String.make (2 * level) ' ' in 
      Hashtbl.add h level s; 
      s

let read_file file_name = 
  let ic = open_in file_name in 
  let len = in_channel_length ic in 
  let b = Bytes.create len in 
  let offset = ref 0 in 
  let remaining = ref len in 
  while !offset <> len do 
    let i = input ic b !offset ! remaining in 
    offset:=(!offset + i); 
    remaining:=(!remaining - i);
  done; 
  close_in ic;
  Bytes.to_string b 

module List = struct 
  let rec pop_last = function 
    | [] -> failwith "Invalid argument [] for pop_last"
    | _::[] -> []
    | hd::tl -> hd :: (pop_last tl)
  
  let rec apply_until f = function 
    | []  -> None 
    | hd::tl -> 
      begin match f hd with 
      | None -> apply_until f tl 
      | x    -> x
      end
  
  let rec filter_map f = function
    | [] -> []
    | hd::tl -> 
      begin match f hd with
      | None -> filter_map f tl 
      | Some x -> x :: (filter_map f tl)
      end  

end (* List *) 
