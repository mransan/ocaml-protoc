
let __log__ = ref None 

let setup_from_out_channel oc = 
  __log__ := Some oc 

let log (x:('a, out_channel, unit) format) = 
  match !__log__ with
  | None    -> Printf.ifprintf stdout x 
  | Some oc -> Printf.fprintf  oc     x 


let endline s = log "%s\n" s 
