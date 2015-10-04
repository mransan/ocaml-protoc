
module Pc = Protobuf_codec 
let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let get_binary_file_content file_name = 
  let ic     = open_in_bin file_name in 
  let s      = in_channel_length ic in 
  Printf.printf "binary file size: %i \n%!" s ;
  let b      = Bytes.create s in 
  let t1     = Unix.gettimeofday () in 

  let offset = ref 0 in 
  let remaining = ref s in 
  while !offset <> s do 
    let i = input ic b !offset ! remaining in 
    offset:=(!offset + i); 
    remaining:=(!remaining - i);
  done; 
  let t2 = Unix.gettimeofday () in 
  let t  = (t2 -. t1) in 
  Printf.printf "Read file : %f \n%!" t; 
  b, s
  
type mode = 
  | Encode 
  | Decode 

let mode_of_string  = function 
  | "encode" -> Encode
  | "decode" -> Decode
  | _ -> failwith "Invalid mode, must be [encode|decode]"

let parse_args () = 
  let mode          = ref "" in  
  let cmd_line_args = [ ] in 
  let anon_fun  = (fun arg_mode -> 
    mode := arg_mode
  )  in 
  let usage = "test01_ml.tsk [encode|decode]" in  
  Arg.parse cmd_line_args anon_fun usage;
  mode_of_string !mode 

let decode ?noprint file_name f_decode f_to_string ref_data  = 
  let buffer, size = get_binary_file_content file_name in 
  Printf.printf "Done reading data, size=%i\n%!" size ;

  let buffer = Bytes.sub buffer 0 size in 
  let decoder = Pc.Decoder.of_bytes buffer in 
  
  let t, x = time_f (fun () -> 
    f_decode decoder 
  ) in 
  Printf.printf "Decode : %f \n%!" t; 
  if  x = ref_data 
  then (
    print_endline "ML: -- Good --"; 
    match noprint with 
    | None -> (
      print_endline "-- [ML Debug Start] :\n";
      print_endline @@ f_to_string x; 
      print_endline "-- [ML Debug End] :\n"
    )
    | Some _ -> () ;
    exit 0
  )
  else (
    print_endline "ML: -- Test Failed --";  
    match noprint with
    | None -> print_endline @@ f_to_string x
    | Some _ -> (); 
    exit 1
  )

let encode file_name f_encode ref_data = 
  let encoder = Pc.Encoder.create () in 
  f_encode ref_data encoder; 
  let oc = open_out_bin file_name in 
  output_bytes oc @@ Pc.Encoder.to_bytes encoder 
