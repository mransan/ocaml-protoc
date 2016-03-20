
let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let get_binary_file_content file_name = 
  let t, (ic, s) = time_f (fun () -> 
    let ic     = open_in_bin file_name in 
    let s      = in_channel_length ic in 
    ic, s
  ) in 

  let t', b = time_f (fun () -> 
    Bytes.create s 
  ) in 

  Printf.printf "binary file size: %i in %f, %f \n%!" s t t'  ;
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

let string_of_pp f_pp v =
  Format.fprintf Format.str_formatter "@[%a@]" f_pp v;
  Format.flush_str_formatter () 

let check_decoded ?noprint x f_pp ref_data = 
  if  x = ref_data 
  then (
    print_endline "ML: -- Good --"; 
    match noprint with 
    | None -> (
      print_endline "-- [ML Debug Start] :\n";
      print_endline @@ string_of_pp f_pp x; 
      print_endline "-- [ML Debug End] :\n"
    )
    | Some () -> () ;
    exit 0
  )
  else (
    print_endline "ML: -- Test Failed --";  
    begin 
      match noprint with
      | None -> (
          Printf.eprintf "decoded : %s \n" @@ string_of_pp f_pp x;
          Printf.eprintf "expected: %s \n" @@ string_of_pp f_pp ref_data;
      )
      | Some () -> (); 
    end; 
    exit 1
  )

let decode ?noprint ?notest file_name f_decode f_pp ref_data  = 
  let buffer, size = get_binary_file_content file_name in 
  Printf.printf "Done reading data, size=%i\n%!" size ;

  (*
  let buffer = Bytes.sub buffer 0 size in 
  *)
  let decoder = Pbrt.Decoder.of_bytes buffer in 
  
  let t, x = time_f (fun () -> 
    f_decode decoder 
  ) in 
  Printf.printf "Decode : %f \n%!" t; 
  begin 
    match notest with
    | None    -> check_decoded ?noprint x f_pp ref_data 
    | Some () -> ()
  end 

let encode file_name f_encode ref_data = 
  let encoder = Pbrt.Encoder.create () in 
  f_encode ref_data encoder; 
  let oc = open_out_bin file_name in 
  output_bytes oc @@ Pbrt.Encoder.to_bytes encoder 
