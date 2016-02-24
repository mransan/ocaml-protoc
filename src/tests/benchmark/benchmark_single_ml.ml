module type T_sig = Ocaml_test_runner.T_sig 
module Bench_t    = Benchmark_pb

module Util = struct 
  let shuffle_array a n = 
    let len = Array.length a in 
    for i=1 to n do 
      let pos = Random.int len in 
      let pos'= Random.int len in 
      let tmp = a.(pos) in 
      a.(pos)  <- a.(pos'); 
      a.(pos') <- tmp
    done
  
  let rec append_n l v = function
    | 0 -> l
    | n -> append_n (v::l)  v (n - 1)
  
  let map_filter f l =
    List.fold_right (fun e acc -> 
      match f e with
      | Some x -> x::acc
      | None -> acc 
    ) l [] 
end (* Util *)

let process_request ({Bench_t.test_id; _ } as request : Bench_t.test_request)  = 
  let module Test   = (val (Ocaml_test_types.get_test test_id):T_sig) in 
  let module Runner = Ocaml_test_runner.Make(Test) in  
  Runner.run request

let string_of_test_id = function
  | Bench_t.Int_list            -> "int_list"
  | Bench_t.Int_repeated        -> "int_repeated"
  | Bench_t.Int_packed_repeated -> "int_packed_repeated"

let make_test_requests nb_sample test_ids = 
  let rec aux test_id acc = function
    | 0 -> acc 
    | difficulty -> 
      let file_name = Printf.sprintf "test_%s_%i.data" 
        (string_of_test_id test_id) difficulty 
      in 
      let encode = Bench_t.({
        type_ = Encode difficulty; 
        file_name;
        test_id;
      }) in
      let decode = Bench_t.({
        type_ = Decode;
        file_name;
        test_id;
      }) in 

      let init_cases, test_cases = acc in 
      let test_cases = Util.append_n test_cases encode nb_sample in 
      let test_cases = Util.append_n test_cases decode nb_sample in 

      aux test_id (encode::init_cases, test_cases) (difficulty - 1)
  in
  let max_difficulty = 5 in 
  let init_cases, test_cases = List.fold_left (fun acc test_id -> 
      aux test_id acc max_difficulty
  ) ([], []) test_ids in 
  let test_cases = Array.of_list test_cases in 
  Util.shuffle_array test_cases (nb_sample * 2 * 5 * 10 * (List.length test_ids)) ;
  (Array.of_list init_cases, test_cases) 


let print_test_id responses test_id_to_print =  

  Printf.printf "\nTest result for [%s]\n%!" (string_of_test_id test_id_to_print);

  let decode_data = Util.map_filter (fun (response:Bench_t.test_response) -> 
    if response.Bench_t.test_id = test_id_to_print
    then 
      match response.Bench_t.data with
      | Bench_t.Decode x -> Some (response.Bench_t.difficulty_size, x) 
      | Bench_t.Encode _ -> None
    else None 
  ) responses in 

  let all_difficulties = List.sort_uniq Pervasives.compare @@ List.map fst decode_data in 

  List.iter (fun difficulty_size_to_print ->
    let decode_data = List.filter (fun (difficulty_size, _ ) -> 
      difficulty_size_to_print = difficulty_size
    ) decode_data in

    let n = float_of_int @@ List.length decode_data in 

    let avg_decode_time = List.fold_left (fun avg (_, {Bench_t.decode_time; _ }) -> 
      avg +. (decode_time /. n)
    ) 0. decode_data in 

    Printf.printf "%15i : %10.5f \n%!" difficulty_size_to_print avg_decode_time
  ) all_difficulties

let () = 

  Random.self_init ();

  let all_test_ids = [
    Bench_t.Int_list;
    Bench_t.Int_repeated;
    Bench_t.Int_packed_repeated;
  ] in

  let init_cases, test_cases = make_test_requests 5 all_test_ids in
  
  Printf.printf "Number of test cases: %i\n" (Array.length test_cases);
  Array.iter (fun r -> ignore @@ process_request r) init_cases;
  
  let responses = Array.fold_left (fun responses request -> 
      (process_request request)::responses
    ) [] test_cases
  in

  List.iter (print_test_id responses) all_test_ids 

