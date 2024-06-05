module type T_sig = Ocaml_test_runner.T_sig

module Bench_t = Benchmark

module Util = struct
  let shuffle_array a n =
    let len = Array.length a in
    for _ = 1 to n do
      let pos = Random.int len in
      let pos' = Random.int len in
      let tmp = a.(pos) in
      a.(pos) <- a.(pos');
      a.(pos') <- tmp
    done

  let rec append_n l v = function
    | 0 -> l
    | n -> append_n (v :: l) v (n - 1)

  let map_filter f l =
    List.fold_right
      (fun e acc ->
        match f e with
        | Some x -> x :: acc
        | None -> acc)
      l []
end
(* Util *)

let process_request ({ Bench_t.test_id; _ } as request : Bench_t.test_request) =
  let module Test = (val Ocaml_test_types.get_test test_id : T_sig) in
  let module Runner = Ocaml_test_runner.Make (Test) in
  Runner.run request

let string_of_test_id = function
  | Bench_t.Int_list -> "int_list"
  | Bench_t.Int_repeated -> "int_repeated"
  | Bench_t.Int_packed_repeated -> "int_packed_repeated"
  | Bench_t.Int32_list -> "int32_list"

let make_test_requests ~number_of_sample test_ids =
  let rec aux test_id acc = function
    | 0 -> acc
    | difficulty ->
      let file_name =
        Printf.sprintf "test_%s_%i.data" (string_of_test_id test_id) difficulty
      in
      let encode =
        Bench_t.{ type_ = { t = Some (Encode difficulty) }; file_name; test_id }
      in
      let decode =
        Bench_t.{ type_ = { t = Some Decode }; file_name; test_id }
      in

      let init_cases, test_cases = acc in
      let test_cases = Util.append_n test_cases encode number_of_sample in
      let test_cases = Util.append_n test_cases decode number_of_sample in

      aux test_id (encode :: init_cases, test_cases) (difficulty - 1)
  in
  let max_difficulty = 5 in
  let init_cases, test_cases =
    List.fold_left
      (fun acc test_id -> aux test_id acc max_difficulty)
      ([], []) test_ids
  in
  let test_cases = Array.of_list test_cases in
  Util.shuffle_array test_cases
    (number_of_sample * 2 * 5 * 10 * List.length test_ids);
  Array.of_list init_cases, test_cases

(** Prints the test results for a single metric and a list of test id, 
    in a table format so that one can easily compare the performance 
    of various test data structures. 

    For instance:

    Size     Test 1,    Test 2,     Test i, 
      10      0.001,     0.001,      0.002, 
     100      0.010,     0.010,      0.020,

 *)
let print_compare_type ~metric responses test_ids =
  (* Organize response per difficulty then per test id:
   
   * (difficulty, (test_id, test_response_data Stack.t) Hashtbl.t) Hashtbl.t 
   *)
  let responses_per_difficulty = Hashtbl.create 10 in

  (* Utility fuction to insert a test response into the 
   * container organized per difficult and per test 
   *)
  let add_response { Bench_t.difficulty_size; Bench_t.test_id; Bench_t.data } =
    match Hashtbl.find responses_per_difficulty difficulty_size with
    | test_id_responses ->
      let data_stack =
        match Hashtbl.find test_id_responses test_id with
        | data_stack -> data_stack
        | exception Not_found ->
          let data_stack = Stack.create () in
          Hashtbl.add test_id_responses test_id data_stack;
          data_stack
      in
      Stack.push data data_stack
    | exception Not_found ->
      let test_id_responses = Hashtbl.create 10 in
      let data_stack = Stack.create () in
      Stack.push data data_stack;
      Hashtbl.add test_id_responses test_id data_stack;
      Hashtbl.add responses_per_difficulty difficulty_size test_id_responses
  in

  (* Utility to get the test data in Stack data structure based on 
   * difficulty_size and test_id. 
   *)
  let get_data_stack difficulty_size test_id =
    let test_id_responses =
      Hashtbl.find responses_per_difficulty difficulty_size
    in
    match Hashtbl.find test_id_responses test_id with
    | data_stack -> data_stack
    | exception Not_found ->
      failwith
        (Printf.sprintf "No test data found for test_id: %s"
           (string_of_test_id test_id))
  in

  (* Find all the unique difficulties
   *)
  let all_difficulties =
    List.fold_left
      (fun all_difficulties response ->
        add_response response;
        response.Bench_t.difficulty_size :: all_difficulties)
      [] responses
  in
  let all_difficulties = List.sort_uniq compare all_difficulties in

  Printf.printf "%10s: " "Size";
  List.iter
    (fun test_id -> Printf.printf "  %20s," (string_of_test_id test_id))
    test_ids;
  print_newline ();
  List.iter
    (fun difficulty_size ->
      Printf.printf "%10i: " difficulty_size;
      List.iter
        (fun test_id ->
          let data_stack = get_data_stack difficulty_size test_id in

          let sum = ref 0. in
          let counter = ref 0 in
          Stack.iter
            (fun data ->
              let open Bench_t in
              match data, metric with
              | Some (Encode { encode_time; _ }), `Encode_time ->
                sum := !sum +. encode_time;
                incr counter
              | Some (Decode { decode_time; _ }), `Decode_time ->
                sum := !sum +. decode_time;
                incr counter
              | _ -> ())
            data_stack;
          let avg = !sum /. float_of_int !counter in
          Printf.printf "  %20.5f," avg)
        test_ids;
      print_newline ())
    all_difficulties;
  ()

let () =
  Random.self_init ();

  let all_test_ids =
    [
      Bench_t.Int32_list;
      Bench_t.Int_list;
      Bench_t.Int_repeated;
      Bench_t.Int_packed_repeated;
    ]
  in

  let init_cases, test_cases =
    make_test_requests ~number_of_sample:10 all_test_ids
  in

  Printf.printf "Number of test cases: %i\n" (Array.length test_cases);

  (* Initialization which makes sure that at least one `encode` for each
   * test and each difficulty is run so that the file is present
   * on disk in case the shuffling of tests puts a `decode` prior to 
   * an `encode`.
   *)
  Array.iter (fun r -> ignore @@ process_request r) init_cases;

  let responses =
    Array.fold_left
      (fun responses request -> process_request request :: responses)
      [] test_cases
  in

  print_compare_type ~metric:`Decode_time responses all_test_ids
