module Bench_t    = Benchmark_pb
module type T_sig = Ocaml_test_runner.T_sig  

module IntList = struct 

  type t = Bench_t.int_list 
  
  let rec make_impl acc = function 
    | 0 -> acc 
    | n -> make_impl (n :: acc) (n-1) 

  let make difficulty  =
    let difficulty_size = match difficulty with
      | 1  ->        100
      | 2  ->       1000
      | 3  ->     50_000
      | 4  ->  1_000_000
      | 5  -> 10_000_000
      | _  -> failwith "Invalid difficulty"
    in
    let difficulty_size = difficulty_size  in
    (Bench_t.({int_list = make_impl [] difficulty_size}) , difficulty_size)

  let difficulty_size {Bench_t.int_list} = List.length int_list

  let encode = Bench_t.encode_int_list 

  let decode = Bench_t.decode_int_list

end  

module IntRepeated = struct 

  type t = Bench_t.int_repeated
  
  let rec make_impl n = 
    let r = Pbrt.Repeated_field.make 0 in 
    let rec aux = function
      | 0 -> r
      | n -> Pbrt.Repeated_field.add n r;  aux (n-1) 
    in
    aux n

  let make difficulty  =
    let difficulty_size = match difficulty with
      | 1  ->        100
      | 2  ->       1000
      | 3  ->     50_000
      | 4  ->  1_000_000
      | 5  -> 10_000_000
      | _  -> failwith "Invalid difficulty"
    in
    let difficulty_size = difficulty_size  in
    (Bench_t.({int_repeated= make_impl difficulty_size}) , difficulty_size)

  let difficulty_size {Bench_t.int_repeated} = Pbrt.Repeated_field.length int_repeated

  let encode = Bench_t.encode_int_repeated

  let decode = Bench_t.decode_int_repeated

end  

module IntPackedRepeated= struct 

  type t = Bench_t.int_packed_repeated
  
  let rec make_impl n = 
    let r = Pbrt.Repeated_field.make 0 in 
    let rec aux = function
      | 0 -> r
      | n -> Pbrt.Repeated_field.add n r;  aux (n-1) 
    in
    aux n

  let make difficulty  =
    let difficulty_size = match difficulty with
      | 1  ->        100
      | 2  ->       1000
      | 3  ->     50_000
      | 4  ->  1_000_000
      | 5  -> 10_000_000
      | _  -> failwith "Invalid difficulty"
    in
    let difficulty_size = difficulty_size  in
    let v = Bench_t.({
      int_packed_repeated = make_impl difficulty_size
     }) in
     v.Bench_t.int_packed_repeated;
     (v , difficulty_size)

  let difficulty_size {Bench_t.int_packed_repeated = l} = Pbrt.Repeated_field.length l

  let encode = Bench_t.encode_int_packed_repeated

  let decode = Bench_t.decode_int_packed_repeated

end  

let get_test = function 
  | Benchmark_pb.Int_list -> (module IntList : T_sig) 
  | Benchmark_pb.Int_repeated -> (module IntRepeated: T_sig) 
  | Benchmark_pb.Int_packed_repeated -> (module IntPackedRepeated: T_sig) 
