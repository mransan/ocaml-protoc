module type T_sig = Ocaml_test_runner.T_sig  

let difficulty_size_of_difficulty = function 
  | 1  ->        100
  | 2  ->       1000
  | 3  ->     50_000
  | 4  ->  1_000_000
  | 5  -> 10_000_000
  | _  -> failwith "Invalid difficulty"

module Int32List = struct 

  type t = Benchmark_types.int32_list 
  
  let rec make_impl acc = function 
    | 0l -> acc 
    | n -> make_impl (n :: acc) (Int32.sub n 1l) 

  let make difficulty  =
    let difficulty_size = difficulty_size_of_difficulty difficulty in 
    (Benchmark_types.({int32_list = make_impl [] (Int32.of_int difficulty_size)}) , difficulty_size)

  let difficulty_size {Benchmark_types.int32_list} = List.length int32_list

  let encode = Benchmark_pb.encode_int32_list

  let decode = Benchmark_pb.decode_int32_list 

end  

module IntList = struct 

  type t = Benchmark_types.int_list 
  
  let rec make_impl acc = function 
    | 0 -> acc 
    | n -> make_impl (n :: acc) (n-1) 

  let make difficulty  =
    let difficulty_size = difficulty_size_of_difficulty difficulty in 
    (Benchmark_types.({int_list = make_impl [] difficulty_size}) , difficulty_size)

  let difficulty_size {Benchmark_types.int_list} = List.length int_list

  let encode = Benchmark_pb.encode_int_list 

  let decode = Benchmark_pb.decode_int_list

end  

module IntRepeated = struct 

  type t = Benchmark_types.int_repeated
  
  let make_impl n = 
    let r = Pbrt.Repeated_field.make 0 in 
    let rec aux = function
      | 0 -> r
      | n -> Pbrt.Repeated_field.add n r;  aux (n-1) 
    in
    aux n

  let make difficulty  =
    let difficulty_size = difficulty_size_of_difficulty difficulty in
    (Benchmark_types.({int_repeated= make_impl difficulty_size}) , difficulty_size)

  let difficulty_size {Benchmark_types.int_repeated} = Pbrt.Repeated_field.length int_repeated

  let encode = Benchmark_pb.encode_int_repeated

  let decode = Benchmark_pb.decode_int_repeated

end  

module IntPackedRepeated= struct 

  type t = Benchmark_types.int_packed_repeated
  
  let make_impl n = 
    let r = Pbrt.Repeated_field.make 0 in 
    let rec aux = function
      | 0 -> r
      | n -> Pbrt.Repeated_field.add n r;  aux (n-1) 
    in
    aux n

  let make difficulty  =
    let difficulty_size = difficulty_size_of_difficulty difficulty in
    let v = Benchmark_types.({
      int_packed_repeated = make_impl difficulty_size
     }) in
     (v , difficulty_size)

  let difficulty_size {Benchmark_types.int_packed_repeated = l} = Pbrt.Repeated_field.length l

  let encode = Benchmark_pb.encode_int_packed_repeated

  let decode = Benchmark_pb.decode_int_packed_repeated

end  

let get_test = function 
  | Benchmark_types.Int_list -> (module IntList : T_sig) 
  | Benchmark_types.Int_repeated -> (module IntRepeated: T_sig) 
  | Benchmark_types.Int_packed_repeated -> (module IntPackedRepeated: T_sig) 
  | Benchmark_types.Int32_list -> (module Int32List: T_sig) 
