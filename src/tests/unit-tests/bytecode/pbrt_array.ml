let insert_n n : int Pbrt.Repeated_field.t =
  let a = Pbrt.Repeated_field.make 0 in
  let rec loop = function
    | i when i = n -> ()
    | i ->
      Pbrt.Repeated_field.add i a;
      loop (i + 1)
  in
  loop 0;
  a

let () =
  (* Small array *)
  let a = insert_n 3 in
  assert ([| 0; 1; 2 |] = Pbrt.Repeated_field.to_array a)

let test_n n : unit =
  let a = insert_n n in
  let a = Pbrt.Repeated_field.to_array a in
  Array.iteri (fun i j -> assert (i = j)) a

let () =
  (* Larger Repeated_field *)
  for i = 0 to 11_000 do
    test_n i
  done

let test_n n : unit =
  let a = insert_n n in
  let a = Pbrt.Repeated_field.map_to_array (fun x -> -x) a in
  Array.iteri (fun i j -> assert (i = -j)) a

let () =
  (* Larger Repeated_field *)
  for i = 0 to 5_000 do
    test_n i
  done

let () =
  (* Pbrt.Repeated_field.fold_left *)
  let a = insert_n 3 in
  let l = Pbrt.Repeated_field.fold_left (fun acc e -> e :: acc) [] a in
  assert ([ 2; 1; 0 ] = l)

let test_n n : unit =
  let a = insert_n n in
  let l = Pbrt.Repeated_field.map_to_list (fun x -> -x) a in
  List.iteri (fun i j -> assert (i = -j)) l

let () =
  for i = 0 to 1_000 do
    test_n i
  done

let () = assert (0 = Pbrt.Repeated_field.length @@ Pbrt.Repeated_field.make 100)
