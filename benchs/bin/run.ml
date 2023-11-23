open Foo

let spf = Printf.sprintf

(* company, with [n] stores and [2^depth] subsidiaries *)
let rec mk_company ~n ~depth : company =
  {
    name = "bigcorp";
    subsidiaries =
      (if depth = 0 then
        []
      else (
        let c = mk_company ~n ~depth:(depth - 1) in
        [ c; c ]
      ));
    stores =
      List.init n (fun i ->
          {
            address = spf "%d foobar street" i;
            clients =
              List.init 2 (fun j ->
                  {
                    name = spf "client_%d_%d" i j;
                    age = Int64.of_int ((j mod 30) + 15);
                  });
            employees =
              List.init 2 (fun j ->
                  {
                    name = spf "employee_%d_%d" i j;
                    age = Int64.of_int ((j mod 30) + 18);
                  });
          });
  }

let comp = mk_company ~n:3 ~depth:2

let () =
  let n = ref 3 in
  let depth = ref 2 in
  let iters = ref 100 in

  let opts =
    [
      "-n", Arg.Set_int n, " size for data";
      "--depth", Arg.Set_int depth, " nesting depth for data";
      "--iters", Arg.Set_int iters, " number of iterations";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  Printf.printf "n=%d, depth=%d, iters=%n\n%!" !n !depth !iters;
  let comp = mk_company ~n:!n ~depth:!depth in

  let enc = Pbrt.Encoder.create () in

  Sys.opaque_identity (Foo.encode_pb_company comp enc);
  let size = String.length @@ Pbrt.Encoder.to_string enc in
  Printf.printf "size=%d B\n%!" size;

  for _i = 1 to !iters do
    Pbrt.Encoder.clear enc;
    Sys.opaque_identity (Foo.encode_pb_company comp enc)
  done
