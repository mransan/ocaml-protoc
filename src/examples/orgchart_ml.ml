open Orgchart

let spf = Printf.sprintf

(* company, with [n] stores and [2^depth] subsidiaries *)
let rec mk_company ~n ~depth : company =
  make_company ~name:"bigcorp"
    ~subsidiaries:
      (if depth = 0 then
         []
       else (
         let c = mk_company ~n ~depth:(depth - 1) in
         [ c; c ]
       ))
    ~stores:
      (List.init n (fun i ->
           make_store ~address:(spf "%d foobar street" i)
             ~clients:
               (List.init 2 (fun j ->
                    make_person ~name:(spf "client_%d_%d" i j)
                      ~age:(Int64.of_int ((j mod 30) + 15))
                      ()))
             ~employees:
               (List.init 2 (fun j ->
                    make_person ~name:(spf "employee_%d_%d" i j)
                      ~age:(Int64.of_int ((j mod 30) + 18))
                      ()))
             ()))
    ()

let test ~n ~depth () : unit =
  let c = mk_company ~n ~depth in
  let enc = Pbrt.Encoder.create () in
  encode_pb_company c enc;
  let str = Pbrt.Encoder.to_string enc in
  let c2 =
    let dec = Pbrt.Decoder.of_string str in
    decode_pb_company dec
  in

  if c <> c2 then (
    Format.eprintf "c=%a@." pp_company c;
    Format.eprintf "dec(enc(c))=%a@." pp_company c2;
    failwith @@ spf "failed for n=%d, depth=%d" n depth
  )
(* else Printf.eprintf "ok for n=%d, depth=%d\n%!" n depth *)

let () =
  List.iter
    (fun (n, depth) -> test ~n ~depth ())
    [
      1, 1;
      1, 1;
      1, 4;
      1, 6;
      1, 10;
      2, 1;
      2, 4;
      2, 6;
      2, 10;
      5, 1;
      5, 4;
      5, 6;
      10, 1;
      10, 2;
      10, 3;
      10, 4;
      20, 1;
      50, 1;
      20, 3;
      20, 4;
      50, 1;
      50, 3;
      50, 4;
      100, 1;
      100, 3;
    ]
