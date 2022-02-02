
module B = Bechamel

module Dec = struct
  open Pbrt.Decoder

  type t = {
    source : bytes;
    limit  : int;
    mutable offset : int;
  }

  let of_bytes source =
    { source;
      offset = 0;
      limit  = Bytes.length source; }

  let of_string source = of_bytes (Bytes.unsafe_of_string source)

  let byte d =
    if d.offset >= d.limit then
      raise (Failure Incomplete);
    let byte = int_of_char (Bytes.get d.source d.offset) in
    d.offset <- d.offset + 1;
    byte

  (* imperative *)
  let varint_imp d : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = byte d in
      if b land 0x80 <> 0 then (
        (* at least one byte follows this one *)
        res := Int64.(logor !res (shift_left (logand (of_int b) 0x7fL) !shift));
        shift := !shift + 7;
      ) else if !shift < 63 || (b land 0x7f) <= 1 then (
        res := Int64.(logor !res (shift_left (of_int b) !shift));
        continue := false;
      ) else (
        raise (Failure Overlong_varint)
      );
    done;
    !res

  let varint_rec d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int64.(logor (shift_left (logand (of_int b) 0x7fL) s) (read (s + 7)))
      else if s < 63 || (b land 0x7f) <= 1 then
        Int64.(shift_left (of_int b) s)
      else
        raise (Failure Overlong_varint)
    in
    read 0

  (* make a buffer with the integers from [0] to [n] inside *)
  let mk_buf_n n : string =
    let enc = Pbrt.Encoder.create () in
    for i=0 to n do
      Pbrt.Encoder.int_as_varint i enc
    done;
    Pbrt.Encoder.to_string enc

  let test_imp n (s:string) : B.Test.t =
    B.Test.make ~name:"dec-varint-imp"
      (B.Staged.stage @@ fun () ->
       let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_imp dec in
         ()
       done;
      )

  let test_rec n (s:string) : B.Test.t =
    B.Test.make ~name:"dec-varint-rec"
      (B.Staged.stage @@ fun () ->
       let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_rec dec in
         ()
       done;
      )

end

let test_dec n =
  let s = Dec.mk_buf_n n in
  B.Test.make_grouped ~name:"decode-varint" [
    Dec.test_imp n s;
    Dec.test_rec n s;
  ]

let bench_dec n =
  let instances = B.[Toolkit.Instance.monotonic_clock] in
  let cfg =
    B.Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(B.Time.second 0.5) ()
  in
  B.Benchmark.all cfg instances @@ test_dec n

let () =
  let results = bench_dec 50 in 
  let ols = B.Analyze.ols ~bootstrap:0 ~r_square:true
    ~predictors:[| B.Measure.run |] in
  let results = B.Analyze.all ols B.Toolkit.Instance.monotonic_clock results in
  B.Analyze.merge ols [ B.Toolkit.Instance.monotonic_clock ] [ results ]

