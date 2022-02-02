
module B = Benchmark

let mk_t name f = name, f, ()

module Enc = struct
  type t = Buffer.t

  let[@inline] varint_rec i (e:t) =
    let rec write i =
      if Int64.(logand i (lognot 0x7fL)) = Int64.zero
      then
        Buffer.add_char e (char_of_int Int64.(to_int (logand 0x7fL i)))
      else begin
        Buffer.add_char e (char_of_int Int64.(
            to_int (logor 0x80L (logand 0x7fL i))
        ));
        write (Int64.shift_right_logical i 7)
      end
    in
    write i

  let[@inline] varint_imp (i:int64) e =
    let i = ref i in
    let continue = ref true in
    while !continue do
      let cur = Int64.(logand !i 0x7fL) in
      if cur = !i
      then (
        continue := false;
        Buffer.add_char e (char_of_int Int64.(to_int cur))
      ) else (
        Buffer.add_char e
          (char_of_int Int64.( to_int (logor 0x80L cur)
        ));
        i := Int64.shift_right_logical !i 7;
      )
    done

  let test_imp buf n  =
    mk_t  "enc-varint-imp" @@ fun () ->
    Sys.opaque_identity (
      Buffer.clear buf;
      for i = 0 to n do
        varint_imp (Int64.of_int i) buf;
      done
    )

  let test_rec buf n  =
    mk_t  "enc-varint-rec" @@ fun () ->
    Sys.opaque_identity (
      Buffer.clear buf;
      for i = 0 to n do
        varint_rec (Int64.of_int i) buf;
      done
    )

  (* sanity check *)
  let () =
    let buf = Buffer.create 32 in
    let to_str f n =
      Buffer.clear buf;
      for i = 0 to n do
        f (Int64.of_int i) buf
      done;
      Buffer.contents buf
    in

    List.iter (fun n ->
      let s_imp = to_str varint_imp n in
      let s_rec = to_str varint_rec n in
      assert (s_imp = s_rec))
      [1; 5; 100]

end

let test_enc n =
  let open B.Tree in
  let buf = Buffer.create 64 in
  Printf.sprintf "%d" n @> lazy (
    B.throughputN ~repeat:3 4 [Enc.test_imp buf n; Enc.test_rec buf n]
  )

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
  let[@inline] varint_imp d : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = byte d in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        res := Int64.(logor !res (shift_left (of_int cur) !shift));
        shift := !shift + 7;
      ) else if !shift < 63 || (b land 0x7f) <= 1 then (
        res := Int64.(logor !res (shift_left (of_int b) !shift));
        continue := false;
      ) else (
        raise (Failure Overlong_varint)
      );
    done;
    !res

  let[@inline] varint_rec d =
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

  let test_imp n (s:string) =
    mk_t  "dec-varint-imp" @@ fun () ->
    Sys.opaque_identity (
      let dec = of_string s in
      for _i = 0 to n do
        let _n = varint_imp dec in
        ()
      done
    )

  let test_rec n (s:string) =
    mk_t "dec-varint-rec" @@ fun () ->
    Sys.opaque_identity (
      let dec = of_string s in
      for _i = 0 to n do
        let _n = varint_rec dec in
        ()
      done
    )

  (* sanity check *)
  let () =
    let n = 5 in
    let s = mk_buf_n n in

    let dec_to_l f =
      let dec = of_string s in
      let l =ref [] in
      for _i = 0 to n do
        let n = f dec in
        l := Int64.to_int n :: !l
      done;
      List.rev !l
    in
    assert (dec_to_l varint_rec = [0;1;2;3;4;5]);
    assert (dec_to_l varint_imp = [0;1;2;3;4;5]);
    ()
end

let test_dec n =
  let open B.Tree in
  let s = Dec.mk_buf_n n in
  Printf.sprintf "%d" n @> lazy (
    B.throughputN ~repeat:3 4 [Dec.test_imp n s; Dec.test_rec n s]
  )

let () =
  let open B.Tree in
  register @@ "varint" @>>> [
      "dec" @>>> [ test_dec 5; test_dec 10; test_dec 50; test_dec 1000 ];
      "enc" @>>> [ test_enc 5; test_enc 10; test_enc 50; test_enc 1000 ];
    ]

let () =
  B.Tree.run_global ()
