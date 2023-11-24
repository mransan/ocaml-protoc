module B = Benchmark

let spf = Printf.sprintf
let mk_t name f = name, f, ()

module Enc = struct
  type t = Buffer.t

  let[@inline] varint_rec i (e : t) =
    let rec write i =
      if Int64.(logand i (lognot 0x7fL)) = Int64.zero then
        Buffer.add_char e (char_of_int Int64.(to_int (logand 0x7fL i)))
      else (
        Buffer.add_char e
          (char_of_int Int64.(to_int (logor 0x80L (logand 0x7fL i))));
        write (Int64.shift_right_logical i 7)
      )
    in
    write i

  let[@inline] varint_imp (i : int64) e =
    let i = ref i in
    let continue = ref true in
    while !continue do
      let cur = Int64.(logand !i 0x7fL) in
      if cur = !i then (
        continue := false;
        Buffer.add_char e (char_of_int Int64.(to_int cur))
      ) else (
        Buffer.add_char e (char_of_int Int64.(to_int (logor 0x80L cur)));
        i := Int64.shift_right_logical !i 7
      )
    done

  let[@inline] varint_imp_coi (i : int64) e =
    let i = ref i in
    let continue = ref true in
    while !continue do
      let cur = Int64.(logand !i 0x7fL) in
      if cur = !i then (
        continue := false;
        Buffer.add_char e (Char.unsafe_chr Int64.(to_int cur))
      ) else (
        Buffer.add_char e (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
        i := Int64.shift_right_logical !i 7
      )
    done

  let test_imp buf n =
    mk_t "enc-varint-imp" @@ fun () ->
    Sys.opaque_identity
      (Buffer.clear buf;
       for i = 0 to n do
         varint_imp (Int64.of_int i) buf
       done)

  let test_imp_coi buf n =
    mk_t "enc-varint-imp-coi" @@ fun () ->
    Sys.opaque_identity
      (Buffer.clear buf;
       for i = 0 to n do
         varint_imp_coi (Int64.of_int i) buf
       done)

  let test_rec buf n =
    mk_t "enc-varint-rec" @@ fun () ->
    Sys.opaque_identity
      (Buffer.clear buf;
       for i = 0 to n do
         varint_rec (Int64.of_int i) buf
       done)

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

    List.iter
      (fun n ->
        let s_imp = to_str varint_imp n in
        let s_rec = to_str varint_rec n in
        assert (s_imp = s_rec))
      [ 1; 5; 100 ]
end

let test_enc n =
  let open B.Tree in
  let buf = Buffer.create 64 in
  Printf.sprintf "%d" n
  @> lazy
       (B.throughputN ~repeat:3 4
          [ Enc.test_imp buf n; Enc.test_imp_coi buf n; Enc.test_rec buf n ])

module Dec = struct
  open Pbrt.Decoder

  type t = {
    source: bytes;
    limit: int;
    mutable offset: int;
  }

  let of_bytes source = { source; offset = 0; limit = Bytes.length source }
  let of_string source = of_bytes (Bytes.unsafe_of_string source)

  let[@inline] byte d =
    if d.offset >= d.limit then raise (Failure Incomplete);
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
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        raise (Failure Overlong_varint)
    done;
    !res

  (* imperative no inline *)
  let varint_imp_noinline d : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = byte d in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        raise (Failure Overlong_varint)
    done;
    !res

  let[@inline] varint_rec d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int64.(logor (shift_left (logand (of_int b) 0x7fL) s) (read (s + 7)))
      else if s < 63 || b land 0x7f <= 1 then
        Int64.(shift_left (of_int b) s)
      else
        raise (Failure Overlong_varint)
    in
    read 0

  let varint_rec_noinline d =
    let rec read s =
      let b = byte d in
      if b land 0x80 <> 0 then
        Int64.(logor (shift_left (logand (of_int b) 0x7fL) s) (read (s + 7)))
      else if s < 63 || b land 0x7f <= 1 then
        Int64.(shift_left (of_int b) s)
      else
        raise (Failure Overlong_varint)
    in
    read 0

  (* make a buffer with the integers from [0] to [n] inside *)
  let mk_buf_n n : string =
    let enc = Pbrt.Encoder.create () in
    for i = n downto 0 do
      Pbrt.Encoder.int_as_varint i enc
    done;
    Pbrt.Encoder.to_string enc

  let test_imp n (s : string) =
    mk_t "dec-varint-imp" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_imp dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_imp2 n (s : string) =
    mk_t "dec-varint-imp-noinline" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_imp_noinline dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_rec n (s : string) =
    mk_t "dec-varint-rec" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_rec dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_rec2 n (s : string) =
    mk_t "dec-varint-rec-noinline" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_rec_noinline dec in
         Sys.opaque_identity (ignore _n)
       done)

  (* sanity check *)
  let () =
    let n = 5 in
    let s = mk_buf_n n in

    let dec_to_l f =
      let dec = of_string s in
      let l = ref [] in
      for _i = 0 to n do
        let n = f dec in
        l := Int64.to_int n :: !l
      done;
      List.rev !l
    in
    assert (dec_to_l varint_rec = [ 0; 1; 2; 3; 4; 5 ]);
    assert (dec_to_l varint_imp = [ 0; 1; 2; 3; 4; 5 ]);
    ()
end

let test_dec n =
  let open B.Tree in
  let s = Dec.mk_buf_n n in
  Printf.sprintf "%d" n
  @> lazy
       (B.throughputN ~repeat:3 4
          [
            Dec.test_imp n s;
            Dec.test_imp2 n s;
            Dec.test_rec n s;
            Dec.test_rec2 n s;
          ])

let () =
  let open B.Tree in
  register @@ "varint"
  @>>> [
         "dec" @>>> [ test_dec 5; test_dec 10; test_dec 50; test_dec 1000 ];
         "enc" @>>> [ test_enc 5; test_enc 10; test_enc 50; test_enc 1000 ];
       ]

module Dec_bits64 = struct
  open Pbrt.Decoder
  open! Dec

  (* put the int64 integers from 0 to n in a dec *)
  let mk_buf_n n : string =
    let enc = Pbrt.Encoder.create () in
    for i = n downto 0 do
      Pbrt.Encoder.int_as_bits64 i enc
    done;
    Pbrt.Encoder.to_string enc

  (** Read 8 bytes at once, return offset of the first one. *)
  let get8 (self : t) : int =
    if self.offset + 8 > self.limit then raise (Failure Incomplete);
    let n = self.offset in
    self.offset <- self.offset + 8;
    n

  let bits64_unrolled (d : t) =
    let b1 = byte d in
    let b2 = byte d in
    let b3 = byte d in
    let b4 = byte d in
    let b5 = byte d in
    let b6 = byte d in
    let b7 = byte d in
    let b8 = byte d in
    Int64.(
      add
        (shift_left (of_int b8) 56)
        (add
           (shift_left (of_int b7) 48)
           (add
              (shift_left (of_int b6) 40)
              (add
                 (shift_left (of_int b5) 32)
                 (add
                    (shift_left (of_int b4) 24)
                    (add
                       (shift_left (of_int b3) 16)
                       (add (shift_left (of_int b2) 8) (of_int b1))))))))

  let bits64_unrolled_single_read (d : t) =
    let off = get8 d in
    let b1 = int_of_char @@ Bytes.unsafe_get d.source off in
    let b2 = int_of_char @@ Bytes.unsafe_get d.source (off + 1) in
    let b3 = int_of_char @@ Bytes.unsafe_get d.source (off + 2) in
    let b4 = int_of_char @@ Bytes.unsafe_get d.source (off + 3) in
    let b5 = int_of_char @@ Bytes.unsafe_get d.source (off + 4) in
    let b6 = int_of_char @@ Bytes.unsafe_get d.source (off + 5) in
    let b7 = int_of_char @@ Bytes.unsafe_get d.source (off + 6) in
    let b8 = int_of_char @@ Bytes.unsafe_get d.source (off + 7) in
    Int64.(
      add
        (shift_left (of_int b8) 56)
        (add
           (shift_left (of_int b7) 48)
           (add
              (shift_left (of_int b6) 40)
              (add
                 (shift_left (of_int b5) 32)
                 (add
                    (shift_left (of_int b4) 24)
                    (add
                       (shift_left (of_int b3) 16)
                       (add (shift_left (of_int b2) 8) (of_int b1))))))))

  let bits64_loop (d : t) =
    let res = ref 0L in
    for i = 0 to 7 do
      let byte = byte d in
      res := Int64.(logor !res (shift_left (of_int byte) (8 * i)))
    done;
    !res

  let bits64_from_stdlib (d : t) : int64 =
    let off = get8 d in
    Bytes.get_int64_le d.source off

  let test_unrolled s n =
    mk_t "dec-bits64-unrolled" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = bits64_unrolled dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_unrolled_single_read s n =
    mk_t "dec-bits64-unrolled-single-read" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = bits64_unrolled_single_read dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_loop s n =
    mk_t "dec-bits64-loop" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = bits64_loop dec in
         Sys.opaque_identity (ignore _n)
       done)

  let test_stdlib s n =
    mk_t "dec-bits64-stdlib" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = bits64_from_stdlib dec in
         Sys.opaque_identity (ignore _n)
       done)

  (* sanity check *)
  let () =
    let n = 5 in
    let s = mk_buf_n n in

    let dec_to_l f =
      let dec = of_string s in
      let l = ref [] in
      for _i = 0 to n do
        let n = f dec in
        l := Int64.to_int n :: !l
      done;
      List.rev !l
    in
    assert (dec_to_l bits64_unrolled = [ 0; 1; 2; 3; 4; 5 ]);
    assert (dec_to_l bits64_unrolled_single_read = [ 0; 1; 2; 3; 4; 5 ]);
    assert (dec_to_l bits64_loop = [ 0; 1; 2; 3; 4; 5 ]);
    assert (dec_to_l bits64_from_stdlib = [ 0; 1; 2; 3; 4; 5 ]);
    ()
end

let test_dec_bits64 n =
  let open B.Tree in
  let s = Dec_bits64.mk_buf_n n in
  Printf.sprintf "%d" n
  @> lazy
       (B.throughputN ~repeat:3 4
          [
            Dec_bits64.test_unrolled s n;
            Dec_bits64.test_unrolled_single_read s n;
            Dec_bits64.test_loop s n;
            Dec_bits64.test_stdlib s n;
          ])

let () =
  let open B.Tree in
  register @@ "bits64"
  @>>> [
         "dec"
         @>>> [
                test_dec_bits64 5;
                test_dec_bits64 10;
                test_dec_bits64 50;
                test_dec_bits64 1000;
              ];
         (* "enc" @>>> [ test_enc 5; test_enc 10; test_enc 50; test_enc 1000 ]; *)
       ]

module Varint_size = struct
  type run_loop = n:int -> unit

  module While_inline = struct
    (** Number of bytes to encode [i] *)
    let[@inline] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      let continue = ref true in
      while !continue do
        incr n;
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then
          continue := false
        else
          i := Int64.shift_right_logical !i 7
      done;
      !n

    let loop ~n =
      for i = 1 to n do
        ignore (Sys.opaque_identity (varint_size (Int64.of_int i)) : int)
      done
  end

  module While_noinline = struct
    let[@inline never] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      let continue = ref true in
      while !continue do
        incr n;
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then
          continue := false
        else
          i := Int64.shift_right_logical !i 7
      done;
      !n

    let loop ~n =
      for i = 1 to n do
        ignore (Sys.opaque_identity (varint_size (Int64.of_int i)) : int)
      done
  end

  module For_loop = struct
    external int_of_bool : bool -> int = "%identity"

    let[@inline] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      for _j = 0 to 10 do
        n := !n + int_of_bool (not (Int64.equal !i 0L));
        i := Int64.shift_right_logical !i 7
      done;
      !n

    let loop ~n =
      for i = 1 to n do
        ignore (Sys.opaque_identity (varint_size (Int64.of_int i)) : int)
      done
  end

  module C_while = struct
    external varint_size : (int64[@unboxed]) -> int
      = "b_caml_pbrt_varint_size_byte" "b_caml_pbrt_varint_size"
      [@@noalloc]

    let loop ~n =
      for i = 1 to n do
        ignore (Sys.opaque_identity (varint_size (Int64.of_int i)) : int)
      done
  end

  (* sanity checks *)
  let () =
    List.iter
      (fun i ->
        let i = Int64.of_int i in
        let c1 = While_inline.varint_size i in
        let c2 = While_noinline.varint_size i in
        let c3 = For_loop.varint_size i in
        let c4 = C_while.varint_size i in
        assert (c1 = c2);
        assert (c1 = c3);
        assert (c1 = c4))
      [
        1;
        2;
        3;
        10;
        15;
        20;
        21;
        22;
        30;
        50;
        100;
        300;
        1000;
        2000;
        100_000;
        1_000_000_000;
        max_int - 10;
        max_int;
      ]
end

let test_varint_size n =
  let open B.Tree in
  let mkbench name (run : Varint_size.run_loop) =
    mk_t (spf "varint-size-%s" name) @@ fun () -> Sys.opaque_identity (run ~n)
  in

  spf "%d" n
  @> lazy
       (B.throughputN ~repeat:4 3
          [
            mkbench "while-inline" Varint_size.While_inline.loop;
            mkbench "while-noinline" Varint_size.While_noinline.loop;
            mkbench "for-loop" Varint_size.For_loop.loop;
            mkbench "c-while" Varint_size.C_while.loop;
          ])

let () =
  let open B.Tree in
  register @@ "varint-size" @>>> List.map test_varint_size [ 1000; 100_000 ]

module Nested = struct
  type person = Foo.person = {
    name: string;
    age: int64;
  }

  type store = Foo.store = {
    address: string;
    employees: person list;
    clients: person list;
  }

  type company = Foo.company = {
    name: string;
    stores: store list;
    subsidiaries: company list;
  }

  type payload_kind = Pbrt.payload_kind =
    | Varint
    | Bits32
    | Bits64
    | Bytes

  module type ENC = sig
    val name_of_enc : string

    type t

    val create : unit -> t
    val clear : t -> unit
    val to_string : t -> string
    val string : string -> t -> unit
    val int_as_varint : int -> t -> unit
    val int64_as_varint : int64 -> t -> unit
    val key : int -> payload_kind -> (t -> unit) -> t -> unit
    val list : ('a -> t -> unit) -> 'a list -> t -> unit
    val nested : (t -> unit) -> t -> unit
  end

  let[@inline] zigzag i : int64 =
    Int64.(logxor (shift_left i 1) (shift_right i 63))

  module Make_enc (E : ENC) = struct
    let enc_person (p : person) (e : E.t) : unit =
      E.key 1 Bytes (E.string p.name) e;
      E.key 2 Varint (E.int64_as_varint @@ zigzag p.age) e;
      ()

    let enc_store (st : store) (e : E.t) : unit =
      E.key 1 Bytes (E.string st.address) e;
      E.list
        (fun p e -> E.key 2 Bytes (E.nested (enc_person p)) e)
        st.employees e;
      E.list (fun p e -> E.key 3 Bytes (E.nested (enc_person p)) e) st.clients e;
      ()

    let rec enc_company (c : company) (e : E.t) : unit =
      E.key 1 Bytes (E.string c.name) e;
      E.list (fun st e -> E.key 2 Bytes (E.nested (enc_store st)) e) c.stores e;
      E.list
        (fun st c -> E.key 3 Bytes (E.nested (enc_company st)) c)
        c.subsidiaries e;
      ()
  end

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

  module type MK_COMPANY = sig
    type t

    val name_of_enc : string
    val create : unit -> t
    val clear : t -> unit
    val enc_company : company -> t -> unit
    val to_string : t -> string
  end

  module Make_bench_of_mk_company (E : MK_COMPANY) = struct
    let bench company =
      (* create an encoder that we'll reuse every time, so we can measure
         the allocations caused purely by the encoding itself *)
      let enc = E.create () in
      mk_t E.name_of_enc @@ fun () ->
      E.clear enc;
      Sys.opaque_identity (E.enc_company company enc)

    let string_of_company c =
      let e = E.create () in
      E.enc_company c e;
      E.to_string e
  end

  module Make_bench (E : ENC) = struct
    module Arg = struct
      include E
      include Make_enc (E)
    end

    include Make_bench_of_mk_company (Arg)
  end

  module Cur = Make_bench_of_mk_company (struct
    let name_of_enc = "current"

    include Pbrt.Encoder

    let create () = create ()
    let enc_company = Foo.encode_pb_company
  end)

  module Basic = Make_bench (struct
    let name_of_enc = "basic-buffer"

    type t = Buffer.t

    let create () = Buffer.create 16
    let clear = Buffer.clear
    let to_string = Buffer.contents

    let varint (i : int64) e =
      let i = ref i in
      let continue = ref true in
      while !continue do
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then (
          continue := false;
          Buffer.add_char e (Char.unsafe_chr Int64.(to_int cur))
        ) else (
          Buffer.add_char e (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
          i := Int64.shift_right_logical !i 7
        )
      done

    let int64_as_varint = varint
    let int_as_varint i e = (varint [@inlined]) (Int64.of_int i) e

    let[@inline] key k pk f e =
      let pk' =
        match pk with
        | Varint -> 0
        | Bits64 -> 1
        | Bytes -> 2
        | Bits32 -> 5
      in
      int_as_varint (pk' lor (k lsl 3)) e;
      f e

    let bytes b e =
      int_as_varint (Bytes.length b) e;
      Buffer.add_bytes e b

    let string s e = bytes (Bytes.unsafe_of_string s) e
    let list f l e = List.iter (fun x -> f x e) l

    let nested f e =
      let e' = Buffer.create 16 in
      f e';
      int_as_varint (Buffer.length e') e;
      Buffer.add_buffer e e'
  end)

  type buffers_nested = {
    buf: Buffer.t;
    mutable sub: buffers_nested option;
  }

  module Buffers_nested = Make_bench (struct
    let name_of_enc = "nested-bufs"

    type t = buffers_nested

    let create () = { buf = Buffer.create 16; sub = None }
    let[@inline] clear self = Buffer.clear self.buf
    let[@inline] to_string self = Buffer.contents self.buf

    let varint (i : int64) e =
      let i = ref i in
      let continue = ref true in
      while !continue do
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then (
          continue := false;
          Buffer.add_char e.buf (Char.unsafe_chr Int64.(to_int cur))
        ) else (
          Buffer.add_char e.buf
            (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
          i := Int64.shift_right_logical !i 7
        )
      done

    let int64_as_varint = varint
    let int_as_varint i e = (varint [@inlined]) (Int64.of_int i) e

    let[@inline] key k pk f e =
      let pk' =
        match pk with
        | Varint -> 0
        | Bits64 -> 1
        | Bytes -> 2
        | Bits32 -> 5
      in
      int_as_varint (pk' lor (k lsl 3)) e;
      f e

    let bytes b e =
      int_as_varint (Bytes.length b) e;
      Buffer.add_bytes e.buf b

    let string s e = bytes (Bytes.unsafe_of_string s) e
    let list f l e = List.iter (fun x -> f x e) l

    let nested f e =
      let e' =
        match e.sub with
        | None ->
          let e' = create () in
          e.sub <- Some e';
          e'
        | Some e' ->
          clear e';
          e'
      in
      f e';
      int_as_varint (Buffer.length e'.buf) e;
      Buffer.add_buffer e.buf e'.buf
  end)

  type from_back_end = {
    mutable b: bytes;
    mutable start: int;
  }

  module From_back = Make_bench (struct
    let name_of_enc = "write-backward"

    type t = from_back_end

    let create () : t = { b = Bytes.create 16; start = 16 }
    let[@inline] clear self = self.start <- Bytes.length self.b
    let[@inline] cap self = Bytes.length self.b
    let[@inline] length self = cap self - self.start

    let to_string self : string =
      Bytes.sub_string self.b self.start (length self)

    let grow_to_ self newcap =
      let n = length self in
      let b' = Bytes.create newcap in
      Bytes.blit self.b self.start b' (newcap - n) n;
      self.b <- b';
      self.start <- newcap - n

    let next_cap_ (self : t) : int =
      let n = cap self in
      n + (n lsr 1) + 3

    let[@inline never] grow_reserve_n (self : t) n : unit =
      let newcap = max (cap self + n) (next_cap_ self) in
      grow_to_ self newcap;
      assert (self.start >= n)

    (** Reserve [n] bytes, return the offset at which we can write them. *)
    let[@inline] reserve_n (self : t) (n : int) : int =
      if self.start < n then grow_reserve_n self n;
      self.start <- self.start - n;
      self.start

    let add_bytes (self : t) (b : bytes) =
      let n = Bytes.length b in
      if self.start <= n then grow_to_ self (cap self + n + (n lsr 1) + 1);
      Bytes.blit b 0 self.b (self.start - n) n;
      self.start <- self.start - n;
      ()

    (** Number of bytes to encode [i] *)
    let[@inline] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      let continue = ref true in
      while !continue do
        incr n;
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then
          continue := false
        else
          i := Int64.shift_right_logical !i 7
      done;
      !n

    let[@inline] varint (i : int64) (e : t) : unit =
      let n_bytes = varint_size i in
      let start = reserve_n e n_bytes in

      let i = ref i in
      for j = 0 to n_bytes - 1 do
        let cur = Int64.(logand !i 0x7fL) in
        if j = n_bytes - 1 then
          Bytes.set e.b (start + j) (Char.unsafe_chr Int64.(to_int cur))
        else (
          Bytes.set e.b (start + j)
            (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
          i := Int64.shift_right_logical !i 7
        )
      done

    let int64_as_varint = varint
    let[@inline] int_as_varint i e = varint (Int64.of_int i) e

    let[@inline] key k pk f e =
      let pk' =
        match pk with
        | Varint -> 0
        | Bits64 -> 1
        | Bytes -> 2
        | Bits32 -> 5
      in
      f e;
      int_as_varint (pk' lor (k lsl 3)) e;
      (* write this after the data *)
      ()

    let bytes b (e : t) =
      add_bytes e b;
      int_as_varint (Bytes.length b) e;
      ()

    let string s e = bytes (Bytes.unsafe_of_string s) e

    (* encode lists in reverse order *)
    let list f l e =
      let rec loop = function
        | [] -> ()
        | [ x ] -> f x e
        | x :: tl ->
          loop tl;
          f x e
      in
      loop l

    let nested f (e : t) =
      let s0 = length e in
      f e;
      let size = length e - s0 in
      int_as_varint size e
  end)

  module From_back_noinline = Make_bench (struct
    let name_of_enc = "write-backward-noinline"

    type t = from_back_end

    let create () : t = { b = Bytes.create 16; start = 16 }
    let[@inline] clear self = self.start <- Bytes.length self.b
    let[@inline] cap self = Bytes.length self.b
    let[@inline] length self = cap self - self.start

    let to_string self : string =
      Bytes.sub_string self.b self.start (length self)

    let grow_to_ self newcap =
      let n = length self in
      let b' = Bytes.create newcap in
      Bytes.blit self.b self.start b' (newcap - n) n;
      self.b <- b';
      self.start <- newcap - n

    let next_cap_ (self : t) : int =
      let n = cap self in
      n + (n lsr 1) + 3

    let[@inline never] grow_reserve_n (self : t) n : unit =
      let newcap = max (cap self + n) (next_cap_ self) in
      grow_to_ self newcap;
      assert (self.start >= n)

    (** Reserve [n] bytes, return the offset at which we can write them. *)
    let[@inline] reserve_n (self : t) (n : int) : int =
      if self.start < n then grow_reserve_n self n;
      self.start <- self.start - n;
      self.start

    let add_bytes (self : t) (b : bytes) =
      let n = Bytes.length b in
      if self.start <= n then grow_to_ self (cap self + n + (n lsr 1) + 1);
      Bytes.blit b 0 self.b (self.start - n) n;
      self.start <- self.start - n;
      ()

    let[@inline never] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      let continue = ref true in
      while !continue do
        incr n;
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then
          continue := false
        else
          i := Int64.shift_right_logical !i 7
      done;
      !n

    let[@inline never] varint (i : int64) (e : t) : unit =
      let n_bytes = varint_size i in
      let start = reserve_n e n_bytes in

      let i = ref i in
      for j = 0 to n_bytes - 1 do
        let cur = Int64.(logand !i 0x7fL) in
        if j = n_bytes - 1 then
          Bytes.set e.b (start + j) (Char.unsafe_chr Int64.(to_int cur))
        else (
          Bytes.set e.b (start + j)
            (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
          i := Int64.shift_right_logical !i 7
        )
      done

    let int64_as_varint = varint
    let[@inline] int_as_varint i e = varint (Int64.of_int i) e

    let[@inline] key k pk f e =
      let pk' =
        match pk with
        | Varint -> 0
        | Bits64 -> 1
        | Bytes -> 2
        | Bits32 -> 5
      in
      f e;
      int_as_varint (pk' lor (k lsl 3)) e;
      (* write this after the data *)
      ()

    let bytes b (e : t) =
      add_bytes e b;
      int_as_varint (Bytes.length b) e;
      ()

    let string s e = bytes (Bytes.unsafe_of_string s) e

    (* encode lists in reverse order *)
    let list f l e =
      let rec loop = function
        | [] -> ()
        | [ x ] -> f x e
        | x :: tl ->
          loop tl;
          f x e
      in
      loop l

    let nested f (e : t) =
      let s0 = length e in
      f e;
      let size = length e - s0 in
      int_as_varint size e
  end)

  module From_back_c = Make_bench (struct
    let name_of_enc = "write-backward-c"

    type t = from_back_end

    let create () : t = { b = Bytes.create 16; start = 16 }
    let[@inline] clear self = self.start <- Bytes.length self.b
    let[@inline] cap self = Bytes.length self.b
    let[@inline] length self = cap self - self.start

    let to_string self : string =
      Bytes.sub_string self.b self.start (length self)

    let grow_to_ self newcap =
      let n = length self in
      let b' = Bytes.create newcap in
      Bytes.blit self.b self.start b' (newcap - n) n;
      self.b <- b';
      self.start <- newcap - n

    let next_cap_ (self : t) : int =
      let n = cap self in
      n + (n lsr 1) + 3

    let[@inline never] grow_reserve_n (self : t) n : unit =
      let newcap = max (cap self + n) (next_cap_ self) in
      grow_to_ self newcap;
      assert (self.start >= n)

    (** Reserve [n] bytes, return the offset at which we can write them. *)
    let[@inline] reserve_n (self : t) (n : int) : int =
      if self.start < n then grow_reserve_n self n;
      self.start <- self.start - n;
      self.start

    let add_bytes (self : t) (b : bytes) =
      let n = Bytes.length b in
      if self.start <= n then grow_to_ self (cap self + n + (n lsr 1) + 1);
      Bytes.blit b 0 self.b (self.start - n) n;
      self.start <- self.start - n;
      ()

    (*
    external varint_size : (int64[@unboxed]) -> int
      = "b_caml_pbrt_varint_size_byte" "b_caml_pbrt_varint_size"
      [@@noalloc]
      *)

    (* keep this in OCaml because the C overhead is non trivial *)
    let[@inline] varint_size (i : int64) : int =
      let i = ref i in
      let n = ref 0 in
      let continue = ref true in
      while !continue do
        incr n;
        let cur = Int64.(logand !i 0x7fL) in
        if cur = !i then
          continue := false
        else
          i := Int64.shift_right_logical !i 7
      done;
      !n

    external varint_slice :
      bytes -> (int[@untagged]) -> (int64[@unboxed]) -> unit
      = "b_caml_pbrt_varint_byte" "b_caml_pbrt_varint"
      [@@noalloc]

    let[@inline] varint (i : int64) (e : t) : unit =
      let n_bytes = varint_size i in
      let start = reserve_n e n_bytes in
      varint_slice e.b start i

    let int64_as_varint = varint
    let[@inline] int_as_varint i e = varint (Int64.of_int i) e

    let[@inline] key k pk f e =
      let pk' =
        match pk with
        | Varint -> 0
        | Bits64 -> 1
        | Bytes -> 2
        | Bits32 -> 5
      in
      f e;
      int_as_varint (pk' lor (k lsl 3)) e;
      (* write this after the data *)
      ()

    let bytes b (e : t) =
      add_bytes e b;
      int_as_varint (Bytes.length b) e;
      ()

    let string s e = bytes (Bytes.unsafe_of_string s) e

    (* encode lists in reverse order *)
    let list f l e =
      let rec loop = function
        | [] -> ()
        | [ x ] -> f x e
        | x :: tl ->
          loop tl;
          f x e
      in
      loop l

    let nested f (e : t) =
      let s0 = length e in
      f e;
      let size = length e - s0 in
      int_as_varint size e
  end)

  let bench_cur = Cur.bench
  let bench_basic = Basic.bench
  let bench_buffers_nested = Buffers_nested.bench
  let bench_from_back = From_back.bench
  let bench_from_back_noinline = From_back_noinline.bench
  let bench_from_back_c = From_back_c.bench

  let pp_size ~n ~depth =
    Printf.printf "bench nested enc: length for n=%d, depth=%d is %d B\n" n
      depth
      (String.length (Cur.string_of_company @@ mk_company ~n ~depth))

  (* sanity check *)
  let check ~n ~depth () =
    let comp = mk_company ~n ~depth in
    let s_cur = Cur.string_of_company comp in
    let s_basic = Basic.string_of_company comp in

    (*
    Printf.printf "###### n=%d, depth=%d\n" n depth;
    Printf.printf "s_cur[%d]=%S\n" (String.length s_cur) s_cur;
    Printf.printf "s_basic[%d]=%S\n" (String.length s_basic) s_basic;
    *)
    let s_buffers_nested = Buffers_nested.string_of_company comp in
    let s_from_back = From_back.string_of_company comp in
    let s_from_back2 = From_back_noinline.string_of_company comp in
    let s_from_backc = From_back_c.string_of_company comp in
    (*
       Printf.printf "basic:\n(len=%d) %S\n" (String.length s_basic) s_basic;
       Printf.printf "from_back:\n(len=%d) %S\n"
         (String.length s_from_back)
         s_from_back;
       Printf.printf "from_back_c:\n(len=%d) %S\n"
         (String.length s_from_backc)
         s_from_backc;
    *)
    let dec_s s =
      Pbrt.Decoder.(
        let dec = of_string s in
        Foo.decode_pb_company dec)
    in
    let c_basic = dec_s s_basic in
    let c_cur = dec_s s_cur in
    let c_buffers_nested = dec_s s_buffers_nested in
    let c_from_back = dec_s s_from_back in
    let c_from_back2 = dec_s s_from_back2 in
    let c_from_backc = dec_s s_from_backc in
    (*
    Format.printf "comp=%a@." Foo.pp_company comp;
    Format.printf "c_basic=%a@." Foo.pp_company c_basic;
    Format.printf "c_cur=%a@." Foo.pp_company c_cur;
       *)
    assert (c_basic = comp);
    assert (c_basic = c_cur);
    assert (c_basic = c_buffers_nested);
    assert (c_basic = c_from_back);
    assert (c_basic = c_from_back2);
    assert (c_basic = c_from_backc);
    ()

  let () =
    List.iter
      (fun (n, depth) -> check ~n ~depth ())
      [ 1, 3; 2, 4; 10, 1; 20, 2; 1, 10 ]
end

let test_nested_enc ~n ~depth =
  let open B.Tree in
  let company = Nested.mk_company ~n ~depth in
  Printf.sprintf "n=%d,depth=%d" n depth
  @> lazy
       (Nested.pp_size ~n ~depth;
        B.throughputN ~repeat:4 3
          [
            Nested.bench_basic company;
            Nested.bench_cur company;
            Nested.bench_buffers_nested company;
            Nested.bench_from_back company;
            Nested.bench_from_back_noinline company;
            Nested.bench_from_back_c company;
          ])

let () =
  let open B.Tree in
  register @@ "nested"
  @>>> [
         "enc"
         @>>> List.map
                (fun (n, depth) -> test_nested_enc ~n ~depth)
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
                ];
       ]

let () = B.Tree.run_global ()
