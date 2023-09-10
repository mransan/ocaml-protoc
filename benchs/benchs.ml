module B = Benchmark

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

  let byte d =
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
    for i = 0 to n do
      Pbrt.Encoder.int_as_varint i enc
    done;
    Pbrt.Encoder.to_string enc

  let test_imp n (s : string) =
    mk_t "dec-varint-imp" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_imp dec in
         ()
       done)

  let test_imp2 n (s : string) =
    mk_t "dec-varint-imp-noinline" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_imp_noinline dec in
         ()
       done)

  let test_rec n (s : string) =
    mk_t "dec-varint-rec" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_rec dec in
         ()
       done)

  let test_rec2 n (s : string) =
    mk_t "dec-varint-rec-noinline" @@ fun () ->
    Sys.opaque_identity
      (let dec = of_string s in
       for _i = 0 to n do
         let _n = varint_rec_noinline dec in
         ()
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

  module Make_enc (E : ENC) = struct
    let enc_person (p : person) (e : E.t) : unit =
      E.key 1 Bytes (E.string p.name) e;
      E.key 2 Varint (E.int64_as_varint p.age) e;
      ()

    let enc_store (st : store) (e : E.t) : unit =
      E.key 1 Bytes (E.string st.address) e;
      E.list
        (fun p e -> E.key 2 Bytes (E.nested (enc_person p)) e)
        st.employees e;
      E.list (fun p e -> E.key 3 Bytes (E.nested (enc_person p)) e) st.clients e;
      ()

    let enc_company (c : company) (e : E.t) : unit =
      E.key 1 Bytes (E.string c.name) e;
      E.list (fun st e -> E.key 2 Bytes (E.nested (enc_store st)) e) c.stores e;
      ()
  end

  let spf = Printf.sprintf

  let mk_company n =
    {
      name = "bigcorp";
      stores =
        List.init n (fun i ->
            {
              address = spf "%d foobar street" i;
              clients =
                List.init 30 (fun j ->
                    {
                      name = spf "client_%d_%d" i j;
                      age = Int64.of_int ((j mod 30) + 15);
                    });
              employees =
                List.init 5 (fun j ->
                    {
                      name = spf "employee_%d_%d" i j;
                      age = Int64.of_int ((j mod 30) + 18);
                    });
            });
    }

  module Make_bench (E : ENC) = struct
    include Make_enc (E)

    let bench company =
      let enc = E.create () in
      mk_t (spf "nested-enc-%s" E.name_of_enc) @@ fun () ->
      Sys.opaque_identity
        (E.clear enc;
         enc_company company enc)

    let string_of_company c =
      let e = E.create () in
      enc_company c e;
      E.to_string e
  end

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

    let[@inline never] grow_ self =
      assert (self.start = 0);
      let n = cap self in
      let newcap = n + (n lsr 1) + 3 in
      grow_to_ self newcap;
      assert (self.start > 0)

    let[@inline] add_char (self : t) (c : char) : unit =
      if self.start = 0 then grow_ self;
      self.start <- self.start - 1;
      Bytes.unsafe_set self.b self.start c

    let add_bytes (self : t) (b : bytes) =
      let n = Bytes.length b in
      if self.start - n <= 0 then grow_to_ self (cap self + n + (n lsr 1) + 1);
      Bytes.blit b 0 self.b (self.start - n) n;
      self.start <- self.start - n;
      ()

    let varint i (e : t) =
      let[@unroll 2] rec write i =
        let cur = Int64.(logand i 0x7fL) in
        if cur = i then
          add_char e (Char.unsafe_chr Int64.(to_int cur))
        else (
          write (Int64.shift_right_logical i 7);
          add_char e (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)))
        )
      in
      write i

    (* TODO: can we do this in a loop?
       let varint (i:int64) (e:t) =
         let i = ref i in
         let continue = ref true in
         while !continue do
           let cur = Int64.(logand !i 0x7fL) in
           if cur = !i
           then (
             continue := false;
             add_char e (Char.unsafe_chr Int64.(to_int cur))
           ) else (
             add_char e
               (Char.unsafe_chr Int64.( to_int (logor 0x80L cur)
             ));
             i := Int64.shift_right_logical !i 7;
           )
         done
    *)

    let int64_as_varint = varint
    let int_as_varint i e = varint (Int64.of_int i) e

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

  let bench_basic = Basic.bench
  let bench_buffers_nested = Buffers_nested.bench
  let bench_from_back = From_back.bench

  (* sanity check *)
  let () =
    let s_basic = Basic.string_of_company (mk_company 1) in
    let s_buffers_nested = Buffers_nested.string_of_company (mk_company 1) in
    let s_from_back = From_back.string_of_company (mk_company 1) in
    (*
    Printf.printf "basic: (len=%d) %S\n" (String.length s_basic) s_basic;
    Printf.printf "from_back: (len=%d) %S\n" (String.length s_from_back) s_from_back;
       *)
    let dec_s s =
      Pbrt.Decoder.(
        let dec = of_string s in
        Foo.decode_pb_company dec)
    in
    let c_basic = dec_s s_basic in
    let c_buffers_nested = dec_s s_buffers_nested in
    let c_from_back = dec_s s_from_back in
    (*
    Format.printf "c_basic=%a@." Foo_pp.pp_company c_basic;
    Format.printf "c_from_back=%a@." Foo_pp.pp_company c_from_back;
       *)
    assert (c_basic = c_buffers_nested);
    assert (c_basic = c_from_back);
    ()
end

let test_nested_enc n =
  let open B.Tree in
  let company = Nested.mk_company n in
  Printf.sprintf "%d" n
  @> lazy
       (B.throughputN ~repeat:4 3
          [
            Nested.bench_basic company;
            Nested.bench_buffers_nested company;
            Nested.bench_from_back company;
          ])

let () =
  let open B.Tree in
  register @@ "nested"
  @>>> [
         "enc"
         @>>> [
                test_nested_enc 2;
                test_nested_enc 5;
                test_nested_enc 10;
                test_nested_enc 20;
                test_nested_enc 50;
                test_nested_enc 100;
              ];
       ]

let () = B.Tree.run_global ()
