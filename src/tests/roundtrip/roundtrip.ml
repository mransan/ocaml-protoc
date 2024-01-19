open! Base

module type S = sig
  type t [@@deriving equal, quickcheck, sexp_of]

  val gen : t QCheck2.Gen.t
  val encode_pb : t -> Pbrt.Encoder.t -> unit
  val decode_pb : Pbrt.Decoder.t -> t
  val encode_json : t -> Yojson.Basic.t
  val decode_json : Yojson.Basic.t -> t
end

let roundtrip_property_exn (type a) (module M : S with type t = a) ~encoder
    (t : a) =
  Pbrt.Encoder.clear encoder;
  M.encode_pb t encoder;
  let encoded = Pbrt.Encoder.to_string encoder in
  let decoder = Pbrt.Decoder.of_string encoded in
  let decoded = M.decode_pb decoder in
  if not (M.equal t decoded) then
    raise_s
      [%sexp
        "Decoded pb value not equal to original value",
          { t : M.t; decoded : M.t; encoded : string }];

  let encoded = M.encode_json t |> Yojson.Basic.to_string in
  let decoded = M.decode_json (Yojson.Basic.from_string encoded) in
  if not (M.equal t decoded) then
    raise_s
      [%sexp
        "Decoded json value not equal to original value",
          { t : M.t; decoded : M.t; encoded : string }]

let run_quickcheck (type a) ?examples (module M : S with type t = a) =
  let encoder = Pbrt.Encoder.create () in
  let result =
    Base_quickcheck.Test.run
      (module M)
      ?examples
      ~f:(fun t ->
        Or_error.try_with (fun () ->
            roundtrip_property_exn (module M) ~encoder t))
  in
  match result with
  | Ok () -> ()
  | Error e -> Stdlib.print_endline (Error.to_string_hum e)

let run_qcheck (type a) ?(examples = []) (module M : S with type t = a) =
  let gen = QCheck2.Gen.graft_corners M.gen examples () in
  let encoder = Pbrt.Encoder.create () in
  let cell =
    QCheck2.Test.make_cell gen (fun t ->
        roundtrip_property_exn (module M) ~encoder t;
        true)
  in
  let test_result = QCheck2.Test.check_cell cell in
  match QCheck2.TestResult.get_state test_result with
  | Success -> ()
  | Error { instance; exn; backtrace = _ } ->
    Stdlib.print_endline
      (Sexp.to_string_hum
         [%sexp
           "QCheck2.Test.check_cell failed",
             { input = (instance.instance : M.t) },
             (exn : Exn.t)])
  | Failed { instances = _ } | Failed_other { msg = _ } ->
    (* These cases are never triggered because we systematically raise if the
       property doesn't hold. *)
    assert false

let run (type a) ?examples (module M : S with type t = a) =
  run_quickcheck ?examples (module M);
  run_qcheck ?examples (module M);
  ()
