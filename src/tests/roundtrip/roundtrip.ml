module type S = sig
  type t

  val quickcheck : t Pbrt_quickcheck.Type_class.t
  val gen : t QCheck2.Gen.t
end

module Test_failure = struct
  type t = {
    format: string;
    input: string;
    decoded: string;
    encoded: string;
  }
  [@@deriving show]
end

exception Test_failure of Test_failure.t

let () =
  Printexc.register_printer (function
    | Test_failure test_failure -> Some (Test_failure.show test_failure)
    | _ -> None)

let show (type a) (module M : S with type t = a) (t : a) =
  Format.asprintf "%a" M.quickcheck.pp t

let roundtrip_property_exn (type a) (module M : S with type t = a) ~encoder
    (t : a) =
  Pbrt.Encoder.clear encoder;
  M.quickcheck.encode_pb t encoder;
  let encoded = Pbrt.Encoder.to_string encoder in
  let decoder = Pbrt.Decoder.of_string encoded in
  let decoded = M.quickcheck.decode_pb decoder in
  if not (M.quickcheck.equal t decoded) then
    raise
      (Test_failure
         {
           format = "protobuf";
           input = show (module M) t;
           decoded = show (module M) decoded;
           encoded;
         });

  let encoded = M.quickcheck.encode_json t |> Yojson.Basic.to_string in
  let decoded = M.quickcheck.decode_json (Yojson.Basic.from_string encoded) in
  if not (M.quickcheck.equal t decoded) then
    raise
      (Test_failure
         {
           format = "json";
           input = show (module M) t;
           decoded = show (module M) decoded;
           encoded;
         })

let run (type a) ?(examples = []) (module M : S with type t = a) =
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
    Format.printf "QCheck2.Test.check_cell failed\ninput: %a\nerror: %s@."
      M.quickcheck.pp instance.instance (Printexc.to_string exn)
  | Failed { instances = _ } | Failed_other { msg = _ } ->
    (* These cases are never triggered because we systematically raise with
       added context if the property doesn't hold. *)
    assert false
