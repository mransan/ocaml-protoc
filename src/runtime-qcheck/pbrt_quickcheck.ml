module Type_class = struct
  type 'a t = {
    type_name: string;
    pp: Format.formatter -> 'a -> unit;
    gen: 'a QCheck2.Gen.t;
    equal: 'a -> 'a -> bool;
    encode_pb: 'a -> Pbrt.Encoder.t -> unit;
    decode_pb: Pbrt.Decoder.t -> 'a;
    encode_json: 'a -> Yojson.Basic.t;
    decode_json: Yojson.Basic.t -> 'a;
  }
end

let protobuf_roundtrip (type a) (m : a Type_class.t) ~encoder (t : a) =
  Pbrt.Encoder.clear encoder;
  m.encode_pb t encoder;
  let encoded = Pbrt.Encoder.to_string encoder in
  let decoder = Pbrt.Decoder.of_string encoded in
  let decoded = m.decode_pb decoder in
  m.equal t decoded

let json_roundtrip (type a) (m : a Type_class.t) (t : a) =
  let encoded = m.encode_json t |> Yojson.Basic.to_string in
  let decoded = m.decode_json (Yojson.Basic.from_string encoded) in
  m.equal t decoded

module Test = struct
  type t = QCheck2.Test.t

  let make (type a) ?(examples = []) (m : a Type_class.t) =
    let gen = QCheck2.Gen.graft_corners m.gen examples () in
    let print a = Format.asprintf "%a" m.pp a in
    let encoder = Pbrt.Encoder.create () in
    [
      QCheck2.Test.make
        ~name:(Printf.sprintf "%s.protobuf-roundtrip" m.type_name) ~print gen
        (fun t -> protobuf_roundtrip m ~encoder t);
      QCheck2.Test.make ~name:(Printf.sprintf "%s.json-roundtrip" m.type_name)
        ~print gen (fun t -> json_roundtrip m t);
    ]
end
