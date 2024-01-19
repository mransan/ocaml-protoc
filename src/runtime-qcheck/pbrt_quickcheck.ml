(** Runtime for QuickCheck based tests. *)

(** A type class generated for each type *)
module Type_class = struct
  type 'a t = {
    pp: Format.formatter -> 'a -> unit;
    equal: 'a -> 'a -> bool;
    encode_pb: 'a -> Pbrt.Encoder.t -> unit;
    decode_pb: Pbrt.Decoder.t -> 'a;
    encode_json: 'a -> Yojson.Basic.t;
    decode_json: Yojson.Basic.t -> 'a;
  }
end
