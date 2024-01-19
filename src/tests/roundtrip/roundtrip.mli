open! Base

module type S = sig
  type t [@@deriving equal, quickcheck, sexp_of]

  val gen : t QCheck2.Gen.t
  val encode_pb : t -> Pbrt.Encoder.t -> unit
  val decode_pb : Pbrt.Decoder.t -> t
  val encode_json : t -> Yojson.Basic.t
  val decode_json : Yojson.Basic.t -> t
end

val run : ?examples:'a list -> (module S with type t = 'a) -> unit
