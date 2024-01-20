(** Runtime for QuickCheck based tests.

    This module contains additional types and functions that are used by the
    code generator when the option [--quickcheck] is supplied to [ocaml-protoc]. *)

(** A type class generated for each type *)
module Type_class : sig
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

module Test : sig
  type t = QCheck2.Test.t

  val make : ?examples:'a list -> 'a Type_class.t -> t list
  (** Generates a test suite for that type that checks that values roundtrip
      through serializations. *)
end
