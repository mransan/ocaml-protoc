open! Base

module type S = sig
  type t [@@deriving equal, quickcheck, sexp_of]

  val encode_pb : t -> Pbrt.Encoder.t -> unit
  val decode_pb : Pbrt.Decoder.t -> t
  val encode_json : t -> Yojson.Basic.t
  val decode_json : Yojson.Basic.t -> t
end

let run (type a) ?examples (module M : S with type t = a) =
  let encoder = Pbrt.Encoder.create () in
  let result =
    Base_quickcheck.Test.run
      (module M)
      ?examples
      ~f:(fun t ->
        Or_error.try_with (fun () ->
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
                    { t : M.t; decoded : M.t; encoded : string }]))
  in
  match result with
  | Ok () -> ()
  | Error e -> Stdlib.print_endline (Error.to_string_hum e)
