module type S = sig
  type t

  val quickcheck : t Pbrt_quickcheck.Type_class.t
  val gen : t QCheck2.Gen.t
end

 val run : ?examples:'a list -> (module S with type t = 'a) -> unit
