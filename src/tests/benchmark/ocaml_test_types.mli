(** Test type implementation *) 

val get_test : Benchmark_types.test_id -> (module Ocaml_test_runner.T_sig) 
(** [get_test test_id] returns the module for the given [test_id] which 
    can then be plugged into the Ocaml_test_runner.Make functor
  *)
