let () =
  (* Here we show how we can aggregate tests from several files and package them
     into a single executable. Individual tests may be added with specific
     inputs if needed, as demonstrated below. This is just for the sake of the
     example and documenting what may be done. To be adapted to your specific
     use case. *)
  QCheck_runner.run_tests_main
    (List.flatten
       [
         Messages.quickcheck_tests_error
           ~examples:[ { error = "Hello Error" } ]
           ();
         Messages.all_quickcheck_tests ();
       ])
