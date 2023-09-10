module H = Tiny_httpd

let calc_service =
  Calculator.service_impl_of_calculator
    (module struct
      open Calculator

      let add (a : add_req) : i32 = default_i32 ~value:Int32.(add a.a a.b) ()

      let add_all (a : add_all_req) : i32 =
        let l = ref 0l in
        List.iter (fun x -> l := Int32.add !l x) a.ints;
        default_i32 ~value:!l ()

      let n_pings = ref 0
      let ping () = incr n_pings
      let get_pings () : i32 = default_i32 ~value:(Int32.of_int !n_pings) ()
    end)

let () =
  let port = try int_of_string (Sys.getenv "PORT") with _ -> 8080 in
  Printf.printf "listen on http://localhost:%d/\n%!" port;

  let server = H.create ~port () in
  Twirp_tiny_httpd.add_service ~prefix:(Some "twirp") server calc_service;

  H.run_exn server
