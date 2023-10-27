module H = Tiny_httpd
open Calculator

module Service_impl = struct
  let add (a : add_req) : i32 = default_i32 ~value:Int32.(add a.a a.b) ()

  let add_all (a : add_all_req) : i32 =
    let l = ref 0l in
    List.iter (fun x -> l := Int32.add !l x) a.ints;
    default_i32 ~value:!l ()

  let n_pings = ref 0
  let ping () = incr n_pings
  let get_pings () : i32 = default_i32 ~value:(Int32.of_int !n_pings) ()
end

let calc_service : Twirp_tiny_httpd.handler Pbrt_services.Server.t =
  Calculator.make_server
    ~add:(fun rpc -> Twirp_tiny_httpd.mk_handler rpc Service_impl.add)
    ~add_all:(fun rpc -> Twirp_tiny_httpd.mk_handler rpc Service_impl.add_all)
    ~ping:(fun rpc -> Twirp_tiny_httpd.mk_handler rpc Service_impl.ping)
    ~get_pings:(fun rpc ->
      Twirp_tiny_httpd.mk_handler rpc Service_impl.get_pings)
    ()

let () =
  let port = try int_of_string (Sys.getenv "PORT") with _ -> 8080 in
  Printf.printf "listen on http://localhost:%d/\n%!" port;

  let server = H.create ~port () in
  Twirp_tiny_httpd.add_service ~prefix:(Some "twirp") server calc_service;

  H.run_exn server
