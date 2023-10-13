let ( let@ ) = ( @@ )

let calc_service =
  Calculator.service_impl_of_calculator
    (module struct
      open Calculator

      let add (a : add_req) : i32 = default_i32 ~value:(Int32.add a.a a.b) ()

      let add_all (a : add_all_req) : i32 =
        let l = ref 0l in
        List.iter (fun x -> l := Int32.add !l x) a.ints;
        default_i32 ~value:!l ()

      let n_pings = ref 0
      let ping () = incr n_pings
      let get_pings () : i32 = default_i32 ~value:(Int32.of_int !n_pings) ()
    end)

let () =
  let url = ref "tcp://0.0.0.0:8123" in
  let opts =
    [ "--url", Arg.Set_string url, " listening endpoint" ] |> Arg.align
  in
  Arg.parse opts ignore "";

  Printf.printf "listen on %s\n%!" !url;

  let ctx = Zmq.Context.create () in
  let@ () = Fun.protect ~finally:(fun () -> Zmq.Context.terminate ctx) in

  let server = Pbrt_zmq.Server.create ctx ~addr:!url [ calc_service ] in
  Pbrt_zmq.Server.run server
