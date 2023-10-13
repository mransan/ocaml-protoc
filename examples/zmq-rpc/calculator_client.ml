let spf = Printf.sprintf
let aspf = Format.asprintf
let ( let@ ) = ( @@ )

let () =
  let url = ref "tcp://localhost:8123" in
  let opts = [ "--url", Arg.Set_string url, " server address" ] |> Arg.align in
  Arg.parse opts ignore "";

  Printf.printf "query on %s/\n%!" !url;

  let ctx = Zmq.Context.create () in
  let@ () = Fun.protect ~finally:(fun () -> Zmq.Context.terminate ctx) in
  let client = Pbrt_zmq.Client.create ctx ~addr:!url in

  let r =
    match
      Pbrt_zmq.Client.call_block client Calculator.Calculator.add
      @@ Calculator.default_add_req ~a:31l ~b:100l ()
    with
    | Ok x -> x.value |> Int32.to_int
    | Error err ->
      failwith (aspf "call to add failed: %a" Pbrt_zmq.pp_error err)
  in

  Printf.printf "add call: returned %d\n%!" r;
  Pbrt_zmq.Client.dispose client;
  ()
