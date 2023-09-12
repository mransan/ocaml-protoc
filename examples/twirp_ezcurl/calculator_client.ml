let spf = Printf.sprintf
let aspf = Format.asprintf

let () =
  let port = try int_of_string (Sys.getenv "PORT") with _ -> 8080 in
  Printf.printf "query on http://localhost:%d/\n%!" port;

  let r =
    match
      Twirp_ezcurl.call ~use_tls:false ~host:"localhost" ~port
        Calculator.Calculator.add
      @@ Calculator.default_add_req ~a:31l ~b:100l ()
    with
    | Ok x -> x.value |> Int32.to_int
    | Error err ->
      failwith (aspf "call to add failed: %a" Twirp_ezcurl.pp_error err)
  in

  Printf.printf "add call: returned %d\n%!" r;
  ()
