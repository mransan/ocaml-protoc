open Pbrt_services
module Meta = Meta

let pp_error = Meta.pp_error

module Str_tbl = Hashtbl.Make (struct
  include String

  let hash = Hashtbl.hash
end)

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal : t -> t -> bool = ( = )
  let hash = Hashtbl.hash
end)

let spf = Printf.sprintf

type 'a or_error = ('a, Meta.error) result

let service_prefix package service_name : string =
  let prefix = String.concat "." package in
  if prefix = "" then
    service_name
  else
    spf "%s.%s" prefix service_name

let assemble_meth_name prefix name : string = spf "%s.%s" prefix name

module Server = struct
  let default_spawn f = ignore (Thread.create f () : Thread.t)

  type t = {
    active: bool Atomic.t;
    spawn: (unit -> unit) -> unit;
    handlers: Server.rpc Str_tbl.t;
    sock: [ `Router ] Zmq.Socket.t;
  }

  let stop self =
    if not (Atomic.exchange self.active false) then Zmq.Socket.close self.sock

  let handle_ (self : t) (msg : string list) : unit =
    let encoder = Pbrt.Encoder.create () in

    match msg with
    | [ sender; header; body ] ->
      let header_dec = Pbrt.Decoder.of_string header in
      let header = Meta.decode_pb_meta header_dec in

      (* how to reply *)
      let send_back header msg : unit =
        Pbrt.Encoder.clear encoder;
        Meta.encode_pb_meta header encoder;
        let header = Pbrt.Encoder.to_string encoder in
        Zmq.Socket.send_all ~block:true self.sock [ sender; header; msg ]
      in

      let send_back_err msg =
        Pbrt.Encoder.clear encoder;
        Meta.encode_pb_error { Meta.msg } encoder;
        let body = Pbrt.Encoder.to_string encoder in
        send_back (Meta.default_meta ~id:header.id ~kind:Error ()) body
      in

      (match header.kind, header.meth with
      | Request, Some m ->
        (match Str_tbl.find_opt self.handlers m with
        | None ->
          (* reply with error *)
          Format.eprintf "server: no handler for %S@." m;
          send_back
            (Meta.default_meta ~id:header.id ~kind:Error ())
            "no handler"
        | Some (Server.RPC rpc) ->
          let body_dec = Pbrt.Decoder.of_string body in
          let body = rpc.decode_pb_req body_dec in

          (* how to run the handler *)
          let run_handler () =
            match rpc.f with
            | Unary f ->
              (match f body with
              | res ->
                (* we got the result *)
                let encoder = Pbrt.Encoder.create () in
                rpc.encode_pb_res res encoder;
                let res_str = Pbrt.Encoder.to_string encoder in

                send_back
                  (Meta.default_meta ~id:header.id ~kind:Reply ())
                  res_str
              | exception e ->
                send_back_err
                  (spf "handler failed with exception %s" (Printexc.to_string e)))
            | _ ->
              (* TODO: handle these other cases *)
              send_back_err "cannot handle this kind of handler"
          in

          (* run handler in a background thread/task *)
          self.spawn run_handler)
      | Request, None ->
        send_back_err "missing method name";
        Format.eprintf "server: request: missing method name for %a@."
          Meta.pp_meta header
      | Invalid, _ | Error, _ | Reply, _ ->
        send_back_err "invalid message from client";
        Format.eprintf "server: invalid message of kind=%a@." Meta.pp_kind
          header.kind)
    | _ ->
      Printf.eprintf "server: invalid message with %d frames\n%!"
        (List.length msg)

  let create ?(spawn = default_spawn) ?(active = Atomic.make true) ctx ~addr
      (services : Server.t list) : t =
    let sock = Zmq.Socket.create ctx Zmq.Socket.router in
    Zmq.Socket.bind sock addr;

    (* build a table with all known handlers *)
    let handlers = Str_tbl.create 32 in
    List.iter
      (fun (s : Server.t) ->
        let prefix = service_prefix s.package s.service_name in
        List.iter
          (fun (Server.RPC rpc as handler) ->
            let full_name = assemble_meth_name prefix rpc.name in
            Str_tbl.add handlers full_name handler)
          s.handlers)
      services;

    let server = { active; spawn; handlers; sock } in
    server

  let run (self : t) : unit =
    while Atomic.get self.active do
      let msg = Zmq.Socket.recv_all ~block:true self.sock in
      try handle_ self msg
      with e ->
        Format.eprintf "error while handling message:\n%s\n%!"
          (Printexc.to_string e)
    done;
    Zmq.Socket.close self.sock
end

module Ivar_ = struct
  type 'a t = {
    mutable v: 'a option;
    mutex: Mutex.t;
    cond: Condition.t;
  }

  let create () : _ t =
    { v = None; mutex = Mutex.create (); cond = Condition.create () }

  let set (self : 'a t) (v : 'a) : unit =
    Mutex.lock self.mutex;
    match self.v with
    | None ->
      self.v <- Some v;
      Condition.broadcast self.cond;
      Mutex.unlock self.mutex
    | Some _ -> Mutex.unlock self.mutex

  let wait (self : 'a t) : 'a =
    Mutex.lock self.mutex;
    let rec loop () =
      match self.v with
      | Some x ->
        Mutex.unlock self.mutex;
        x
      | None ->
        Condition.wait self.cond self.mutex;
        loop ()
    in
    loop ()
end

module Client = struct
  (** A in-flight query *)
  type in_flight =
    | In_flight :
        ('req, _, 'res, _) Client.rpc * ('res or_error -> unit)
        -> in_flight

  type t = {
    active: bool Atomic.t;
    sock: [ `Dealer ] Zmq.Socket.t;
    counter: int Atomic.t;  (** to allocate message numbers *)
    in_flight: in_flight Int32_tbl.t;
    mutable background_worker: Thread.t option;
  }

  let background_worker (self : t) : unit =
    while Atomic.get self.active do
      (* use a poll to not block *)
      let poll = Zmq.Poll.of_masks [| Zmq.Poll.mask_in self.sock |] in

      match
        ignore (Zmq.Poll.poll ~timeout:100 poll : _ array);
        Zmq.Socket.recv_all ~block:false self.sock
      with
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> ()
      | exception Unix.Unix_error (err, _, _) ->
        Printf.eprintf "poll failed with error %s\n%!" (Unix.error_message err)
      | [ header; body ] ->
        let header_dec = Pbrt.Decoder.of_string header in
        let header = Meta.decode_pb_meta header_dec in

        (match header.kind with
        | Reply ->
          (match Int32_tbl.find_opt self.in_flight header.id with
          | None -> Format.eprintf "client: no query had id %ld@." header.id
          | Some (In_flight (rpc, cb)) ->
            let body_dec = Pbrt.Decoder.of_string body in
            let body = rpc.decode_pb_res body_dec in
            cb (Ok body))
        | Error ->
          (match Int32_tbl.find_opt self.in_flight header.id with
          | None -> Format.eprintf "client: no query had id %ld@." header.id
          | Some (In_flight (_rpc, cb)) ->
            let body_dec = Pbrt.Decoder.of_string body in
            let err = Meta.decode_pb_error body_dec in
            cb (Error err))
        | Invalid | Request ->
          Format.eprintf "client: invalid message of kind=%a@." Meta.pp_kind
            header.kind)
      | _msg ->
        Printf.eprintf "client : invalid message with %d frames\n%!"
          (List.length _msg)
    done

  let create ctx ~addr : t =
    let sock = Zmq.Socket.create ctx Zmq.Socket.dealer in
    Zmq.Socket.connect sock addr;
    let in_flight = Int32_tbl.create 32 in
    let active = Atomic.make true in
    let client =
      {
        active;
        sock;
        in_flight;
        counter = Atomic.make 0;
        background_worker = None;
      }
    in
    let worker = Thread.create background_worker client in
    client.background_worker <- Some worker;
    client

  let dispose self =
    if Atomic.exchange self.active false then (
      Option.iter Thread.join self.background_worker;
      Zmq.Socket.close self.sock
    )

  let call (self : t) ?timeout:(_ = 30.) (rpc : _ Pbrt_services.Client.rpc) req
      ~on_result : unit =
    (* prepare query *)
    let id = Int32.of_int (Atomic.fetch_and_add self.counter 1) in
    Int32_tbl.add self.in_flight id (In_flight (rpc, on_result));

    let meth =
      let service_prefix = service_prefix rpc.package rpc.service_name in
      assemble_meth_name service_prefix rpc.rpc_name
    in

    let encoder = Pbrt.Encoder.create () in

    let header =
      let header = Meta.default_meta ~id ~meth:(Some meth) ~kind:Request () in
      Meta.encode_pb_meta header encoder;
      Pbrt.Encoder.to_string encoder
    in

    let body =
      Pbrt.Encoder.clear encoder;
      rpc.encode_pb_req req encoder;
      Pbrt.Encoder.to_string encoder
    in

    Zmq.Socket.send_all ~block:true self.sock [ header; body ];
    ()

  let call_block self ?timeout rpc req : _ or_error =
    let res = Ivar_.create () in
    call self ?timeout rpc req ~on_result:(fun r -> Ivar_.set res r);
    Ivar_.wait res
end
