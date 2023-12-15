# ocaml-protoc [![build](https://github.com/mransan/ocaml-protoc/actions/workflows/main.yml/badge.svg)](https://github.com/mransan/ocaml-protoc/actions/workflows/main.yml)

> :dromedary_camel: **A [protobuf](https://protobuf.dev/) compiler for OCaml :dromedary_camel:.** 

* [Introduction](#introduction)
* [Simple Example](#a-simple-example)
* [Install and Build](#install-and-build)
* [Runtime library](#runtime-library)
* [All Generated Files and Encodings](#all-generated-files-and-encodings)
* [Protobuf <-> OCaml mapping](doc/protobuf_ocaml_mapping.md)
* [Services](#services)
* [Compiler Internals](doc/compiler_internals.md)
* [Protobuf extensions](doc/ocaml_extensions.md)
* [Benchmarking](doc/benchmarking.md)

### Introduction 

⇨ `ocaml-protoc` compiles [protobuf message files](https://goo.gl/YqNT7Q) into 
**OCaml types** along with **serialization functions** for a variety of encodings.

⇨ `ocaml-protoc` supports **both** proto syntax **2 and 3** as well as **binary** and **JSON** encodings. 

⇨ `ocaml-protoc` supports **JavaScript** object encoding through the  **BuckleScript 
compiler**. See [here](https://github.com/mransan/bs-protobuf-demo) for complete example.



### A simple example

> This example generates the binary encoding, if you are more interested in a **JavaScript** 
> example, go [here](https://github.com/mransan/bs-protobuf-demo)

*  **Write** in `example.proto`

```Protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
  repeated string phone = 4;
}
```

* **Run:**

```bash
$ ocaml-protoc --binary --ml_out ./ example.proto
.. Generating example.mli
.. Generating example.ml
```

* **example.mli**:

```OCaml
(** example.proto Generated Types *)

(** {2 Types} *)

type person = {
  name : string;
  id : int32;
  email : string;
  phone : string list;
}

(** {2 Default values} *)

val default_person : 
  ?name:string ->
  ?id:int32 ->
  ?email:string ->
  ?phone:string list ->
  unit ->
  person
(** [default_person ()] is the default value for type [person] *)

(** {2 Protobuf Encoding} *)

val encode_pb_person : person -> Pbrt.Encoder.t -> unit
(** [encode_pb_person v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_person : Pbrt.Decoder.t -> person
(** [decode_pb_person decoder] decodes a [person] binary value from [decoder] *)
```

* in `main.ml`, write the following to encode a person value and save it to a file: 

```OCaml
let () =

  (* Create OCaml value of generated type *) 
  let person = Example.({ 
    name = "John Doe"; 
    id = 1234l;
    email = Some "jdoe@example.com"; 
    phone = ["123-456-7890"];
  }) in 
  
  (* Create a Protobuf encoder and encode value *)
  let encoder = Pbrt.Encoder.create () in 
  Example.encode_pb_person person encoder; 

  (* Output the protobuf message to a file *) 
  let oc = open_out "myfile" in 
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
```

* then in the same `main.ml` append the following to read from the same file:

```OCaml
let () = 
  (* Read bytes from the file *) 
  let bytes = 
    let ic = open_in "myfile" in 
    let len = in_channel_length ic in 
    let bytes = Bytes.create len in 
    really_input ic bytes 0 len; 
    close_in ic; 
    bytes 
  in 
  
  (* Decode the person and Pretty-print it *)
  Example.decode_pb_person (Pbrt.Decoder.of_bytes bytes)
```

* :heavy_exclamation_mark: **Int32** vs **int**

*OCaml users will immediately point to the use of `int32` type in the generated code which might not be the most convenient choice.
One can modify this behavior using [custom extensions](doc/ocaml_extensions.md).* 

### Install & Build

**Prerequesite**

`ocaml-protoc` only depends on
* the OCaml compiler distribution (byte code/native compiler).
* dune
* stdlib-shims for the compiler itself
* a C99 compiler for the runtime library's stubs

**Intall from OPAM**

```bash
$ opam install ocaml-protoc
```

**Or from source**

```bash
$ mkdir -p tmp/bin
$ export PREFIX=`pwd`/tmp
$ make install
```

**Build your program** 

Using dune, the program can be compiled with:

```
(executable
  (name main)
  (modules main example)
  (libraries pbrt))
```

More manually, the program can be built directly using [ocamlfind](http://projects.camlcity.org/projects/findlib.html):

```Bash
$ ocamlfind ocamlopt -linkpkg -package pbrt \
    -o example \
    example.mli example.ml \
    main.ml
```

🏁 You can now run the example
```Bash
$ ./example
```

### Runtime library

The generated code depends on the opam package "pbrt", defining
a module `Pbrt`.

Online documentation [here](https://mransan.github.io/ocaml-protoc/dev/pbrt/Pbrt/index.html)

### All Generated Files and Encodings:

| Command line switch | Description | Runtime |
| ------------- | ------------- | ----------|
| | Type definition along with a `default` constructor function to conveniently create values of that type | |
| --make | `make` constructor functions |  |
| --binary  | Binary encodings | `pbrt` |
| --yojson | JSON encoding using the widely popular [yojson](https://github.com/mjambon/yojson) library | `pbrt_yojson` |
| --bs | BuckleScript encoding using the BuckleScript core binding to JS json library | [bs-ocaml-protoc-json][3] |
| --pp | pretty printing functions based on the Format module. | `pbrt` |
| --services | RPC definitions. | `pbrt_services` |

[3]:https://www.npmjs.com/package/bs-ocaml-protoc-json

### Protobuf <-> OCaml mapping
see [here](doc/protobuf_ocaml_mapping.md).

### Services

With the `--services` option, ocaml-protoc now generates stubs for service
declarations.

For example with the given `calculator.proto` file:

```proto
syntax = "proto3";

message I32 {
  int32 value = 0;
}

message AddReq {
  int32 a = 1;
  int32 b = 2;
}

service Calculator {
  rpc add(AddReq) returns (I32);

  rpc add_stream(stream I32) returns (I32);
}
```

Using `ocaml-protoc --binary --services --ml_out=. calculator.proto`, we get the normal
type definitions, but also this service definition:

```ocaml
(** Calculator service *)
module Calculator : sig
  open Pbrt_services
  open Pbrt_services.Value_mode

  module Client : sig
    val add : (add_req, unary, i32, unary) Client.rpc
    val add_stream : (i32, stream, i32, unary) Client.rpc
  end
  
  module Server : sig
    (** Produce a server implementation from handlers *)
    val make : 
      add:((add_req, unary, i32, unary) Server.rpc -> 'handler) ->
      add_stream:((add_req, stream, i32, unary) Server.rpc -> 'handler) ->
      unit -> 'handler Pbrt_services.Server.t
  end
end
```

This can then potentially be used with libraries that implement specific protobuf-based
network protocols, such as [ocaml-grpc](https://github.com/dialohq/ocaml-grpc)
or [ocaml-twirp](https://github.com/c-cube/ocaml-twirp), or other custom protocols.

Protobuf service endpoints take a single type and return a single type, but they have the ability
to stream either side. We represent this ability with the `Pbrt_services.Value_mode` types:

```ocaml
(** Whether there's a single value or a stream of them *)
module Value_mode = struct
  type unary
  type stream
end
```

#### Client-side

A `(req, req_kind, res, res_kind) Client.rpc` is a bundle describing a single RPC endpoint,
from the client perspective. It contains the RPC name, service, etc. alongside encoders for
the request type `req`, and decoders for the response type `res`.

The phantom types `req_kind` and `res_kind` represent the value mode for request,
respectively response. Here we see that `Calculator.Client.add` is unary for both
(it takes a single argument and returns a single value)
but `Calculator.Client.add_stream` takes a string of `i32` as parameters before
returning a single result.

With transports such as grpc, all [4 combinations](https://grpc.io/docs/what-is-grpc/core-concepts/#rpc-life-cycle)
are possible. With twirp over HTTP 1.1, only unary mode is supported.

#### Server-side

On the server side, ocaml-protoc generates individual stubs,
like on the client side; but it also generates _services_ as bundles
of endpoints. One service corresponds to a `service` declaration
in the `.proto` file.

<details open>
<summary>
Detailed explanation of how server-side services work
</summary>

In practice, in something like twirp, a service could be added to a web server
by adding each endpoint to a single HTTP route; or a twirp-aware router could
directly map incoming HTTP queries to services.

The trickiest part here is that the type `'handler Pbrt_services.Server.t` is
parametric. Indeed it'd be hard for the generated code to cater to every possible
combination of network transport and concurrency library (eio, lwt, async, etc.).

Instead, the code is generic over `'handler` (the type of a query handler for a _single_
endpoint; e.g. a HTTP endpoint for a single route). The function
```ocaml
  module Server : sig
    val make : 
      add:((add_req, unary, i32, unary) Server.rpc -> 'handler) ->
      add_stream:((add_req, stream, i32, unary) Server.rpc -> 'handler) ->
      unit -> 'handler Pbrt_services.Server.t
  end
```
seen previously is used to build the `'handler service` by asking the user
to provide a handler for each method. The builder for `add` is given a
description of the `add` endpoint (with decoders for requests; and encoders
for responses), and must return a handler that knows how to decode the request,
add numbers, and turn that back into a response.

Libraries will provide facilities to build such handlers, so that the user
only has to provide the actual logic (here, adding numbers). For example
in `twirp_tiny_httpd` (part of `ocaml-twirp`), implementing a
server looks like this[^1]:

[^1]: we use a different `.proto` because twirp doesn't handle streams.

```proto
syntax = "proto3";

message I32 {
  int32 value = 0;
}

message AddReq {
  int32 a = 1;
  int32 b = 2;
}

message AddAllReq {
  repeated int32 ints = 1;
}

service Calculator {
  rpc add(AddReq) returns (I32);

  rpc add_all(AddAllReq) returns (I32);
}
```

```ocaml
let add (a : add_req) : i32 = default_i32 ~value:Int32.(add a.a a.b) ()

let add_all (a : add_all_req) : i32 =
  let l = ref 0l in
  List.iter (fun x -> l := Int32.add !l x) a.ints;
  default_i32 ~value:!l ()

let calc_service : Twirp_tiny_httpd.handler Pbrt_services.Server.t =
  Calculator.Server.make
    ~add:(fun rpc -> Twirp_tiny_httpd.mk_handler rpc add)
    ~add_all:(fun rpc -> Twirp_tiny_httpd.mk_handler rpc add_all)
    ()

let() =
  let server = Tiny_httpd.create ~port:1234 () in
  Twirp_tiny_httpd.add_service ~prefix:(Some "twirp") server calc_service;
  Tiny_httpd.run_exn server
```

Here we see that all the logic is in `add` and `add_all`, which know nothing
about protobuf or serialization. A `calc_service` bundle, using the `Twirp_tiny_httpd.handler`
type for each handler, is built from them. Finally, a HTTP server is created,
the service is added to it (binding some routes), and we enter the
server's main loop.

</details>


### Compiler Internals

see [here](doc/compiler_internals.md)

### Protobuf Extensions
 
see [here](doc/ocaml_extensions.md)

### Benchmarking
 
see [here](doc/benchmarking.md)
