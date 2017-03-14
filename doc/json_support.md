JSON Support
------------

* [Goal](#goal)
* [Possible Approaches](#possible-approaches)
* [Solution Description](#solution-description)
* [Javacript Developer Workflow](#javacript-developer-workflow)
* [Native Developer Workflow](#native-developer-workflow)

### Goal 

1. Support JSON serialization as specififed by [proto 3 guide](https://developers.google.com/protocol-buffers/docs/proto3#json)

2. Support both native/byte code compilation as well as JavaScript using 
   BuckleScript 

3. For JavaScript compilation make use of JavaScript function `JSON.parse`
   rather than an OCaml library to parse JSON. First this built-in function
   should be faster and second it reduces the amount of JavaScript code on 
   the client side. 

### Possible Approaches

1. Generate 2 different code for native and javascript. In native mode, the 
   generated code would be based on the widely popular YoJson libray, while
   in JavaScript code a new file would be generated which would depend
   on the BuckleScript runtime `Js_json`. 

2. Generate Functorized code. The Functor could then be called with 
   either a module based on YoJson or BuckleScript runtime. 

> The current prototype is based on **approach 2 (Functor)**. It makes the generated 
> code **simpler and easier to use** and also the compiler code is shorter. The 
> separation of the JS/Native is clean and it also allows for later having
> a more optimized implementation of the native parsing. 

### Solution Description

**1. JSON Decoder/Encoder Signature**

The runtime library (`pbrt.cmxa`) will contain a new module `Pbrt_js` containing
the module signature required for JSON encoding and decoding:

```OCaml
(** Module signature for a JSON decoder required by the generated code *)
module type Decoder_sig = sig 
  type t
  (** JSON decoder for a Protobuf message *)

  type value = | String of string | Float of float (* ... more value ... *)

  val key : t -> (string * value) option 
  (** [key decoder] returns the next key/value pair in the current JSON 
      object. [None] indicates no more keys are available. *)
end

(** Module signature for a JSON encoder required by the generated code *)
module type Decoder_sig = sig 
  type t 
  (** JSON encoder ofr a Protobuf message *)

  val empty : unit -> t 
  (** [empty ()] creates a new encoder *)

  val set\_string : t -> string -> string -> unit 
  (** [set\_string encoder key value] sets the key/value pair *)

  (** ... more setters *)
module
``` 

**2. [Native] New Runtime Library for JSON**

A new runtime library `pbrt-json.cmxa` will be published in OPAM and contain
an implementation of the JSON Encoder and Decoder module signature. This 
library will be based on YoJSON.

**3. [JavaScript] New Runtime Library for JSON**

A new runtime library will be be published to NPM using BuckleScript. 

### Javacript Developer Workflow

**1. Setup**

```shell
opam install ocaml-protoc 
  # ocaml-protoc compiler is currently only available in opam, alternatively
  # once esy is available it could be installed with it as well
yarn install ocaml-protoc
  # This will install the runtime libraries for the generated code
```

**2. Code Generation**

```shell
ocaml-protoc -json-bs --ml_out src/ src/app.proto
  # The following files are generated
  # - src/app_pb.mli
  # - src/app_pb.ml
```

**3. Using generated code**

```OCaml

module Encoder = App_pb.Make_encoder(Pbrt_json_js.Encoder) 
module Decoder = App_pb.Make_decoder(Pbrt_json_js.Decoder)

let () = 
  let v:App_pb.t = {App_pb.foo  = "foo"; bar = "bar"} in 
  let encoder = Pbrt_json_js.make () in 
  Encoder.encode_t v encoder; 
  let json_str = Pbrt_json_js.to_string encoder in 
  (** send the json str to the client *)
```

### Native Developer Workflow

**1. Setup**

```shell
opam install ocaml-protoc
```

**2. Code Generation**

```shell
ocaml-protoc -json-native --ml_out src/ src/app.proto
  # The following files are generated
  # - src/app_pb.mli
  # - src/app_pb.ml
```

**3. Using generated code**

```OCaml

module Encoder = App_pb.Make_encoder(Pbrt_ocaml_json.Encoder) 
module Decoder = App_pb.Make_decoder(Pbrt_ocaml_json.Decoder)

let () = 
  let v:App_pb.t = {App_pb.foo  = "foo"; bar = "bar"} in 
  let encoder = Pbrt_ocaml_json.make () in 
  Encoder.encode_t v encoder; 
  let json_str = Pbrt_ocaml_json.to_string encoder in 
  (** send the json str to the client *)
```
