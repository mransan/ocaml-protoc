# ocaml-protoc [![build](https://github.com/mransan/ocaml-protoc/actions/workflows/main.yml/badge.svg)](https://github.com/mransan/ocaml-protoc/actions/workflows/main.yml)

> :dromedary_camel: **A [protobuf](https://protobuf.dev/) compiler for OCaml :dromedary_camel:.** 

* [Introduction](#introduction)
* [Simple Example](#a-simple-example)
* [Install and Build](#install-and-build)
* [Runtime library](#runtime-library)
* [All Generated Files and Encodings](#all-generated-files-and-encodings)
* [Protobuf <-> OCaml mapping](doc/protobuf_ocaml_mapping.md)
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
| --binary  | Binary encodings | pbrt |
| --yojson | JSON encoding using the widely popular [yojson](https://github.com/mjambon/yojson) library | pbrt_yojson |
| --bs | BuckleScript encoding using the BuckleScript core binding to JS json library | [bs-ocaml-protoc-json][3] |
| --pp | pretty printing functions based on the Format module. | pbrt |
| --services | RPC definitions. | pbrt_services |

[3]:https://www.npmjs.com/package/bs-ocaml-protoc-json

### Protobuf <-> OCaml mapping
see [here](doc/protobuf_ocaml_mapping.md).

### Compiler Internals

see [here](doc/compiler_internals.md)

### Protobuf Extensions
 
see [here](doc/ocaml_extensions.md)

### Benchmarking
 
see [here](doc/benchmarking.md)
