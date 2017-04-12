# ocaml-protoc

> A [protobuf](https://goo.gl/YqNT7Q) compiler for OCaml. 

* [Introduction](#introduction)
* [Simple Example](#a-simple-example)
* [Install & Build](#install-build)
* [Protobuf <-> OCaml mapping](doc/protobuf_ocaml_mapping.md)
* [Compiler Internals](doc/compiler_internals.md)
* [Protobuf extensions](doc/ocaml_extensions.md)
* [Benchmarking](doc/benchmarking.md)

### Introduction 

`ocaml-protoc` compiles [protobuf message files](https://goo.gl/YqNT7Q) into 
OCaml types along with serialization functions for a variety of encodings.

`ocaml-protoc` supports **both** proto syntax **2 and 3** as well as **binary** and **JSON** encodings. 

Since OCaml also supports compilation to **JavaScript** with the **BuckleScript 
compiler**, `ocaml-protoc` generates dedicated serialization functions for
it, relying on the JavaScript built-in JSON support. See 
[here](https://github.com/mransan/bs-protobuf-demo) for complete example.

For each `.proto` file, `ocaml-protoc` will generate multiple OCaml files, 
depending on command line argument switch:
* **\<proto file name\>_types.{ml|mli}**: contains the type definition along 
  with a constructor function to conveniently create values of that type. Those
  files are **always** generated. The generated code might rely on 
  the `ocaml-protoc` runtime library.
* **\<proto file name\>_pb.{ml|mli}**: contains the binary encoding, those files 
  are triggered by the `-binary` command line switch. The generated code
  depends on the `ocaml-protoc` runtime library.
* **\<proto file name\>_yosjon.{ml|mli}**: contains the JSON encoding using the 
  widely popular [yojson](https://github.com/mjambon/yojson) library. Those 
  files are triggered by the `-yojson` command line switch. The generated code 
  depends solely on the `ocaml-protoc-yojson` library.
* **\<proto file name\>_bs.{ml|mli}**: contains the BuckleScript encoding using the
  BuckleScript core binding to JS json library. Those files are triggered by 
  the `-bs` command line switch.
* **\<proto file name\>_pp.{ml|mli}**: contains pretty printing functions based
  on the Format module. Those files are triggered by the `-pp` command 
  line switch.

### A simple example

Let's take a similar example as the [google one](https://developers.google.com/protocol-buffers/docs/overview#how-do-they-work):

```Protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
  repeated string phone = 4;
}
```

The following OCaml code will get generated after running:

```bash
❯ ocaml-protoc -binary -ml_out ./ example.proto`
```

**example01_types.mli**:

```OCaml
(** example01.proto Generated Types *)

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
```

**example01_pb.mli**:

```OCaml
(** {2 Protobuf Encoding} *)

val encode_person : Example01_types.person -> Pbrt.Encoder.t -> unit
(** [encode_person v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_person : Pbrt.Decoder.t -> Example01_types.person
(** [decode_person decoder] decodes a [person] value from [decoder] *)
```

You can then use this OCaml module in your application to populate, serialize, and retrieve `person` protocol buffer messages.
For example:

```OCaml
let () =

  (* Create OCaml value of generated type *) 
  let person = Example_types.({ 
    name = "John Doe"; 
    id = 1234l;
    email = Some "jdoe@example.com"; 
    phone = ["123-456-7890"];
  }) in 
  
  (* Create a Protobuf encoder and encode value *)
  let encoder = Pbrt.Encoder.create () in 
  Example_pb.encode_person person encoder; 

  (* Output the protobuf message to a file *) 
  let oc = open_out "myfile" in 
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
```

Then later on you can read your message back in:
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
  Example_pb.decode_person (Pbrt.Decoder.of_bytes bytes)
```

**ℹ** *OCaml users will immediately point to the use of `int32` type in the generated code which might not be the most convenient choice. One can modify this behavior using [custom extensions](doc/ocaml_extensions.md).* 

### Install & Build

**Prerequesite**

`ocaml-protoc` depends on :
* the OCaml compiler distribution (byte code/native compiler and ocamlbuild).
* [ppx_deriving_protobuf](https://github.com/whitequark/ppx_deriving_protobuf) for the generated code runtime.

**Intall from OPAM**

```bash
❯ opam install ocaml-protoc
```

**Install from source with [ocamlfind](http://projects.camlcity.org/projects/findlib.html)**

```bash
❯ mkdir -p tmp/bin
❯ export PREFIX=`pwd`/tmp
❯ make install
```

**Build your program** 

Here are the steps to build the example above where the source are in `src/examples/`. We now assume that `$PREFIX/bin` is in your path.

```Bash 
❯ ocaml-protoc -binary -ml_out ./ example01.proto
```

When using `findlib`:
```Bash
❯ ocamlfind ocamlopt -linkpkg -package ocaml-protoc \
    -o example01 \
    example01_types.mli example01_types.ml \
    example01_types.mli example01_types.ml \
    example01.ml
```

🏁 You can now run the example
```Bash
❯ ./example01
```
### Protobuf <-> OCaml mapping
see [here](doc/protobuf_ocaml_mapping.md).

### Compiler Internals

see [here](doc/compiler_internals.md)

### Protobuf Extensions
 
see [here](doc/ocaml_extensions.md)

### Benchmarking
 
see [here](doc/benchmarking.md)
