# ocaml-protoc

> :dromedary_camel: **A [protobuf](https://goo.gl/YqNT7Q) compiler for OCaml :dromedary_camel:.** 

* [Introduction](#introduction)
* [Simple Example](#a-simple-example)
* [Install and Build](#install-and-build)
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
❯ ocaml-protoc -binary -ml_out ./ example.proto
.. Generating example_types.mli
.. Generating example_types.ml
.. Generating example_pb.mli
.. Generating example_pb.ml
```

* **example_types.mli**:

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
```

* **example_pb.mli**:

```OCaml
(** {2 Protobuf Encoding} *)

val encode_person : Example_types.person -> Pbrt.Encoder.t -> unit
(** [encode_person v encoder] encodes [v] with the given [encoder] *)

(** {2 Protobuf Decoding} *)

val decode_person : Pbrt.Decoder.t -> Example_types.person
(** [decode_person decoder] decodes a [person] value from [decoder] *)
```

* You can then use this OCaml module in your application to populate, serialize, and retrieve `person` protocol buffer messages: 

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

* Then later on you can read your message back in:

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

:heavy_exclamation_mark: Int32 vs int

*OCaml users will immediately point to the use of `int32` type in the generated code which might not be the most convenient choice. One can modify this behavior using [custom extensions](doc/ocaml_extensions.md).* 

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

### All Generated Files and Encodings:

| file name | Command line switch | Description | Runtime | 
| ------------- | ------------- | ----------| ------- |
| \<name\>_**types.**{ml\|mli} |  | Type definition along with a constructor function to conveniently create values of that type | | 
| \<name\>_**pb.**{ml\|mli}  | -binary  | Binary encodings | [ocaml-protoc][1] |
| \<name\>_**yojson.**{ml\|mli} | -yojson | JSON encoding using the widely popular [yojson](https://github.com/mjambon/yojson) library | [ocaml-protoc-yojson][2] |
| \<name\>_**bs.**{ml\|mli} | -bs | BuckleScript encoding using the BuckleScript core binding to JS json library | [bs-ocaml-protoc-json][3] | 
|  \<name\>_**pp.**{ml\|mli} | -pp | pretty printing functions based on the Format module. | [ocaml-protoc][1] |

[1]:http://opam.ocaml.org/packages/ocaml-protoc/
[2]:http://opam.ocaml.org/packages/ocaml-protoc-yojson/
[3]:https://www.npmjs.com/package/bs-ocaml-protoc-json

### Protobuf <-> OCaml mapping
see [here](doc/protobuf_ocaml_mapping.md).

### Compiler Internals

see [here](doc/compiler_internals.md)

### Protobuf Extensions
 
see [here](doc/ocaml_extensions.md)

### Benchmarking
 
see [here](doc/benchmarking.md)
