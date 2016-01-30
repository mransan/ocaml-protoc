# ocaml-protoc

A [protobuf](https://developers.google.com/protocol-buffers/) compiler for OCaml. 

* [Introduction](#introduction)
* [Simple Example](#a-simple-example)
* [Build/Install](#build-install)
* [Protobuf <-> OCaml mapping](doc/protobuf_ocaml_mapping.md)
* [Compiler Internals](doc/compiler_internals.md)


### Introduction 

`ocaml-protoc` compiles [protobuf message files](https://developers.google.com/protocol-buffers/docs/proto) into 
[OCaml modules](http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html). Each message/enum/oneof protobuf type 
will have a corresponding OCaml type along with the following functions:
* `encode_<type>` : encode the generated type to `bytes` following protobuf specification
* `decode_<type>` : decode the generated type from `bytes` following protobuf specification
* `default_<type>` : default value honoring [protobuf default attributes](https://developers.google.com/protocol-buffers/docs/proto#optional) or [protobuf version 3 default rules](https://developers.google.com/protocol-buffers/docs/proto3#default) 
* `pp_<type>` : pretty print of the OCaml type

### A simple example

Let's take a similar example as the [google one](https://developers.google.com/protocol-buffers/docs/overview#how-do-they-work):

```Javascript
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
  repeated string phone = 4;
}
```
The following OCaml code will get generated after running `ocaml-protoc -ml_out ./ example.proto`
```OCaml
type person = {
  mutable name : string;
  mutable id : int32;
  mutable email : string option;
  mutable phone : string list;
}

val decode_person : Pbrt.Decoder.t -> person
(** [decode_person decoder] decodes a [person] value from [decoder] *)

val encode_person : person -> Pbrt.Encoder.t -> unit
(** [encode_person v encoder] encodes [v] with the given [encoder] *)

val pp_person : Format.formatter -> person -> unit 
(** [pp_person v] formats v] *)

val default_person : unit -> person
(** [default_person ()] is the default value for type [person] *)
```

You can then use this OCaml module in your application to populate, serialize, and retrieve `person` protocol buffer messages.
For example:

```OCaml
let () =

  (* Create OCaml value of generated type *) 
  let person = Example_pb.({ 
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
  let person = Test14_pb.decode_person (Pbrt.Decoder.of_bytes bytes) in 
  Format.fprintf Format.std_formatter "%a" Test14_pb.pp_person person
```

*OCaml users will immediately point to the use of `int32` type in the generated code which might not be the most convenient choice. One can modify this behavior using [custom extensions](doc/ocaml_extensions.md).* 

### Build-Install

**Prerequesite**

`ocaml-protoc` solely depends on the OCaml compiler distribution (byte code/native compiler and ocamlbuild).

**Install from source**

Assuming you want to install `ocaml-protoc` in `tmp` directory inside source directory simply do:

```bash
export PREFIX=`pwd`/tmp
make install
```

You should now see the following:
```bash
tmp//bin/ocaml-protoc                           #symbolic link to the native executable
tmp//bin/ocaml-protoc.native     
tmp//include/ocaml-protoc/ocamloptions.proto    #protobuf extensions for OCaml code 
tmp//lib/pbrt.a
tmp//lib/pbrt.cma
tmp//lib/pbrt.cmi
tmp//lib/pbrt.cmt
tmp//lib/pbrt.cmti
tmp//lib/pbrt.cmxa
tmp//lib/pbrt.cmxs
tmp//lib/pbrt.mli
```

*Note that if you only want the byte code installation use `make install.byte`*

**Intall from OPAM**

Installation from OPAM will be provided soon.

**Build your program** 

Here are the steps to build the example above where the source are in `src/examples/`:

```Bash
# Generate the OCaml protobuf module 
../../tmp/bin/ocaml-protoc -ml_out ./  example01.proto

# Compile the example including the ocaml protobuf runtime (pbrt.cmxa)
ocamlopt.opt \
  -I ../../tmp/lib/ -I ./  \
  pbrt.cmxa \
  -o example01 \
  example01_pb.mli example01_pb.ml example01.ml
```
You can now run the example
```Bash
./example01
```
### Protobuf <-> OCaml mapping
see [here](doc/protobuf_ocaml_mapping.md).

### Compiler Internals

see [here](doc/compiler_internals.md)
