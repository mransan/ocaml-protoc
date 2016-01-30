## Protobuf <-> OCaml Mapping

This page describes how the mapping between protobuf type system and OCaml is done. 

* [Basic Types](#basic-types)
* [Oneof fields](#oneof-fields)
* [Field rules](#field-rules)
* [Message](#message)
* [File name](#file-name) 
* [Package](#package) 
 
##### [Basic Types](https://developers.google.com/protocol-buffers/docs/proto#scalar)

| .proto type  | OCaml Type  | [Extensions](ocaml_extensions.md) | Notes |
|--------------|-------------|------------|-------|
| double       | float       |            |       |
| float        | float       |            |       | 
| int32        | int32       |  int       |       |
| int64        | int64       |  int       |       |
| uint32       | int32       |  int       |       |
| uint64       | int64       |  int       |       |
| sint32       | int32       |  int       |       |
| sint64       | int64       |  int       |       |
| fixed32      | int32       |  int       |       |
| fixed64      | int64       |  int       |       |
| sfixed32     |             |            | This encoding is not supported |
| sfixed64     |             |            | This encoding is not supported |
| bool         | bool        |            |  |
| string       | string      |            |  |
| bytes        | bytes       |            |  |


##### [Oneof fields](https://developers.google.com/protocol-buffers/docs/proto#oneof)

`oneof` fields are encoded as OCaml `variant`. The variant name is the concatenation of the enclosing message name 
and the `oneof` field name.

*Note that since it's not possible to encode the variant type without being part of a message, no encoding/decoding
functions are generated.*

##### [Field rules](https://developers.google.com/protocol-buffers/docs/proto#specifying-field-rules)

`optional` field will generate `option` type in OCaml, while `repeated` field will generate OCaml `list`.


##### [Message](https://developers.google.com/protocol-buffers/docs/proto#simple) 

Message are encoded as OCaml `records` with all fields mutable. 

Note that if the protobuf message only contains a single `oneof` field then a single `variant` will be generated. 
This simplify greatly the generated code; for instance:

```Javascript
message IntOrString {
    oneof t {
        int32  intVal    = 1;
        string stringVal = 2;
    }
}

```
will generate the compact representation:

```OCaml
type int_or_string =
  | Int_val of int32
  | String_val of string
```

##### File name 

`ocaml-protoc` generate one OCaml file (module) for each protobuf file following a similar convention as protoc:
* &lt;file name&gt;_pb.mli 
* &lt;file name&gt;_pb.ml 

##### [Package](https://developers.google.com/protocol-buffers/docs/proto#packages) 

While `ocaml-protoc` honors the package [compilation rules](https://developers.google.com/protocol-buffers/docs/proto#packages-and-name-resolution) it does not use the package name for the generated OCaml code. Therefore any package semantic or convention is lost in the OCaml code.  
