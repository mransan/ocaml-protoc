## Protobuf <-> OCaml Mapping

This page describes how the mapping between protobuf type system and OCaml is done. 

* [Basic Types](#basic-types)
* [Oneof fields](#oneof-fields)
* [Field rules](#field-rules)
* [Message](#message)
* [Enumerations](#enumerations)
* [File name](#file-name) 
* [Package](#package) 
* [Extensions](#extensions)
* [Nested Types](#nested-types)
* [Maps Group Services](#map-group-services)
 
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

An additional simplification is done for empty message used in oneof field; in this case we simply generate a constant constructor simplifying greatly the type:

```Javascript
message string_some {
    message none {
    }
    oneof t {
        none none = 1;
        string some  = 2;
    }
}
```

Will generate the compact OCaml type:

```OCaml
type string_some =
  | None
  | Some of string
```

##### Enumerations

Enumerations are fully supported and will map to OCaml variant with constant constructor.

For example:
```Javascript
enum Corpus {
    UNIVERSAL = 0;
    WEB = 1;
    IMAGES = 2;
    LOCAL = 3;
    NEWS = 4;
    PRODUCTS = 5;
    VIDEO = 6;
  }
```

Will generate:

```OCaml
type corpus =
  | Universal 
  | Web 
  | Images 
  | Local 
  | News 
  | Products 
  | Video 
```

##### File name 

`ocaml-protoc` generate one OCaml file (module) for each protobuf file following a similar convention as protoc:
* &lt;file name&gt;_pb.mli 
* &lt;file name&gt;_pb.ml 

##### [Package](https://developers.google.com/protocol-buffers/docs/proto#packages) 

While `ocaml-protoc` honors the package [compilation rules](https://developers.google.com/protocol-buffers/docs/proto#packages-and-name-resolution) it does not use the package name for the generated OCaml code. Therefore any package semantic or convention is lost in the OCaml code.  

##### [Extensions](https://developers.google.com/protocol-buffers/docs/proto#extensions)

Extensions are parsed by `ocaml-protoc` however they are **ignored**. The main reason is that I have not reached a conclusion as to how they should be represented.

##### [Nested Types](https://developers.google.com/protocol-buffers/docs/proto#nested)

Nested types are fully supported and generate records which name is the concatenation of the inner and outer messages. 

For example:
```Javascript
message ma {
  message mb {
    required int32 bfield = 1; 
  }
  required mb bfield = 1;
}
```

Willl generate:

```OCaml
type ma_mb = {
  mutable bfield : int32;
}

(* ... *)

type ma = {
  mutable bfield : ma_mb;
}
```

##### Maps Groups Services

[Maps](https://developers.google.com/protocol-buffers/docs/proto#maps), [Groups](https://developers.google.com/protocol-buffers/docs/proto#groups) and [Services](https://developers.google.com/protocol-buffers/docs/proto#services) are currently **NOT supported. While groups are most likely never going be supported since they are being deprecated, maps should be relatively easy to add. Services requires a lot more work though, but I think generating an [Mirage Cohttp server](https://github.com/mirage/ocaml-cohttp) would be pretty awesome.
