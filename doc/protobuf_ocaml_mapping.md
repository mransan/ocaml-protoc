## Protobuf <-> OCaml Mapping

This page describes how the mapping between protobuf type system and OCaml is done. 

* [Basic Types](#basic-types)
* [Oneof fields](#oneof-fields)
* [Field rules](#field-rules)
* [Default values](#default-values)
* [Message](#message)
* [Enumerations](#enumerations)
* [File name](#file-name) 
* [Package](#package) 
* [Extensions](#extensions)
* [Nested Types](#nested-types)
* [Maps](#maps)
* [Groups Services](#maps-groups-services)
 
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

##### [Default values](https://developers.google.com/protocol-buffers/docs/proto#optional)

`ocaml-protoc` supports the majority of the default values that can be specified in a `.proto` file:
* double/float: Decimal notation (ie 12.345) is supported while scientific notation [is not](https://github.com/mransan/ocaml-protoc/issues/41) (ie 2E8 or -8e2). `nan` and `inf` [are not supported](https://github.com/mransan/ocaml-protoc/issues/45).  
* int types: Decimal notation (ie 123) is supported while [hexadecimal is not](https://github.com/mransan/ocaml-protoc/issues/42) (ie 0xFF) 
* string: default ASCII strings are supported but not [escaped byte notation](https://github.com/mransan/ocaml-protoc/issues/43) (ie \001\002)
* bytes: [not supported](https://github.com/mransan/ocaml-protoc/issues/44) 

##### [Message](https://developers.google.com/protocol-buffers/docs/proto#simple) 

Message are compiled to OCaml `records` with all fields mutable, while `oneof` fields are compiled to OCaml variant.

**Oneof optimization**

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

**Recursive message**

Recursive message are supported and compiled to recursive type in OCaml. For instance the following protobuf:

```Javascript
message IntList {
    message Nil  {  }
    message Cons {
        required int32   value = 1 [(ocaml_type) = int_t] ; 
        required IntList next  = 2;
    }
    oneof t {
        Cons cons = 1;
        Nil  nil  = 2; 
    }
}
```

Will compile to the following OCaml type:

```OCaml
type int_list_cons = {
  value : int;
  next : int_list;
}

and int_list =
  | Cons of int_list_cons
  | Nil
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
  bfield : int32;
}

(* ... *)

type ma = {
  bfield : ma_mb;
}
```

##### [Maps](https://developers.google.com/protocol-buffers/docs/proto#maps)

Maps is fully supported in `ocaml-protoc` and the OCaml type to represent an associative container can be configurable with a field option. 

By default a `map<a, b> = 1` Protobuf field will generate an OCaml list: `('a * 'b) list`. When setting the `(ocaml_container) = hashtbl` in the `.proto` file then it will generate `('a, 'b) Hashtbl.t`. 

**example 1 (default):**
```Javascript
message M {
  map<string, string> s2s = 1;
}
```
will generate 
```OCaml
type m = {
  s2s : (string * string) list;
}
```
**example 2 (Hashtbl.t):**
```Javascript
message M {
  map<string, string> s2s = 1 [(ocaml_container) = hashtbl];
}
```
will generate 
```OCaml
type m = {
  s2s : (string, string) Hashtbl.t;
}
```

> Thanks to [Laurent Mazare](https://github.com/LaurentMazare) for the initial implementation of map fields.

##### Groups and  Services

[Groups](https://developers.google.com/protocol-buffers/docs/proto#groups) and 
[Services](https://developers.google.com/protocol-buffers/docs/proto#services) are currently **NOT** supported. 

While groups are most likely never going be supported since they are being deprecated, maps should be relatively easy to add. 
Services requires a lot more work though, but I think generating an [Mirage Cohttp server](https://github.com/mirage/ocaml-cohttp) would be pretty awesome.
