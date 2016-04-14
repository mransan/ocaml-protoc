## OCaml extensions 

In order to make the OCaml code generated more OCaml friendly several extensions are being introduced. 

#### OCaml type : `(ocaml_type)`

For int type, `ocaml-protoc` generates either `int32` or `int64` depending on the encoding size. This is to ensure by default 
correct behavior without any assumption on:
* max value of the integer value 
* platform on which the software is running

However the application developer might very well know that the OCaml int type will work perfectly fine for certain fields. This is for example the case for any 32 bit protobuf type when compiled with 64 bits. We are therefore introducing the `[(ocaml_type) = int]` extension for fields. 

Find below the example from the introduction slightly modified to make sure `int` type is used:

```Javascript
import "ocaml-protoc/ocamloptions.proto";

message Person {
    required string name = 1;
    required int32 id = 2 [(ocaml_type) = int_t];
    optional string email = 3;
    repeated string phone = 4;
}
``` 

The generated type will now be:

```OCaml
type person = {
  name : string;
  id : int;
  email : string option;
  phone : string list;
}
```

#### Mutable field : `(ocaml_mutable)`

By default generated record field are immutable. `ocaml-protoc` support a custom boolean field option: `(ocaml_mutable)` to indicate that the field should be mutable in OCaml. 

For instance the following `.proto` file:
```Javascript
message m {
  required int32 f = 1 [(ocaml_mutable) = true];
```
Will generate the following OCaml type:
```OCaml
type m = {
  mutable f : int32; 
}
```

#### Repeated_field type : `(ocaml_container)`

By default a protobuf repeated field generates a list type; for better performance the runtime library `pbrt.cmxa` contains a dedicated `deque` type called `Repeated_field.t`. This type is optimized for append and therefore will speed the decoding of the protobuf message significantly. (You can check the [benchmark](doc/benchmark.md) section for more details. 

By using the `(ocaml_container) = repeated_field` the .proto file author can indicate to `ocaml-protoc` to use the `Pbrt.Repeated_field.t` type. 

For instance the following `.proto` file:
```Javascript
message m {
  repeated int32 f = 1 [(ocaml_container) = repeated_field];
```
Will generate:
```OCaml
type m = {
  f : int32 Repeated_field.t; 
```

#### Hashtbl : `(ocaml_container)`

By default a protobuf `map<a, b>` field is mapped to an associative list in OCaml. This mapping can be overriden by using the `(ocaml_container)` field option and setting it to `hashtbl` value. 

For instance:
```Javascript
message M {
  map<string, string> s2s = 1 [(ocaml_container) = hashtbl];
}
```
Will generate:
```OCaml
type m = {
  s2s : (string, string) Hashtbl.t;
}
```
