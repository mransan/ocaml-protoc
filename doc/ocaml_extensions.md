## OCaml extensions 

In order to make the OCaml code generated more OCaml friendly several extensions are being introduced. 

### OCaml type 

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
  mutable name : string;
  mutable id : int;
  mutable email : string option;
  mutable phone : string list;
}
```
