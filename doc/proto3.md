## Adding support for proto3 syntax work

* [References](#references) 
* [Major Differences](#major-differences)
* [Work Items](#work-items)

#### References

* [Proto3 language guide](https://developers.google.com/protocol-buffers/docs/proto3) 
* [Proto2 language guide](https://developers.google.com/protocol-buffers/docs/proto)
* [Proto3 language spec](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
* [Proto2 language spec](https://developers.google.com/protocol-buffers/docs/reference/proto2-spec)
* [default value discussion](http://stackoverflow.com/questions/33204321/upgrading-protobuf-from-version-2-to-3-incompatible-with-protobuf-default-valu)
* [Protobuf release page](https://github.com/google/protobuf/releases)

#### Major Differences

1. `required` and `optional` are removed

Proto3 defines a new semantic for non-repeated field. No fields can be required in proto3 however 
the semantic of the new proto3 field is not exactly the same as the optional proto2:

* On the wire proto3 fields are the same as proto2. They are optional.
* proto3 fields have hard-coded and well-defined **default value**. Unlike proto2 
  default value cannot be customized with a field option. 
* Generated code no longer has method such as `is_<field_set()` for application
  to know if the value was set or not when the message was encoded.

> Note that there is currently a bug in `ocaml-protoc` in which required fields
> are actually required during decoding and that the natural default value 
> is used instead. Interestingly this is the behavior one would expect in 
> proto3.

2. Addition of the syntax top level statement in the `.proto` file 

In order to enable the proto3 semantic one can write the following:

```Protobuf
syntax = "proto3";
```

3. Group are no longer supported

`ocaml-protoc` never supported those so no change.

4. Removal of unknown field 

The generated code for proto2 used to preserve unknown field during decoding and 
re-encoding. This feature is removed and unknown field are dropped during decoding. 

`ocaml-protoc` already had this behavior. 


#### Work Items

Here are in chronological order the various work items to implement:

1. Bug fix for proto2 - **done** 

As mentioned previously `ocaml-protoc` does not throw an exception if 
a required field is not send on the wire. 

2. Introduce a new field label `Proto3` 

In the protobuf AST ([Pbpt module](src/compilerlib/pbpt.ml)) we should introduce 
a new field label `Proto3`. This would allow the compiler to work for both 
proto2 and proto3 seamlessly. Some of the proto3 invariants (such as defaults not being
allowed) should be enforced at runtime.

The parser ([Pbparser module](src/compilerlib/pbparser.mly)) should also be updated to 
allow the proto3 fields. 

The compiler should generate all warnings where the new field label should be supported in 
pattern matching. 

3. Introduce proto3 check

The following method should make all the runtime checks about the constrains that 
proto3 imposes

```OCaml
val is_proto3_valid : Pbpt.proto -> bool 
```

For instance: 
* no optional or required field 
* no default values 

This function can then be called in the main function of the 
compiler (src/ocaml-protoc/ocaml_protoc.ml).

4. Add support for `reserved` syntax 

Looks like compilier `.proto` files are now using this field. 
a) Accept the syntax but do not perform any checks
b) Add checks
