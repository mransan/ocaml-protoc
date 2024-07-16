
## 3.1.1

- fix error in codegen for nested `oneof` in yojson

## 3.1

- Expose one-of per-constructor options
- Support options for enum values
- expose server stubs for advanced users

- fix: only generate services from the current file
- silence warning 44 in generated code
- Use generated encoders/decoders for empty messages in rpc

breaking:

- remove bs runtime, we don't support it going forward

## 3.0.2

- fix segfault in pbrt in bytecode

## 3.0.1

- fix bug where code for services would be generated even if `--services`
    is not passed on the CLI

## 3.0

Major version that breaks code for every user. Sorry!

### Breaking

- Generated code now always fits into 2 files: code
    for `foo.proto` is produced in `foo.ml` and `foo.mli`.
    This follows a fairly large internal refactor that enables
    a plugin architecture internally (which facilitated codegen for
    services).
    All encoder/decoder functions also have a new name now,
    so that they don't collide inside the same file.
- CLI flags now use `--this-style` rather than `-this-style`
- Bump minimal OCaml version to 4.08

### Performance

- encode probotufs backward, which means we can write directly
    into a single buffer (which is not possible in a forward mode
    because sub-messages require an unknown amount of prefix space
    to write their size as a varint)
- add C stubs for pbrt 
- use Bytes functions to read/write fixed size integers
- inlining annotations
- more benchmarks, helping optimization overall
- reduce allocations drastically, by having the generated code
    create fewer closures


### Services

- add `pbrt_services` runtime library
- generate code for `service` statements. This is a big feature
    for users who want to implement RPC systems using protobuf.
    The generated code is agnostic to whatever RPC implementation
    will use it, it only packs together RPC method names and path
    with the relevant encoders/decoders. Services require
    both JSON and binary encoders/decoders to be present.

### JSON

- migrate `pbrt_yojson` into the main ocaml-protoc repo
- JSON runtime support for empty messages
- Add support for bytes in JSON encoding
- Add support for encoding and decoding maps in JSON

### Other features and fixes

- add `--make` flag to generate `make` functions that take fewer
    optional arguments. This helps preventing the user from
    forgetting important arguments when they're encoding to protobuf.
    Arguments actually marked as `optional` in the .proto file
    are still optional.
- Support maps/lists in options
- add Pbrt.Decoder.of_substring
- Allow options to be named like `(validate.rules).message.required`
- support code generation for empty messages
- Empty proto file is a valid proto as well

### Testing

- expect-style tests for parser
- Tests for option parsing
- Test demonstrating parse error printing

## 2.4

- expose compiler library as `ocaml-protoc.compiler-lib`
- option to differentiate unsigned arg from signed arguments,
  taggings uint32/64 with `` `unsigned``

internal changes:

- fix(test): change integration test, protoc produces broken C++ code
- automatic generation of gh-pages
- add basic CI
- use ocamlformat

## 2.3

- allow `optional` in proto3 files
- improve compatibility with OCaml 5

## 2.2

- handle `stream` as a name (#179)
- support hexadecimal notation for integers
- improve generated pretty printers
- move to dune 2.0
- rename runtime library to `pbrt`, with a separate opam package
- print bytes opaquely

## 2.1

- perf: nested buffer to reduce allocations for nested messages
- perf: improvements in runtime library, varint encoding/decoding, add benchmarks
- Update docs/tests to reflect that sfixed is supported
- add clear/reset to the encoder
- Add ability to parse stream modifier

## 2.0.3

- parsing of services




