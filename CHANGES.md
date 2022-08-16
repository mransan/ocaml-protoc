
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




