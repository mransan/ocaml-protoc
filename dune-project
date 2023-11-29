(lang dune 2.0)
(name ocaml-protoc)
(generate_opam_files true)
(version 2.4)

(maintainers "Maxime Ransan <maxime.ransan@gmail.com>" "Simon Cruanes")
(authors "Maxime Ransan <maxime.ransan@gmail.com>" "Simon Cruanes")
(source (github mransan/ocaml-protoc))
(license MIT)

(package
  (name ocaml-protoc)
  (synopsis "Pure OCaml compiler for .proto files")
  (depends
    (odoc :with-doc)
    (pbrt (= :version))
    (pbrt_yojson (and (= :version) :with-test))
    (pbrt_services (and (= :version) :with-test))
    (ocaml (>= 4.08)))
  (tags (protoc protobuf codegen)))

(package
  (name pbrt)
  (synopsis "Runtime library for Protobuf tooling")
  (depends
    stdlib-shims
    (odoc :with-doc)
    (ocaml (>= 4.08)))
  (tags (protobuf encode decode)))

(package
  (name pbrt_yojson)
  (synopsis "Runtime library for ocaml-protoc to support JSON encoding/decoding")
  (depends
    (ocaml (>= 4.08))
    (odoc :with-doc)
    (yojson (>= 1.6))
    base64)
  (tags (protobuf encode decode)))

(package
  (name pbrt_services)
  (synopsis "Runtime library for ocaml-protoc to support RPC services")
  (depends
    (ocaml (>= 4.08))
    (pbrt (= :version))
    (pbrt_yojson (= :version)))
  (tags (protobuf encode decode services rpc)))

