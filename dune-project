(lang dune 2.0)
(name ocaml-protoc)
(generate_opam_files true)
(version 2.4)

(maintainers "Maxime Ransan <maxime.ransan@gmail.com>")
(authors "Maxime Ransan <maxime.ransan@gmail.com>")
(source (github mransan/ocaml-protoc))
(license MIT)

(package
  (name ocaml-protoc)
  (synopsis "Pure OCaml compiler for .proto files")
  (depends
    (ocaml (>= 4.03)))
  (tags (protoc protobuf codegen)))

(package
  (name pbrt)
  (synopsis "Runtime library for Protobuf tooling")
  (depends
    stdlib-shims
    (ocaml (>= 4.03)))
  (tags (protobuf encode decode)))

(package
  (name pbrt_yojson)
  (synopsis "Runtime library for ocaml-protoc to support JSON encoding/decoding")
  (depends
    (ocaml (>= 4.03))
    (yojson (>= 1.6))
    base64)
  (tags (protobuf encode decode)))

(package
  (name pbrt_services)
  (synopsis "Runtime library for ocaml-protoc to support RPC services")
  (depends
    (ocaml (>= 4.03))
    (pbrt (= :version))
    (pbrt_yojson (= :version)))
  (tags (protobuf encode decode services rpc)))

(package
  (name pbrt_zmq_rpc)
  (synopsis "RPC based on ZMQ for ocaml-protoc")
  (depends
    (ocaml (>= 4.03))
    (pbrt (= :version))
    (pbrt_services (= :version))
    zmq)
  (tags (protobuf zmq rpc)))

