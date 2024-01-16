(lang dune 2.0)
(name ocaml-protoc)
(generate_opam_files true)
(version 3.0.1)

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
    (base64 (>= 3.0)) )
  (tags (protobuf encode decode)))

(package
  (name pbrt_services)
  (synopsis "Runtime library for ocaml-protoc to support RPC services")
  (depends
    (ocaml (>= 4.08))
    (pbrt (= :version))
    (pbrt_yojson (= :version)))
  (tags (protobuf encode decode services rpc)))

(package
  (name ocaml-protoc-tests)
  (synopsis "Tests for ocaml-protoc and pbrt* packages")
  (depends
    (ocaml (>= 4.08))
    (odoc :with-doc)
    (base
     (and
      (>= v0.16)
      (< v0.17)))
    (base_quickcheck
     (and
      (>= v0.16)
      (< v0.17)))
    (ocaml-protoc (= :version))
    (pbrt (= :version))
    (pbrt_yojson (= :version))
    (pbrt_services (= :version))
    (ppx_compare
     (and
      (>= v0.16)
      (< v0.17)))
    (ppx_expect
     (and
      (>= v0.16)
      (< v0.17)))
    (ppx_sexp_conv
     (and
      (>= v0.16)
      (< v0.17)))
    (ppx_sexp_value
     (and
      (>= v0.16)
      (< v0.17)))
    (yojson (>= 1.6))))

