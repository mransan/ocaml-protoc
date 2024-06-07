FROM ocaml/opam:debian-ocaml-4.14 AS base

WORKDIR /build/
RUN sudo chown -R opam: /build/
RUN sudo apt install protobuf-compiler libprotobuf-dev -y
COPY --chown=opam ./src/ ./src/
COPY --chown=opam pbrt.opam pbrt_services.opam pbrt_yojson.opam ocaml-protoc.opam \
  Makefile Makefile.test dune-project dune .ocamlformat \
  .ocamlformat-ignore ./

RUN opam pin -n .
RUN opam depext -yt ocaml-protoc
RUN opam install -t . --deps-only

FROM base AS run-the-damn-build
RUN opam exec -- dune build @install
RUN opam exec -- dune runtest

# integration tests
RUN opam install ppx_deriving -y
RUN opam exec -- make clean build integration PB_LINC=/usr/lib/x86_64-linux-gnu/ PB_HINC=/usr/include/x86_64-linux-gnu/

