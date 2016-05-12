SRC_COMPILERLIB_FILES       +=$(wildcard src/compilerlib/*.ml)
SRC_COMPILERLIB_FILES       +=$(wildcard src/compilerlib/*.mli)

SRC_COMPILERLIB_OCAML_FILES +=$(wildcard src/compilerlib/ocaml/*.mli)
SRC_COMPILERLIB_OCAML_FILES +=$(wildcard src/compilerlib/ocaml/*.ml)

SRC_JSMAIN_FILES            +=src/js-main/ocaml_protoc_compiler.ml
SRC_JSMAIN_FILES            +=src/js-main/ocaml_protoc_compiler.mli

SRC_FILES+=$(SRC_COMPILERLIB_FILES)
SRC_FILES+=$(SRC_COMPILERLIB_OCAML_FILES)
SRC_FILES+=$(SRC_JSMAIN_FILES)

PHONY: init 
init:
	mkdir -p js 
	cp $(SRC_FILES) js

gen: init
	ocamllex -o js/pblexer.ml src/compilerlib/pblexer.mll
	ocamlyacc src/compilerlib/pbparser.mly
	mv src/compilerlib/pbparser.ml js/
	mv src/compilerlib/pbparser.mli js/

all: init gen
	`npm bin`/bsc -I js/ `ocamldep -sort \`ls js/*.ml*\``
