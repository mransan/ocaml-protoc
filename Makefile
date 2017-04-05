first : yojson
# 	./ocaml-protoc -yojson -binary -pp -ml_out src/examples/ src/examples/example01.proto
# 	$(OCB) -pkgs yojson -I src/examples example01.byte

# OCamlbuild configuration
#
OCB_INC   = -X lib -X node_modules 
OCB_INC  += -I src/runtime -I src/runtime-yojson/src/
OCB_INC  += -I src/compilerlib -I src/ocaml-protoc
OCB_FLAGS = -use-ocamlfind -pkgs ppx_deriving_protobuf.runtime 
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
	@EXE=.exe
else
	@EXE=
endif

# Common Directories
#
UNIT_TESTS_DIR        = src/tests/unit-tests
INTEGRATION_TESTS_DIR = src/tests/integration-tests
BENCHMARK_DIR         = src/tests/benchmark
GOOGLE_UNITTEST_DIR   = src/tests/google_unittest
OCAMLOPTIONS_HINC     = src/include/ocaml-protoc

.PHONY: doc ocaml_protoc.native bin.native bin.byte clean clean.gen default

default:
	$(info use `make [clean|lib.native|lib.byte|bin.native|bin.byte|install|uninstall]`)
	
clean.gen:
	rm -f $(INTEGRATION_TESTS_DIR)/*.pb.*
	rm -f $(INTEGRATION_TESTS_DIR)/*_types.ml*
	rm -f $(INTEGRATION_TESTS_DIR)/*_pb.ml*
	rm -f $(INTEGRATION_TESTS_DIR)/*_pp.ml*
	rm -f $(YOJSON_DIR)/*_types.ml*
	rm -f $(YOJSON_DIR)/*_yojson.ml*
	rm -f $(YOJSON_DIR)/*.pb.*
	rm -f $(GOOGLE_UNITTEST_DIR)/*_pb.ml*
	rm -f $(GOOGLE_UNITTEST_DIR)/*_pp.ml*
	rm -f $(GOOGLE_UNITTEST_DIR)/*_types.ml*
	rm -f $(OCAMLOPTIONS_HINC)/*.pb.*
	rm -f src/examples/*_pb.ml*

clean: clean.gen
	$(OCB) -clean
	rm -f *.data
	rm -f $(INTEGRATION_TESTS_DIR)/*.tsk  

###################
# ---- BUILD ---- # 
###################

# ocaml-protoc runtime library (native build)
lib.native:
	$(OCB) pbrt.cmxa
	$(OCB) pbrt.cmxs

# ocaml-protoc runtime library (byte code build)
lib.byte:
	$(OCB) pbrt.cma

ocaml_protoc.native:
	$(OCB) ocaml_protoc.native

# ocaml-protoc native executable 
bin.native: ocaml_protoc.native 
	cp ocaml_protoc.native ocaml-protoc$(EXE)

# ocaml-protoc byte executable
bin.byte:
	$(OCB) ocaml_protoc.byte
	cp ocaml_protoc.byte ocaml-protoc$(EXE)

####################
# ---- INSTALL---- # 
####################

.PHONY : check_prefix check_install uninstall install lib-install bin-install lib-uninstall bin-uninstall

check_prefix:
ifndef PREFIX
	$(error PREFIX variable undefined... required for install)
endif
ifndef BINDIR
    BINDIR = $(PREFIX)/bin
endif
export BINDIR

check_install: check_prefix 
	@if [ ! -d $(BINDIR) ]; then \
        echo "$(BINDIR) directory does not exist... create it first"; exit 1; \
    fi;

LIB_FILES+=pbrt
LIB_FILES+=pbrt_json

LIB_BUILD     =_build/src/runtime
LIB_INSTALL   = META 
LIB_INSTALL  +=$(OCAMLOPTIONS_HINC)/ocamloptions.proto 

# modules 
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.mli,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmi,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.annot,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmo,$(LIB_FILES))

# byte code library
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cma 

# native library and modules 
LIB_INSTALL  +=-optional  
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmx,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmt,$(LIB_FILES))
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmxa 
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmxs
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.a

# we do not specify the dependency on target lib.byte/lib.native
# since we assume the caller (ie opam) will do it. (See opam file). 
lib.install:  
	ocamlfind install ocaml-protoc $(LIB_INSTALL)

lib.uninstall:
	ocamlfind remove ocaml-protoc


bin.install: check_install
	install -m 0755 ocaml-protoc$(EXE) $(BINDIR)

bin.uninstall: check_install
	rm -f $(BINDIR)/ocaml-protoc$(EXE)

all.install.build: lib.byte lib.native bin.native 

install: all.install.build check_install lib.install.byte lib.install.native bin.install

uninstall: lib.uninstall bin.uninstall

doc:
	$(OCB) src/compilerlib/compilerlib.docdir/index.html
	
include Makefile.test
