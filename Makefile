OCB_FLAGS = -I src/compilerlib -I src/runtime -I src/ocaml-protoc
OCB = 		ocamlbuild $(OCB_FLAGS)

.PHONY: clean default

default:
	$(info use `make [clean|lib.native|lib.byte|bin.native|bin.byte|install|uninstall]`)
	
clean:
	$(OCB) -clean
	rm -f *.data
	rm -f src/integration-tests/*.tsk  
	rm -f src/integration-tests/*.pb.cc
	rm -f src/integration-tests/*.pb.h
	rm -f src/integration-tests/*_pb.ml
	rm -f src/integration-tests/*_pb.mli
	rm -f src/google_unittest/*_pb.ml
	rm -f src/google_unittest/*_pb.mli
	rm -f src/examples/*_pb.ml
	rm -f src/examples/*_pb.mli

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

# ocaml-protoc native executable 
bin.native: 
	$(OCB) ocaml-protoc.native

# ocaml-protoc byte executable
bin.byte:
	$(OCB) ocaml-protoc.byte

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

LIB_BUILD       =_build/src/runtime
LIB_INSTALL     = META $(LIB_BUILD)/pbrt.cmi $(LIB_BUILD)/pbrt.cma src/include/ocaml-protoc/ocamloptions.proto 
LIB_INSTALL_OPT = $(LIB_BUILD)/pbrt.cmxa $(LIB_BUILD)/pbrt.a

lib.install: lib.byte lib.native
	ocamlfind install ocaml-protoc $(LIB_INSTALL) $(LIB_INSTALL_OPT)

lib.uninstall:
	ocamlfind remove ocaml-protoc

ifeq "$(shell ocamlc -config |grep os_type)" "os_type: Win32"
	@EXE=.exe
else
	@EXE=
endif

bin.install: check_install bin.native
	@mv ocaml-protoc.native ocaml-protoc$(EXE) 
	install -m 0755 ocaml-protoc$(EXE) $(BINDIR)

bin.uninstall: check_install
	rm -f $(BINDIR)/ocaml-protoc$(EXE)

install: check_install lib.install bin.install

uninstall: lib.uninstall bin.uninstall
	
###################
# ---- TESTS ---- # 
###################

.PHONY: unit-tests

# unit test of the ocaml-protoc internals  
unit-tests: 		
	$(OCB) ./src/unit-tests/parse_field_options.native 
	$(OCB) ./src/unit-tests/parse_file_options.native 
	$(OCB) ./src/unit-tests/parse_fields.native 
	$(OCB) ./src/unit-tests/parse_enum.native
	$(OCB) ./src/unit-tests/parse_message.native 
	$(OCB) ./src/unit-tests/parse_import.native 
	$(OCB) ./src/unit-tests/pbtt_compile_p1.native 
	$(OCB) ./src/unit-tests/pbtt_compile_p2.native 
	$(OCB) ./src/unit-tests/backend_ocaml_test.native
	$(OCB) ./src/unit-tests/ocaml_codegen_test.native
	$(OCB) ./src/unit-tests/graph_test.native
	$(OCB) ./src/unit-tests/pbrt_array.native
	./parse_field_options.native
	./parse_file_options.native
	./parse_fields.native
	./parse_enum.native
	./parse_message.native
	./parse_import.native
	./pbtt_compile_p1.native
	./pbtt_compile_p2.native
	./backend_ocaml_test.native
	./ocaml_codegen_test.native
	./graph_test.native
	./pbrt_array.native
			
# Integration tests with Google protoc (C++ target) to ensure that 
# the generated OCaml code can encode/decode message compatible with Google 
# implementation

# location of where the Google protoc compiler is installed  
PB_BUILD = ../../install/build
PB_HINC = $(PB_BUILD)/include/
PB_LINC = $(PB_BUILD)/lib/
PROTOC  = $(PB_BUILD)/bin/protoc 

export LD_LIBRARY_PATH=../../install/build/lib

ML_PROTOC=./ocaml-protoc.byte

%_cpp.tsk: %_cpp.cpp %.pb.cc src/include/ocaml-protoc/ocamloptions.pb.cc
	g++ -I ./ -I ./src/integration-tests/  -I src/include/ocaml-protoc -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

src/integration-tests/test10_cpp.tsk: \
	src/integration-tests/test10_cpp.cpp \
	src/integration-tests/test10.pb.cc \
	src/integration-tests/test09.pb.cc 
	g++ -I ./ -I ./src/integration-tests/  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@ 

.SECONDARY: 

%.pb.cc: %.proto
	$(PROTOC) --cpp_out src/integration-tests/ -I $(PB_HINC) -I src/include/ocaml-protoc/ -I src/integration-tests/ $<

%_pb.ml %_pb.mli : %.proto bin.byte bin.native
	$(ML_PROTOC) -I $(PB_HINC) -I src/include/ocaml-protoc/ -I src/integration-tests/ -ml_out ./src/integration-tests/ $<
 
%_ml.native: %_pb.mli %_pb.ml %_ml.ml 
	$(OCB) -tag debug -I src/integration-tests -pkg unix $@ 

test%: bin.native bin.byte src/integration-tests/test%_ml.native ./src/integration-tests/test%_cpp.tsk 
	./src/integration-tests/test$*_cpp.tsk encode
	time ./_build/src/integration-tests/test$*_ml.native decode
	./_build/src/integration-tests/test$*_ml.native encode
	time ./src/integration-tests/test$*_cpp.tsk decode

.PHONY: testCompat 

testCompat: ./src/integration-tests/test03_cpp.tsk ./src/integration-tests/test04_ml.native 
	./src/integration-tests/test03_cpp.tsk encode
	./_build/src/integration-tests/test04_ml.native decode
	./_build/src/integration-tests/test04_ml.native encode
	./src/integration-tests/test03_cpp.tsk decode

.PHONY: integration

integration: test01 test02 testCompat test05 test06 test07 test08 test09 test10 test11 test12 test13 test14 

.PHONY: google_unittest

google_unittest: bin.byte
	$(ML_PROTOC) -I ./src/google_unittest/ -ml_out ./src/google_unittest/ src/google_unittest/unittest_import.proto 
	$(ML_PROTOC) -I ./src/google_unittest/ -ml_out ./src/google_unittest/ src/google_unittest/unittest.proto 
	$(OCB) -I src/google_unittest google_unittest.native
	./google_unittest.native

.PHONY: all-tests

all-tests: bin.native bin.byte unit-tests integration google_unittest testCompat 

# -- Examples -- 

.PHONY: all-examples

example%.native: src/examples/example%.ml src/examples/example%.proto bin.byte bin.native 
	$(ML_PROTOC) -I $(PB_HINC) -I ./src/include -ml_out src/examples/ ./src/examples/example$*.proto 
	$(OCB) -I src/examples src/examples/example$*.native

all-examples: example01.native example02.native

it: bin.native
			$(OCB) ./src/unit-tests/pbrt_array.native
			time ./pbrt_array.native
