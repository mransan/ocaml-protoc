OCB_FLAGS = -pkg compiler-libs.common -I src/compilerlib -I src/compilerlib/ocaml -I src/runtime -I src/ocaml-protoc
OCB = 		ocamlbuild $(OCB_FLAGS)

UNIT_TESTS_DIR        = src/tests/unit-tests
INTEGRATION_TESTS_DIR = src/tests/integration-tests
BENCHMARK_DIR         = src/tests/benchmark
GOOGLE_UNITTEST_DIR   = src/tests/google_unittest

OCAMLOPTIONS_HINC     = src/include/ocaml-protoc

.PHONY: clean default

default:
	$(info use `make [clean|lib.native|lib.byte|bin.native|bin.byte|install|uninstall]`)
	
clean:
	$(OCB) -clean
	rm -f *.data
	rm -f $(INTEGRATION_TESTS_DIR)/*.tsk  
	rm -f $(INTEGRATION_TESTS_DIR)/*.pb.cc
	rm -f $(INTEGRATION_TESTS_DIR)/*.pb.h
	rm -f $(INTEGRATION_TESTS_DIR)/*_pb.ml
	rm -f $(INTEGRATION_TESTS_DIR)/*_pb.mli
	rm -f $(GOOGLE_UNITTEST_DIR)/*_pb.ml
	rm -f $(GOOGLE_UNITTEST_DIR)/*_pb.mli
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
	$(OCB) ocaml_protoc.native
	mv ocaml_protoc.native ocaml-protoc.native

# ocaml-protoc byte executable
bin.byte:
	$(OCB) ocaml_protoc.byte
	mv ocaml_protoc.byte ocaml-protoc.byte

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

ifndef CXX
    CXX = g++
endif
export CXX

ifndef CPPFLAGS
    CPPFLAGS = 
endif
export CPPFLAGS

check_install: check_prefix 
	@if [ ! -d $(BINDIR) ]; then \
        echo "$(BINDIR) directory does not exist... create it first"; exit 1; \
    fi;

LIB_BUILD     =_build/src/runtime
LIB_INSTALL   = META 
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmi 
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmo
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmx
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmt
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.annot
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cma 
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.cmxa 
LIB_INSTALL  +=$(LIB_BUILD)/pbrt.a
LIB_INSTALL  +=$(OCAMLOPTIONS_HINC)/ocamloptions.proto 

lib.install: lib.byte lib.native
	ocamlfind install ocaml-protoc $(LIB_INSTALL)

lib.uninstall:
	ocamlfind remove ocaml-protoc

ifeq "$(shell ocamlc -config | grep os_type)" "os_type: Win32"
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
	$(OCB) $(UNIT_TESTS_DIR)/parse_field_options.native 
	$(OCB) $(UNIT_TESTS_DIR)/parse_file_options.native 
	$(OCB) $(UNIT_TESTS_DIR)/parse_fields.native 
	$(OCB) $(UNIT_TESTS_DIR)/parse_enum.native
	$(OCB) $(UNIT_TESTS_DIR)/parse_message.native 
	$(OCB) $(UNIT_TESTS_DIR)/parse_import.native 
	$(OCB) $(UNIT_TESTS_DIR)/pbtt_compile_p1.native 
	$(OCB) $(UNIT_TESTS_DIR)/pbtt_compile_p2.native 
	$(OCB) $(UNIT_TESTS_DIR)/backend_ocaml_test.native
	$(OCB) $(UNIT_TESTS_DIR)/ocaml_codegen_test.native
	$(OCB) $(UNIT_TESTS_DIR)/graph_test.native
	$(OCB) $(UNIT_TESTS_DIR)/pbrt_array.native
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
PB_INSTALL = ../../install/build
PB_HINC    = $(PB_INSTALL)/include/
PB_LINC    = $(PB_INSTALL)/lib/
PROTOC     = $(PB_INSTALL)/bin/protoc 

export LD_LIBRARY_PATH=$(PB_LINC)

ML_PROTOC=./ocaml-protoc.byte -I $(OCAMLOPTIONS_HINC) -I $(PB_HINC)

%_cpp.tsk: %_cpp.cpp %.pb.cc $(OCAMLOPTIONS_HINC)/ocamloptions.pb.cc
	$(CXX) $(CPPFLAGS) -I ./ -I $(INTEGRATION_TESTS_DIR) -I $(OCAMLOPTIONS_HINC) -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

$(INTEGRATION_TESTS_DIR)/test10_cpp.tsk: \
	$(INTEGRATION_TESTS_DIR)/test10_cpp.cpp \
	$(INTEGRATION_TESTS_DIR)/test10.pb.cc \
	$(INTEGRATION_TESTS_DIR)/test09.pb.cc 
	$(CXX) $(CPPFLAGS) -I ./ -I $(INTEGRATION_TESTS_DIR)  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@ 

.SECONDARY: 

%.pb.cc: %.proto
	$(PROTOC) --cpp_out $(INTEGRATION_TESTS_DIR) -I $(PB_HINC) -I $(OCAMLOPTIONS_HINC) -I $(INTEGRATION_TESTS_DIR) $<

%_pb.ml %_pb.mli : %.proto bin.byte bin.native
	$(ML_PROTOC) -I $(INTEGRATION_TESTS_DIR) -ml_out $(INTEGRATION_TESTS_DIR) $<
 
%_ml.native: %_pb.mli %_pb.ml %_ml.ml 
	$(OCB) -tag debug -I $(INTEGRATION_TESTS_DIR) -pkg unix $@ 

test%: bin.native bin.byte \
	   $(INTEGRATION_TESTS_DIR)/test%_ml.native \
	   $(INTEGRATION_TESTS_DIR)/test%_cpp.tsk 
	$(INTEGRATION_TESTS_DIR)/test$*_cpp.tsk encode
	time ./_build/$(INTEGRATION_TESTS_DIR)/test$*_ml.native decode
	./_build/$(INTEGRATION_TESTS_DIR)/test$*_ml.native encode
	time $(INTEGRATION_TESTS_DIR)/test$*_cpp.tsk decode

.PHONY: testCompat 

testCompat: $(INTEGRATION_TESTS_DIR)/test03_cpp.tsk $(INTEGRATION_TESTS_DIR)/test04_ml.native 
	$(INTEGRATION_TESTS_DIR)/test03_cpp.tsk encode
	./_build/$(INTEGRATION_TESTS_DIR)/test04_ml.native decode
	./_build/$(INTEGRATION_TESTS_DIR)/test04_ml.native encode
	$(INTEGRATION_TESTS_DIR)/test03_cpp.tsk decode

.PHONY: integration

integration: test01 test02 test05 test06 test07 test08 test09 test10 \
	         test11 test12 test13 test14 test15 testCompat 

.PHONY: google_unittest

google_unittest: bin.byte
	$(ML_PROTOC) -I $(GOOGLE_UNITTEST_DIR) -ml_out $(GOOGLE_UNITTEST_DIR) $(GOOGLE_UNITTEST_DIR)/unittest_import.proto 
	$(ML_PROTOC) -I $(GOOGLE_UNITTEST_DIR) -ml_out $(GOOGLE_UNITTEST_DIR) $(GOOGLE_UNITTEST_DIR)/unittest.proto 
	$(OCB) -I $(GOOGLE_UNITTEST_DIR) google_unittest.native
	./google_unittest.native

.PHONY: all-tests

all-tests: unit-tests integration google_unittest testCompat 

# -- Examples -- 

.PHONY: all-examples

example%.native: src/examples/example%.ml src/examples/example%.proto bin.byte bin.native 
	$(ML_PROTOC) -ml_out src/examples/ ./src/examples/example$*.proto 
	$(OCB) -I src/examples src/examples/example$*.native

all-examples: example01.native example02.native

it: bin.native
	$(OCB) ./src/unit-tests/pbrt_array.native
	time ./pbrt_array.native

.PHONY: benchmark_single_ml.native

benchmark_single_ml.native: bin.byte
	$(ML_PROTOC) -I $(BENCHMARK_DIR) -ml_out $(BENCHMARK_DIR) $(BENCHMARK_DIR)/benchmark.proto
	$(OCB) -use-ocamlfind -pkg unix -I src/tests/benchmark $@ 
