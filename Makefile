OCB_FLAGS = -I src/lib -I src/pbrt -I src/ocaml-protoc
OCB = 		ocamlbuild $(OCB_FLAGS)

.PHONY: clean default

default:
	$(info use `make [clean|lib.native|lib.byte|bin.native|bin.byte|install|install.byte]`)
	
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

PREFIX_BIN=$(PREFIX)/bin
PREFIX_LIB=$(PREFIX)/lib

# -- Install -- 

.PHONY : check_prefix uninstall install install.byte install.native 

check_prefix:
ifndef PREFIX
	$(error PREFIX variable undefined... required for install)
endif
	mkdir -p $(PREFIX_BIN)
	mkdir -p $(PREFIX_LIB)

install.byte: check_prefix lib.byte bin.byte 
	cp ./ocaml-protoc.byte $(PREFIX_BIN)
	cp _build/src/pbrt/pbrt.cma  $(PREFIX_LIB)  
	cp _build/src/pbrt/pbrt.cmi  $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.cmt  $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.cmti $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.mli  $(PREFIX_LIB) 
	ln -s $(PREFIX_BIN)/ocaml-protoc.byte $(PREFIX_BIN)/ocaml-protoc

install.native: check_prefix lib.byte lib.native bin.native
	cp ./ocaml-protoc.native $(PREFIX_BIN)
	cp _build/src/pbrt/pbrt.cma  $(PREFIX_LIB)  
	cp _build/src/pbrt/pbrt.a $(PREFIX_LIB)  
	cp _build/src/pbrt/pbrt.cmxa $(PREFIX_LIB)  
	cp _build/src/pbrt/pbrt.cmxs $(PREFIX_LIB)  
	cp _build/src/pbrt/pbrt.cmi  $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.cmt  $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.cmti $(PREFIX_LIB)
	cp _build/src/pbrt/pbrt.mli  $(PREFIX_LIB) 
	ln -s $(PREFIX_BIN)/ocaml-protoc.native $(PREFIX_BIN)/ocaml-protoc

install: install.native
	
uninstall:
	rm -f $(PREFIX_BIN)/ocaml-protoc
	rm -f $(PREFIX_BIN)/ocaml-protoc.byte
	rm -f $(PREFIX_BIN)/ocaml-protoc.native
	rm -f $(PREFIX_LIB)/pbrt.*

# ---- TESTS ---- # 

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
			
# integration tests with Google protoc (C++ target) to ensure that 
# the generated OCaml code can encode/decode message compatible with Google 
# implementation

# location of where the Google protoc compiler is installed  
PB_BUILD = ../../install/build
PB_HINC = $(PB_BUILD)/include/
PB_LINC = $(PB_BUILD)/lib/
PROTOC  = $(PB_BUILD)/bin/protoc 

export LD_LIBRARY_PATH=../../install/build/lib

ML_PROTOC=./ocaml-protoc.byte

%_cpp.tsk: %_cpp.cpp %.pb.cc 
	g++ -I ./ -I ./src/integration-tests/  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

src/integration-tests/test10_cpp.tsk: \
	src/integration-tests/test10_cpp.cpp \
	src/integration-tests/test10.pb.cc \
	src/integration-tests/test09.pb.cc 
	g++ -I ./ -I ./src/integration-tests/  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@ 

.SECONDARY: 

%.pb.cc: %.proto
	$(PROTOC) --cpp_out src/integration-tests/ -I src/integration-tests/ $<

%_pb.ml %_pb.mli : %.proto bin.byte
	$(ML_PROTOC) -I src/integration-tests/ -ml_out ./src/integration-tests/ $<
 
%_ml.native: %_pb.mli %_pb.ml %_ml.ml 
	$(OCB) -I src/integration-tests -pkg unix $@ 

test%: src/integration-tests/test%_ml.native ./src/integration-tests/test%_cpp.tsk 
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

integration: test01 test02 testCompat test05 test06 test07 test08 test09 test10 test11 test12 test13 

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

example%.native: bin.byte  
	$(ML_PROTOC) -ml_out src/examples/ ./src/examples/example$*.proto 
	$(OCB) -I src/examples src/examples/example$*.native

all-examples: example01.native

it: bin.native
			$(OCB) ./src/unit-tests/format_play_ground.native
			./format_play_ground.native
