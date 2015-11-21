#
# Pure OCaml, no packages, no _tags, code in serveral directories
#

# bin-annot is required for Merlin and other IDE-like tools
# The -I flag introduces sub-directories to search for code

OCB_FLAGS = -use-ocamlfind -I src/lib -I src/ocaml-protoc
OCB = 		ocamlbuild $(OCB_FLAGS)


clean:
			$(OCB) -clean
			rm -f *.data
			rm -f src/integration-tests/*.tsk  
			rm -f src/integration-tests/*.pb.cc
			rm -f src/integration-tests/*.pb.h
			rm -f src/integration-tests/*_pb.ml
			rm -f src/integration-tests/*_pb.mli

native: 
			$(OCB) ocaml-protoc.native

byte:
			$(OCB) ocaml-protoc.byte

profile:
			$(OCB) -tag profile ocaml-protoc.native

debug:
			$(OCB) -tag debug ocaml-protoc.byte

it:
			$(OCB) ./src/unit-tests/backend_ocaml_test.native 
			./backend_ocaml_test.native


unit-tests: 		
			$(OCB) ./src/unit-tests/parse_field_options.native 
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
			./parse_fields.native
			./parse_enum.native
			./parse_message.native
			./parse_import.native
			./pbtt_compile_p1.native
			./pbtt_compile_p2.native
			./backend_ocaml_test.native
			./ocaml_codegen_test.native
			./graph_test.native
			
.PHONY: 	all clean byte native profile debug test

# Integration testing 
#


# Location of Google protobuffer installation. 
# -- To modify -- 
#
PB_HINC =../../install/build/include/
PB_LINC =../../install/build/lib/
PROTOC  =../../install/build/bin/protoc 
export LD_LIBRARY_PATH=../../install/build/lib

ML_PROTOC=./ocaml-protoc.native

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

%_pb.ml %_pb.mli : %.proto native 
	$(ML_PROTOC) -I src/integration-tests/ -ml_out ./src/integration-tests/ $<
 
%_ml.native: %_pb.mli %_pb.ml %_ml.ml 
	$(OCB) -I src/integration-tests -pkg unix $@ 

test%: src/integration-tests/test%_ml.native ./src/integration-tests/test%_cpp.tsk 
	./src/integration-tests/test$*_cpp.tsk encode
	./_build/src/integration-tests/test$*_ml.native decode
	./_build/src/integration-tests/test$*_ml.native encode
	./src/integration-tests/test$*_cpp.tsk decode

testCompat: ./src/integration-tests/test03_cpp.tsk ./src/integration-tests/test04_ml.native 
	./src/integration-tests/test03_cpp.tsk encode
	./_build/src/integration-tests/test04_ml.native decode
	./_build/src/integration-tests/test04_ml.native encode
	./src/integration-tests/test03_cpp.tsk decode

integration: test01 test02 testCompat test05 test06 test07 test08 test09 

all: 		native byte unit-tests integration
