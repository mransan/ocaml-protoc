#
# Pure OCaml, no packages, no _tags, code in serveral directories
#

# bin-annot is required for Merlin and other IDE-like tools
# The -I flag introduces sub-directories to search for code

OCB_FLAGS = -use-ocamlfind -I src/lib -I src/ocaml-protoc
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

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

unit-tests: 		
			$(OCB) ./src/unit-tests/unit-tests.native 
			./unit-tests.native
			
.PHONY: 	all clean byte native profile debug test

# Integration testing 
#


# Location of Google protobuffer installation. 
# -- To modify -- 
#
PB_HINC =../install.git/build/include/
PB_LINC =../install.git/build/lib/
PROTOC  =../install.git/build/bin/protoc 
export LD_LIBRARY_PATH=../install.git/build/lib

ML_PROTOC=./ocaml-protoc.native


%_cpp.tsk: %_cpp.cpp %.pb.cc 
	g++ -I ./ -I ./src/integration-tests/  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

.SECONDARY: 

%.pb.cc: %.proto
	$(PROTOC) --cpp_out ./ $<

%_pb.ml %_pb.mli : %.proto native 
	$(ML_PROTOC) -debug $<

 
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

integration: test01 test02 testCompat test05 test06 test07 test08 
