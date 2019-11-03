#
# OCamlbuild configuration
#
DUNE=dune

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
BUCKLESCRIPT_TEST_DIR = src/tests/bs
.PHONY: default clean build

default:
	$(info use `make [clean|build]`)
	
clean.gen:
	rm -rf lib
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
	rm -f $(BUCKLESCRIPT_TEST_DIR)/bs_unittest_bs.*
	rm -f $(BUCKLESCRIPT_TEST_DIR)/bs_unittest_types.*
	rm -f ./ocaml-protoc
	rm -f ./yojson

clean: clean.gen
	$(DUNE) clean
	rm -f *.data
	rm -f $(INTEGRATION_TESTS_DIR)/*.tsk  

build:
	$(DUNE) build

include Makefile.test
