#include <test23.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>
#include <cassert>


Int64T create_test_all_basic_types() {
    Int64T t;
    t.set_i((3L << 56));
    return t;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv); 

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_all_basic_types(), "test23.c2ml.data");
    }
    else if(mode == "decode") {
        Int64T abt; 
        validate_decode(abt, "test23.ml2c.data");
        assert(abt.i() == create_test_all_basic_types().i());
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

