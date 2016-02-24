#include <test06.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>
    
TestM create_test() {
    TestM t;
    t.set_teste_field(TestM_TestE_TestE_Value1);
    return t;
}

int main(int argc, char const* const argv[]) {
    check_argv(argc, argv);
    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test(), "test06.c2ml.data");
    }
    else if(mode == "decode") {
        TestM t; 
        validate_decode(t, "test06.ml2c.data", true);
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

