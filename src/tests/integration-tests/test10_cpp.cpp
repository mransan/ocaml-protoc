#include <test10.pb.h>

#include <test_util.h>

#include <iostream>

M10 create_test_m10() {
    M10 m10; 
    m10.mutable_m10_f1()->set_int32_f1(1);
    return m10;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m10(), "test10.c2ml.data");
    }
    else if(mode == "decode") {
        M10 m10; 
        validate_decode(m10, "test10.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

