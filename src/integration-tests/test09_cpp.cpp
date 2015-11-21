#include <test09.pb.h>

#include <test_util.h>

#include <iostream>

M09 create_test_m09() {
    M09 m09; 
    m09.set_int32_f1(1);
    return m09;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m09(), "test09.c2ml.data");
    }
    else if(mode == "decode") {
        M09 m09; 
        validate_decode(m09, "test09.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

