#include <test24.pb.h>

#include <test_util.h>

#include <iostream>

A create_test_a() {
    A a; 
    a.set_value("value");
    return a;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_a(), "test24.c2ml.data");
    }
    else if(mode == "decode") {
        A a; 
        validate_decode(a, "test24.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
