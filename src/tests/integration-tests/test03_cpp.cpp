#include <test03.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>

Test create_test() {
    Test t;
    t.set_i(123);
    t.set_j(456);
    t.set_k(789);

    return t;
}

int main(int argc, char const* const argv[]) {
    check_argv(argc, argv);
    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test(), "test03.c2ml.data");
    }
    else if(mode == "decode") {
        Test t; 
        validate_decode(t, "test04.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

