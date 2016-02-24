#include <test12.pb.h>

#include <test_util.h>

#include <iostream>

M create_test_m10() {
    M m; 
    m.set_to(1); 
    m.set_type(1);
    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m10(), "test12.c2ml.data");
    }
    else if(mode == "decode") {
        M m; 
        validate_decode(m, "test12.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
