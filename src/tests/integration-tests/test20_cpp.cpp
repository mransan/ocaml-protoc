#include <test20.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>

M create_m() {
    M m;
    m.set_f(1); 

    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv); 

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_m(), "test20.c2ml.data");
    }
    else if(mode == "decode") {
        M m; 
        validate_decode(m, "test20.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

