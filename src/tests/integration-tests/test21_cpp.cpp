#include <test21.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>

M create_m() {
    M m;
    m.set_f1(1); 
    m.mutable_f2()->set_sub_f1(2);
    m.set_f3(EONE);
    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv); 

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_m(), "test21.c2ml.data");
    }
    else if(mode == "decode") {
        M m; 
        validate_decode(m, "test21.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

