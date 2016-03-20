#include <test16.pb.h>

#include <test_util.h>

#include <iostream>

m create_test_m() {
    m m;
    {
        m.set_f1(1); 
        m.set_f2(2); 
        m.set_f3(4); 
        m.add_f4(3);
        m.add_f4(4);
    }
    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test16.c2ml.data");
    }
    else if(mode == "decode") {
        m m; 
        validate_decode(m, "test16.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
