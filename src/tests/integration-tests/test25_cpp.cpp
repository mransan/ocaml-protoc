#include <test25.pb.h>

#include <test_util.h>

#include <iostream>

m create_test_m() {
    m m;
    m.add_f1(1l);
    m.add_f1(2l);
    m.add_f1(3l);
    m.add_f1(4l);

    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test25.c2ml.data");
    }
    else if(mode == "decode") {
        m m; 
        validate_decode(m, "test25.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
