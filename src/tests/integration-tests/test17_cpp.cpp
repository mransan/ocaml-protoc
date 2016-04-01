#include <test17.pb.h>

#include <test_util.h>

#include <iostream>

m create_test_m() {
    m m;
    {
        m_i& m_i = *m.mutable_m1(); 
        m_i.set_i1(1);
        m_i.set_i2(2);
    }

    m.set_m2(1);
    m.set_m3(1);
    m.set_m4(4);
    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test17.c2ml.data");
    }
    else if(mode == "decode") {
        m m; 
        validate_decode(m, "test17.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
