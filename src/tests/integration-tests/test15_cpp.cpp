#include <test15.pb.h>

#include <test_util.h>

#include <iostream>

m2 create_test_m2() {
    m2 m2;
    {
        m1& m1 = *m2.add_m1_l();
        {
            m1.set_f(123);
            for(int i=-100; i <= 100; ++i) {
                m1.add_l(i);
            }
        }
    }
    {
        m1& m1 = *m2.add_m1_l();
        {
            m1.set_f(456);
            for(int i=-5; i <= 5; ++i) {
                m1.add_l(i);
            }
        }
    }
    return m2;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m2(), "test15.c2ml.data");
    }
    else if(mode == "decode") {
        m2 m2; 
        validate_decode(m2, "test15.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
