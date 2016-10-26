#include <test13.pb.h>

#include <test_util.h>

#include <iostream>
#include <cassert>

T create_test_m13() {
    T m;
    {
        m.mutable_p2()->mutable_sub_empty();
        m.mutable_p2()->mutable_empty();
        assert(m.mutable_p2()->has_empty());
    }
    {
        EmptyOrIntList* p1 = m.mutable_p1(); 
        p1->add_l()->mutable_empty();
        p1->add_l()->set_int_(1);
        p1->add_l()->mutable_empty();
        p1->add_l()->set_int_(2);
    }
    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m13(), "test13.c2ml.data");
    }
    else if(mode == "decode") {
        ContainsEmpty m; 
        validate_decode(m, "test13.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
