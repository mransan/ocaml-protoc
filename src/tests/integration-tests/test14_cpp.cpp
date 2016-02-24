#include <test14.pb.h>

#include <test_util.h>

#include <iostream>

void add_4(a* a) {
    a->add_aa(1);
    a->add_aa(2);
    a->add_aa(3);
    a->add_aa(4);
}

d create_test_m() {
    d d;
    {
        add_4(d.mutable_da());
    }
    {
        b* b = d.mutable_db();
        add_4(b->mutable_ba());
    }
    {
        c* c = d.mutable_dc();
        add_4(c->mutable_ca());
        add_4(c->mutable_cc());
    }
    return d;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test14.c2ml.data");
    }
    else if(mode == "decode") {
        d m; 
        validate_decode(m, "test14.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
