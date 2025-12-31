#include <test28.pb.h>
#include <test_util.h>


Person create_person() {
    Person p;
    p.set_ofield33(false);
    return p;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_person(), "test28.c2ml.data");
    }
    else if(mode == "decode") {
        Person p;
        validate_decode(p, "test28.ml2c.data");
        Person expected = create_person();
        assert(MessageDifferencer::Equals(p, expected));
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

