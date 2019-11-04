#include <test27.pb.h>
#include <test_util.h>


Test create_test() {
    Test test;
    test.set_enum1(VALUE_WITH_OPTION);
    test.mutable_unit()->set_unit_field(1l);
    test.mutable_one_of_is_keyword()->set_one_of_is_keyword_field(1l);
    return test;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test(), "test27.c2ml.data");
    }
    else if(mode == "decode") {
        Test test; 
        validate_decode(test, "test27.ml2c.data");
        Test expected = create_test();
        assert(MessageDifferencer::Equals(test, expected));
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

