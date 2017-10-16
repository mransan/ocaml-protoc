#include <test26.pb.h>

#include <test_util.h>

#include <iostream>

Test create_test() {
    Test test;
    test.mutable_double_value()->set_value(1.23);
    test.mutable_float_value()->set_value(1.23);
    test.mutable_int32_value()->set_value(-123);
    test.mutable_uint32_value()->set_value(123);
    test.mutable_int64_value()->set_value(-123);
    test.mutable_uint64_value()->set_value(123);
    test.mutable_bool_value()->set_value(true);
    test.mutable_string_value()->set_value("abc");
    test.mutable_bytes_value()->set_value("abc");
    return test;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test(), "test26.c2ml.data");
    }
    else if(mode == "decode") {
        Test test; 
        validate_decode(test, "test26.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

