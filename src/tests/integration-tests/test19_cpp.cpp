#include <test19.pb.h>

#include <test_util.h>

#include <iostream>

Hash create_test_m() {
    Hash m;
    {
        ::google::protobuf::Map< ::std::string, ::std::string>& map = 
            *m.mutable_t(); 
        map[std::string("one")] = std::string("1");   
        map[std::string("two")] =  std::string("2"); 
        map[std::string("three")] =  std::string("3"); 
    }

    return m;
}

int test_contain_data(Hash const& hash) {
    typedef ::google::protobuf::Map< ::std::string, ::std::string> Map;

    Map::const_iterator i = hash.t().find(std::string("one"));
    if(i == hash.t().end()) {
        return 1;
    }
    i = hash.t().find(std::string("two"));
    if(i == hash.t().end()) {
        return 1;
    }
    i = hash.t().find(std::string("three"));
    if(i == hash.t().end()) {
        return 1;
    }

    return 0;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test19.c2ml.data");
    }
    else if(mode == "decode") {
        Hash m; 
        validate_decode(m, "test19.ml2c.data");
        if(test_contain_data(m)) {
            std::cerr << ">> Invalid data in decoded message" << std::endl;
            return 1;
        }
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
