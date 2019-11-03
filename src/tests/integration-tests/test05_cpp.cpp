#include <google/protobuf/util/message_differencer.h>
#include <test05.pb.h>

#include <test_util.h>

#include <iostream>
#include <climits>
#include <fstream>
    
namespace { 
int max_int = 107374182;
int min_int = -107374182;
int inc     = 250;
}

IntList create_test() {
    IntList t;

    for(int i = min_int ; i<max_int - 2*inc ; i+= inc) {
        t.add_l(i);
    } 

    return t;
}

int main(int argc, char const* const argv[]) {
    check_argv(argc, argv);
    std::string mode(argv[1]);

    std::cout << "inc : " << inc << std::endl; 
    std::cout << "min_int : " << min_int << ", max_int : " << max_int << std::endl; 

    if(mode == "encode") {
        return encode_to_file(create_test(), "test05.c2ml.data");
    }
    else if(mode == "decode") {
        IntList t; 
        validate_decode(t, "test05.ml2c.data", false);
        IntList expected = create_test();
        assert(t.l().size() == expected.l().size());
        for(std::size_t i=0; i < t.l().size(); ++i) {
          assert(t.l()[i] == expected.l()[i]);
        }
        assert(google::protobuf::util::MessageDifferencer::ApproximatelyEquals(
              t, expected));
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

