#include <test02.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>


AllBasicsTypes create_test_all_basic_types() {
    AllBasicsTypes abt;
    abt.set_o01(1.0); 
    abt.set_o02(2.0); 
    abt.set_o03(-123); 
    abt.set_o04(456); 
    abt.set_o05(123); 
    abt.set_o06(456); 
    abt.set_o07(-123); 
    abt.set_o08(-456); 
    abt.set_o09(0xEFFFFFFFl); 
    abt.set_o10(0xEBABABABABABABABL); 
    abt.set_o11(0xEFFFFFFFl);
    abt.set_o12(0xEBABABABABABABABL);
    abt.set_o13(true); 
    abt.set_o14("Iam a test string"); 
    abt.set_o15("Iam a test byte"); 

    return abt;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv); 

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_all_basic_types(), "test02.c2ml.data");
    }
    else if(mode == "decode") {
        AllBasicsTypes abt; 
        validate_decode(abt, "test02.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

