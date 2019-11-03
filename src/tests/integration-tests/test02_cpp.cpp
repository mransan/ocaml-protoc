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
    abt.set_o09(0xFFFFFFFFl); 
    abt.set_o10(0xFFFFFFFFFFFFFFFFL); 
    abt.set_o11(0xFFFFFFFFl);
    abt.set_o12(0xFFFFFFFFFFFFFFFFL);
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
        AllBasicsTypes expected = create_test_all_basic_types();
        assert(abt.o01() == expected.o01());  
        assert(abt.o02() == expected.o02());
        assert(abt.o03() == expected.o03());
        assert(abt.o04() == expected.o04());
        assert(abt.o05() == expected.o05());
        assert(abt.o06() == expected.o06());
        assert(abt.o07() == expected.o07());
        assert(abt.o08() == expected.o08());
        assert(abt.o09() == expected.o09());
        assert(abt.o10() == expected.o10());
        assert(abt.o11() == expected.o11());
        assert(abt.o12() == expected.o12());
        assert(abt.o13() == expected.o13());
        assert(abt.o14() == expected.o14());
        assert(abt.o15() == expected.o15());
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

