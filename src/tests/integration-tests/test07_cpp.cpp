#include <test07.pb.h>

#include <test_util.h>

#include <iostream>


Node create_test_node() {
    Node n; 
    {
        Tree& l  = *n.mutable_left();
        Node& nl = *l.mutable_node(); 
        nl.set_value(2); 
        nl.mutable_left()->set_empty(0);
        nl.mutable_right()->set_empty(0);
    }
    {
        Tree& l  = *n.mutable_right();
        Node& nr = *l.mutable_node(); 
        nr.set_value(3); 
        nr.mutable_left()->set_empty(0);
        nr.mutable_right()->set_empty(0);
    }
    n.set_value(1); 
    return n;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_node(), "test07.c2ml.data");
    }
    else if(mode == "decode") {
        Node n; 
        validate_decode(n, "test07.ml2c.data");
        assert(n.DebugString() == create_test_node().DebugString()); 
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

