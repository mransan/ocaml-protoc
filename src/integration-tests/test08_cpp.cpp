#include <test08.pb.h>

#include <test_util.h>

#include <iostream>


Tree create_test_tree() {
    Tree t; 
    {
        Node& n= *t.mutable_node();
        n.set_value(1); 
        {
            Tree& l  = *n.mutable_left(); 
            Node& ln = *l.mutable_node(); 
            ln.set_value(2); 
            ln.mutable_left ()->set_empty(0);
            ln.mutable_right()->set_empty(0);
        }
        {
            Tree& r  = *n.mutable_right(); 
            Node& rn = *r.mutable_node(); 
            rn.set_value(3); 
            rn.mutable_left ()->set_empty(0);
            rn.mutable_right()->set_empty(0);
        }
    }
    return t;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_tree(), "test08.c2ml.data");
    }
    else if(mode == "decode") {
        Tree t; 
        validate_decode(t, "test08.ml2c.data");
        assert(t.DebugString() == create_test_tree().DebugString()); 
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
