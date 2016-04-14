#include <test18.pb.h>

#include <test_util.h>

#include <iostream>

Maps create_test_m() {
    Maps m;
    {
        ::google::protobuf::Map< ::std::string, ::std::string>& map = 
            *m.mutable_string_to_string(); 
        map[std::string("one")]   = std::string("two");   
        map[std::string("three")] =  std::string("four"); 
    }
    {
        ::google::protobuf::Map< ::std::string, ::google::protobuf::int32>& map = 
            *m.mutable_string_to_int(); 
        map["three"] = 3;
        map["one"] = 1;
    }
    {
        ::google::protobuf::Map< ::google::protobuf::int32, ::google::protobuf::int32>& map = 
            *m.mutable_int_to_int(); 
        map[1] = 2;
        map[3] = 4;
    }
    {
        ::google::protobuf::Map< ::google::protobuf::int32, Maps_MessageValue>& map = 
            *m.mutable_int_to_message_value(); 
        Maps_MessageValue mv; 
        mv.set_mv_field("one"); 
        map[1] = mv;
        mv.set_mv_field("two");
        map[2] = mv;
    }
    {
        ::google::protobuf::Map< ::google::protobuf::int32, Maps_EnumValue>& map = 
            *m.mutable_int_to_enum_value(); 
        Maps_MessageValue mv; 
        map[1] = Maps_EnumValue_EV_1;
        map[2] = Maps_EnumValue_EV_2;
    }
    {
        ::google::protobuf::Map< ::google::protobuf::int32, Maps_OneOfValue>& map = 
            *m.mutable_int_to_oneof_value(); 
        Maps_OneOfValue mo; 
        mo.set_ov_field1("one");
        map[1] = mo;
        mo.set_ov_field2(2);
        map[2] = mo;
    }

    return m;
}

int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_m(), "test18.c2ml.data");
    }
    else if(mode == "decode") {
        Maps m; 
        validate_decode(m, "test18.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}
