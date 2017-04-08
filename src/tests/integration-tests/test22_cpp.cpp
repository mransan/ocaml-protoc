#include <test22.pb.h>

#include <test_util.h>

#include <iostream>

using namespace foo::bar; 

Couple create_test_couple() {
    Couple cp; 
    {
        Person& p = *cp.mutable_p1();
        p.set_first_name("John");
        p.set_last_name("Doe"); 
        p.set_date_of_birth(19820429);
        p.set_employed_by("Google");
        p.set_gender(Male);
    }
    {
        Person& p = *cp.mutable_p2();
        p.set_first_name("Marie");
        p.set_last_name("Dupont"); 
        p.set_date_of_birth(19820306);
        p.set_employed_by("INRIA");
        
        Person_TelNumber& t = *p.mutable_tel_number();
        t.set_area_code(917);
        t.set_number(1111111);
        p.set_gender(Female);
        p.set_marital_status(Person_MaritalStatus_Married);
    }

    for(std::size_t i=0; i<2; ++i) {
        Person_TelNumber& cn = *cp.add_contact_numbers();
        cn.set_area_code(917);
        cn.set_number   (123450 + i);
    }

    return cp;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_couple(), "test22.c2ml.data");
    }
    else if(mode == "decode") {
        Couple cp; 
        validate_decode(cp, "test22.ml2c.data");
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

