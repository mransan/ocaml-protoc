#include <test01.pb.h>
#include <google/protobuf/util/message_differencer.h>

#include <test_util.h>

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
        return encode_to_file(create_test_couple(), "test01.c2ml.data");
    }
    else if(mode == "decode") {
        Couple cp;
        try {
          validate_decode(cp, "test01.ml2c.data", true);
        }
        catch(std::runtime_error const& e) {
          std::cerr << "runtime error: " << e.what() << std::endl;
          return 2;
        }
        using google::protobuf::util::MessageDifferencer;
        Couple expected = create_test_couple();
        assert(MessageDifferencer::ApproximatelyEquals(
              cp.p1(), expected.p1()));
        assert(MessageDifferencer::ApproximatelyEquals(
              cp.p2(), expected.p2()));
        assert(cp.contact_numbers().size() == 
              expected.contact_numbers().size());
        for(std::size_t i=0; i<cp.contact_numbers().size(); ++i) {
           assert(MessageDifferencer::ApproximatelyEquals(
                 cp.contact_numbers()[i], expected.contact_numbers()[i]));
        }
    }
    else {
        std::cerr << "Invalid second argument: "
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

