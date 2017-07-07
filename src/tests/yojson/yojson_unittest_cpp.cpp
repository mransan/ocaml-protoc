#include "yojson_unittest.pb.h"
#include "google/protobuf/util/json_util.h"

#include <iostream>
#include <fstream>

AllBasicTypes all_basic_types() {
  AllBasicTypes ret;
  ret.set_field01(1.2000001); 
  ret.set_field02(1.2);
  ret.set_field03(0xEFFFFFFFl);
  ret.set_field04(0xEBABABABABABABABL);
  ret.set_field05(0x7FFFFFFFl);
  ret.set_field06(0x7BABABABABABABABL);
  ret.set_field07(0xEFFFFFFFl);
  ret.set_field08(0xEBABABABABABABABL);
  ret.set_field09(0xEFFFFFFFl);
  ret.set_field10(0xEBABABABABABABABL);
  ret.set_field13(true);
  ret.set_field14("This is a test \"string\"");

  ret.add_repeated01(1.2000001); 
  ret.add_repeated02(1.2); 
  ret.add_repeated03(0xEFFFFFFFl); 
  ret.add_repeated04(0xEBABABABABABABABL); 
  ret.add_repeated05(0x7FFFFFFFl); 
  ret.add_repeated06(0x7BABABABABABABABL); 
  ret.add_repeated07(0xEFFFFFFFl); 
  ret.add_repeated08(0xEBABABABABABABABL); 
  ret.add_repeated09(0xEFFFFFFFl); 
  ret.add_repeated10(0xEBABABABABABABABL); 
  ret.add_repeated13(true); 
  ret.add_repeated14("This is a test \"string\""); 

  return ret;
}


SmallMessage small_message () {
  SmallMessage ret; 
  ret.set_sm_string("This \"IS\" a small string");

  return ret;
}
  
Enum test_enum0() { return VALUE0; }
Enum test_enum1() { return Value1; }
Enum test_enum2() { return Value_Two; }
  
SingleOneOf single_one_of_string() {
  SingleOneOf ret; 
  ret.set_string_value("This is the single one \"string\"");
  return ret;
}

SingleOneOf single_one_of_int() {
  SingleOneOf ret; 
  ret.set_int_value(0xEFABABABl);
  return ret;
}

SingleOneOf single_one_of_enum() {
  SingleOneOf ret; 
  ret.set_enum_value(test_enum0());
  return ret;
}

SingleOneOf single_one_of_small_message() {
  SingleOneOf ret; 
  *(ret.mutable_small_message()) = small_message ();
  return ret;
}

SingleOneOf single_one_of_recursive() {
  SingleOneOf ret; 
  *(ret.mutable_recursive_value()) = single_one_of_small_message (); 
  return ret;
}

Test test() {
  Test ret; 
  *(ret.mutable_all_basic_types()) = all_basic_types();
  ret.set_test_enum0(test_enum0()); 
  ret.set_test_enum1(test_enum1()); 
  ret.set_test_enum2(test_enum2()); 
  *(ret.mutable_single_one_of_string()) = single_one_of_string();
  *(ret.mutable_single_one_of_int()) = single_one_of_int();
  *(ret.mutable_single_one_of_enum()) = single_one_of_enum();
  *(ret.mutable_single_one_of_small_message()) = single_one_of_small_message();
  *(ret.mutable_single_one_of_recursive()) = single_one_of_recursive();
  ret.add_repeated_enum(test_enum0());
  ret.add_repeated_enum(test_enum1());
  ret.add_repeated_enum(test_enum2());

  return ret;
}

int main() {

  std::string json; 
  google::protobuf::util::MessageToJsonString(test(), &json); 

  {
    static const std::string filename("yojson.data");
    std::ofstream out(filename.c_str()); 
    out << json;
    if(!out.good()) {
      std::cerr << "Error writing to file " << filename << std::endl;
      return 1; 
    }
    out.close();
  }

  return 0;
}
