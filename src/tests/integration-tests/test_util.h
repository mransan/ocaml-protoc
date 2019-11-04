
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cassert>

#include <google/protobuf/util/message_differencer.h>

using google::protobuf::util::MessageDifferencer;

template <typename T>
int encode_to_file(T const& message, std::string const& file_name) {

    std::ofstream out(file_name.c_str());
    message.SerializeToOstream(&out);
    if(! out.good()) {
        std::cerr << "Error writing message to file"
                  << std::endl;
        return 1;
    }
    out.close(); 
    return 0;
}

template <typename T>
int decode_from_file(T& message, std::string const& file_name) {

    std::ifstream in(file_name.c_str()); 
    if(!in.is_open() || !in.good()) {
        std::cerr << "Error opening the file"
                  << std::endl;
        return 1;
    }
    else {
      std::cout << "Success opening the file " << file_name << std::endl;
    }
    bool success = message.ParseFromIstream(&in);
    std::cout << "in.good(): " << std::boolalpha << in.good()
        << ", in.eof(): " << in.eof() << std::endl;
    if(!success) {
        return 1;
    }
    else {
        return 0;
    }
}

inline int check_argv(int argc, char const * const argv[]) {
    if(argc < 2) {
        std::cerr << "Invalid number of argument, must be: "
                  << std::endl
                  << argv[0]
                  << " [encode|decode]"
                  << std::endl;

        exit(1);
    }
    return 0;
}

template<typename T> 
void validate_decode(T& message, 
                     std::string const& file_name, 
                     bool print = true) {
    int rc = decode_from_file(message, file_name);
    if(rc) {
        std::cerr << "C++: Failed to decode" 
                  << std::endl; 
        exit(1);
    }
    else {
        std::cout << "C++: -- Good --" 
                  << std::endl;
        if(print) {
            std::cout << message.DebugString()
                      << std::endl;
        }
    }
} 
