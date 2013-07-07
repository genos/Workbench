// An example of using Qi from Boost::Spirit, based on
// http://www.boost.org/doc/libs/1_54_0/libs/spirit/example/qi/num_list1.cpp
#include <iostream>
#include <boost/spirit/include/qi.hpp>

namespace client {
    namespace qi = boost::spirit::qi;
    namespace ascii = boost::spirit::ascii;

    template <typename Iterator>
    auto parse_numbers(Iterator first, Iterator last) -> bool
    {
        auto r = qi::phrase_parse(first,
                                  last,
                                  qi::double_ >> *(',' >> qi::double_),
                                  ascii::space);
        if (first != last) {
            return false;
        } else {
            return r;
        }
    }
}


auto main() -> int
{
    std::cout << "//////////////////////////////////////////////////////\n\n";
    std::cout << "\t\tA comma separated list parser for Spirit...\n\n";
    std::cout << "//////////////////////////////////////////////////////\n\n";
    std::cout << "Give me a comma separated list of numbers.\n";
    std::cout << "Type [q or Q] to quit\n\n" << std::endl;

    auto line = "-------------------------\n";
    std::string str;
    while (getline(std::cin, str)) {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q') {
            break;
        } else if (client::parse_numbers(str.begin(), str.end())) {
            std::cout << line << "Parsing succeeded\n" << line <<
                str << " Parses OK." << std::endl;
        } else {
            std::cout << line << "Parsing failed!\n" << line;
        }
    }

    std::cout << "Bye... :-)" << std::endl;
}
