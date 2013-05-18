#include <algorithm>
#include <iostream>
#include <iterator>
#include <map>
#include <utility>

int main() {

    std::map<std::string, unsigned> histogram;
    std::istream_iterator<std::string> ii(std::cin);
    std::istream_iterator<std::string> eos;

    std::for_each(ii, eos,
            [&histogram](std::string s){
                ++histogram[s];
            });
    std::for_each(histogram.begin(), histogram.end(),
            [](std::pair<const std::string, unsigned> p){
                std::cout << p.first << ' ' << p.second << std::endl;
            });

    return EXIT_SUCCESS;
}
