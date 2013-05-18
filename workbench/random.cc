#include <functional>
#include <iostream>
#include <random>
#include <vector>
#include <boost/lexical_cast.hpp>

std::mt19937_64 re;

int main(int argc, char *argv[]) {
    re.seed(argc > 1 ? boost::lexical_cast<uint_fast64_t>(argv[1]) : 1729);
    std::normal_distribution<float> nd(31 /* μ */, 8 /* σ */);
    std::vector<int> mn(64);
    auto norm = std::bind(nd, re);

    for (auto i = 0; i < 1200; ++i) ++mn.at(round(norm()));

    for (auto i = 0; i < mn.size(); ++i) {
        std::cout << i << '\t';
        for (auto j = 0; j < mn[i]; ++j) std::cout <<  '*';
        std::cout << std::endl;
    }
}
