// maybe.cc
// Example maybe.h usage

#include <iostream>
#include <vector>
#include "maybe.h"

using namespace maybe;

auto plus2(double d) -> double { return d + 2.0; }
auto under2(double d) -> Maybe<double> {
    return d ? Just(2.0 / d) : Nothing();
}

auto main() -> int {
    using MD = Maybe<double>;
    auto maybes = std::vector<MD>{Just(17.0), Just(0.0), Nothing()};
    std::cout << std::fixed;
    auto actions = std::vector<std::tuple<std::string, std::function<MD(MD)>>>{
        std::make_tuple("id", [](MD m){ return m; }),
        std::make_tuple("fmap(plus2, m)", [](MD m){ return fmap(plus2, m); }),
        std::make_tuple("plus2 ^ m", [](MD m){ return plus2 ^ m; }),
        std::make_tuple("m >>= under2", [](MD m){ return m >>= under2; }),
    };
    for (auto a: actions) {
        std::cout << std::get<0>(a) << ":" << std::endl;
        for (auto m: maybes) {
            std::cout << "\t" << std::get<1>(a)(m) << std::endl;
        }
    }
}
