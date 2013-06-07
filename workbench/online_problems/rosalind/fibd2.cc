#include <algorithm>
#include <iostream>
#include <fstream>
#include <numeric>
#include <vector>
#include <NTL/ZZ.h>


NTL::ZZ mortal_fib(uintmax_t n, uintmax_t m)
{
    std::vector<NTL::ZZ> rabbits(m);
    rabbits.at(0) = 1;
    for (decltype(n) i = 0; i < n - 1; ++i) {
        auto tmp = rabbits.at(0), total = NTL::to_ZZ(0);
        for (decltype(m) j = 1; j < m; ++j) {
            total += rabbits.at(j);
            std::swap(rabbits.at(j), tmp);
        }
        rabbits.at(0) = total;
    }
    return std::accumulate(std::begin(rabbits),
            std::end(rabbits),
            NTL::to_ZZ(0));
}


int main()
{
    std::ifstream f("data/rosalind_fibd.txt");
    uintmax_t n, m;
    f >> n;
    f >> m;
    f.close();
    std::cout << mortal_fib(n, m) << std::endl;
    return EXIT_SUCCESS;
}
