#include <iostream>
#include <numeric>
#include <vector>

double average(const std::vector<double>& xs) {
    if (xs.empty()) {
        return 0.0;
    } else {
        return std::accumulate(xs.begin(),
                               xs.end(),
                               0.0,
                               [](double x, double y){return x + y;}
                               ) / xs.size();
    }
}


int main() {
    unsigned n;
    std::vector<double> xs;
    std::cout << "Number of numbers?" << std::endl;
    std::cin >> n;
    if (n > 0) {
        std::cout << std::endl << "Numbers?" << std::endl;
        for (decltype(n) i = 0; i < n; ++i) {
            double x;
            std::cin >> x;
            xs.push_back(x);
        }
    }
    std::cout << "Average:" << std::endl;
    std::cout << average(xs) << std::endl;
    return EXIT_SUCCESS;
}
