#include <iostream>
#include <cstdint>

constexpr bool is_prime_recursive(std::uintmax_t number, std::uintmax_t c) {
    return (c * c > number) ? true :
        (number % c == 0) ? false :
            is_prime_recursive(number, c + 1);
}

constexpr bool is_prime_func(std::uintmax_t number) {
    return (number <= 1) ? false : is_prime_recursive(number, 2);
}

int main() {
   static_assert(is_prime_func(7), "...");
   std::uintmax_t i = 11;
   std::cout << std::boolalpha << is_prime_func(i) << std::endl;
   std::cout << is_prime_func(4256233) << std::endl;
   std::cout << is_prime_func(86028121) << std::endl;
   return 0;
}
