#include <iostream>
#include <cstdint>

constexpr auto is_prime_recursive(uintmax_t number, uintmax_t c) -> bool
{
    return (c * c > number) ? true :
        (number % c == 0) ? false :
            is_prime_recursive(number, c + 1);
}

constexpr auto is_prime_func(uintmax_t number) -> bool
{
    return (number <= 1) ? false : is_prime_recursive(number, 2);
}

auto main() -> int
{
   static_assert(is_prime_func(7), "...");
   auto i = 11;
   std::cout << std::boolalpha << is_prime_func(i) << std::endl;
   std::cout << is_prime_func(4256233) << std::endl;
   std::cout << is_prime_func(86028121) << std::endl;
   return 0;
}
