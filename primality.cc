// from http://efesx.com/2010/01/27/compile-time-primality-test/
#include <iostream>

using number = uintmax_t;

// square root computation, using binary search
template <number num, number begin = 0, number end = num> class Sqrt {
private:
  const static number mid = (begin + end) / 2;
  const static number midsqr = mid * mid;

public:
  const static number res = Sqrt < num, (midsqr < num) ? mid + 1 : begin,
                      (midsqr < num) ? end : mid > ::res;
};

// specialization for base case
template <number num, number lim> struct Sqrt<num, lim, lim> {
  const static number res = lim;
};

// check for divisors
template <number num, number begin = 2, number end = Sqrt<num>::res>
struct has_any_divs {
  const static bool res =
      !(num % begin) || has_any_divs<num, begin + 1, end>::res;
};

// base case for divisors
template <number num, number lim> struct has_any_divs<num, lim, lim> {
  const static bool res = !(num % lim);
};

// primality check (trial division)
template <number num> struct is_prime {
  const static bool res = !has_any_divs<num>::res;
};

// specializations for base cases
template <> struct is_prime<2> {
  const static bool res = true;
};
template <> struct is_prime<1> {
  const static bool res = false;
};
template <> struct is_prime<0> {
  const static bool res = false;
};

int main() {
  std::cout << std::boolalpha;
  std ::cout << "Is 65537 prime? " << is_prime<65537>::res << std::endl;
  std ::cout << "Is 65549 prime? " << is_prime<65549>::res << std::endl;

  // static assertions re: primality
  static_assert(is_prime<17>::res, "17 IS NOT (?) prime");
  static_assert(!is_prime<57>::res, "57 IS (?) prime");
  return 0;
}
