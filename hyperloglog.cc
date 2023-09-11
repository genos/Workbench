// hyperloglog.cc
// from "HyperLogLog: the analysis of a near-optimal cardinality estimation
// algorithm," by Flajolet, Fusy, Gandouet, & Meunier
#include <cmath>
#include <fstream>
#include <iostream>
#include <numeric>
#include <vector>

auto num_zeros(const std::vector<uint32_t> &xs) -> uint32_t {
  return std::accumulate(
      std::begin(xs), std::end(xs), 0,
      [](uint32_t x, uint32_t y) -> uint32_t { return x + (y == 0); });
}

auto h(const std::string &s) -> uint32_t {
  // FNV-1a
  const auto fnv_offset_basis = uint32_t{2166136261};
  return std::accumulate(std::begin(s), std::end(s), fnv_offset_basis,
                         [](uint32_t h, char c) -> uint32_t {
                           static const auto fnv_prime = uint32_t{1676619};
                           return fnv_prime * (h ^ c);
                         });
}

auto rho(uint32_t s) -> uint32_t {
  return 32 - static_cast<uint32_t>(log(std::max(s, 1u)));
}

auto alpha(uint32_t m) -> double {
  switch (m) {
  case 16:
    return 0.673;
    break;
  case 32:
    return 0.697;
    break;
  case 64:
    return 0.709;
    break;
  default:
    return 0.7123 / (1 + 1.079 / m);
    break;
  }
}

auto hyperloglog(std::ifstream &data, uint32_t b) -> double {
  auto m = uint32_t{1u << b}, bf32 = uint32_t{32u - b};
  auto M = std::vector<uint32_t>(m);
  auto v = std::string{""};
  while (data.good()) {
    data >> v;
    auto x = h(v);
    auto j = x >> bf32;
    auto w = x & ((1u << bf32) - 1);
    M[j] = std::max(M[j], rho(w));
  }
  auto E = alpha(m) * m * m /
           std::accumulate(
               std::begin(M), std::end(M), 0,
               [](double x, double y) -> double { return x + pow(2, -y); });
  auto E_star = E, two32 = pow(2.0, 32);
  if (E < 2.5 * m) {
    auto V = num_zeros(M);
    if (V != 0) {
      E_star = m * log(m / static_cast<double>(V));
    }
  } else if (E > two32 / 30.0) {
    E_star = -two32 * log(1.0 - E / two32);
  }
  return E_star;
}

auto main(int argc, char *argv[]) -> int {
  if (argc != 3) {
    std::cerr << "usage: " << argv[0] << " <file> <b>" << std::endl;
    return EXIT_FAILURE;
  } else {
    auto b = static_cast<uint32_t>(std::stoi(argv[2]));
    if (b > 31) {
      std::cerr << b << " too large" << std::endl;
      return EXIT_FAILURE;
    } else {
      std::ifstream data(argv[1]);
      std::cout << hyperloglog(data, b) << std::endl;
      data.close();
      return EXIT_SUCCESS;
    }
  }
}
