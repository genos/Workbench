#!/usr/bin/env python

import random
from cookbook import primes
# Sieve of Eratosthenes based primes < n, speedy and space efficient
from fractions import gcd
# Speedier than defining it ourselves

def is_fermat_pseudo_prime(a, n):
    return gcd(a, n) == 1 and pow(a, n-1, n) == 1

def _decompose(n):
    d, s = n, 0
    while d % 2 == 0:
        d //= 2
        s += 1
    return d, s

def is_strong_pseudo_prime(a, n):
    d, s = _decompose(n)
    if pow(a, d, n) == 1:
        return True
    else:
        for r in xrange(s):
            if pow(a, d * pow(2, r), n) == n - 1:
                return True
    return False

def primality_test(method, n, times = 42):
    for _ in xrange(times):
        a = random.randrange(n)
        if not method(a, n):
            return False
    return True

def is_carmichael1(n):
    ps = primes(n + 1)
    if ps[-1] == n:
        return False
    else:
        for p in ps:
            if not gcd(p, n) == 1: continue
            elif not is_fermat_pseudo_prime(p, n): return False
        return True

def _factors(n):
    return [i for i in primes(n + 1) if n % i == 0]

def is_carmichael2(n):
    fs = _factors(n)
    if n % 2 == 0 or fs[-1] == n: return False
    for f in fs:
        if n % pow(f, 2) == 0 or (n - 1) % (f - 1) != 0:
            return False
    return True

def carmichael(method, n):
    return [c for c in xrange(561, n) if method(c)]

if __name__ == "__main__":
    n = 10000
    print is_fermat_pseudo_prime(2, 561)
    print is_fermat_pseudo_prime(3, 561)
    print is_strong_pseudo_prime(2, 561)
    print is_strong_pseudo_prime(3, 561)
    print primality_test(is_fermat_pseudo_prime, 561)
    print primality_test(is_strong_pseudo_prime, 561)
    print carmichael(is_carmichael1, n)                 # Slow
    print carmichael(is_carmichael2, n)                 # Slow
