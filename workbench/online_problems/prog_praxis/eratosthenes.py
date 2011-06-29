#!/usr/bin/env python
"""eratosthenes.py
Sieve of Eratosthenes, for
http://programmingpraxis.com/2009/02/19/sieve-of-eratosthenes/

GRE, 5/15/11
"""

from itertools import ifilter
from math import sqrt


def isqrt(n):
    """Integral floor of sqrt(n)"""
    return int(sqrt(n))


def sieve(nums, n):
    """Eratosthenes"""
    prime = next(nums)
    # Optimization 2: Start sieving at prime * prime
    mults = xrange(prime * prime, n, prime)
    return prime, ifilter(lambda x: x not in mults, nums)


def primes(n=2):
    """Simple prime generator"""
    yield 2
    # Optimization 1: consider only odds
    nums = iter(xrange(3, n, 2))
    # Optimization 3: stop sieving at sqrt(n)
    for _ in xrange(isqrt(n)):
        prime, nums = sieve(nums, n)
        yield prime
    for prime in nums:
        yield prime


if __name__ == "__main__":
    import sys
    print list(primes(30))
    print sum(1 for _ in primes(15485863))
