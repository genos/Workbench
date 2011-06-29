#!/usr/bin/env python
"""mersenne_primes.py
My submission to http://programmingpraxis.com/2011/06/03/mersenne-primes/
GRE, 6/4/11
"""

from itertools import count, ifilter, takewhile


def erat2():
    """Sieve of Eratosthenes; revised version of Python Cookbook recipe"""
    D = {}
    yield 2
    for q in count(3, 2):
        p = D.pop(q, None)
        if p is None:
            D[q * q] = q
            yield q
        else:
            x = p + q
            while x in D or not (x & 1):
                x += p
            D[x] = p


def primes(n):
    """Prime number less than n"""
    return takewhile(lambda p: p < n, erat2())


def lucas_lehmer(p):
    """Determines whether prime p is a Mersenne Prime via Lucas-Lehmer."""
    if p == 2:
        return True
    m, i, s = pow(2, p) - 1, 3, 4
    while i <= p:
        i += 1
        s = (pow(s, 2) - 2) % m
    return s == 0


if __name__ == "__main__":
    print [p for p in primes(256) if lucas_lehmer(p)]
