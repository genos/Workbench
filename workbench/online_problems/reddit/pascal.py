#!/usr/bin/env python
"""pascal.py

Here's my solution to a challenge on reddit.com/r/programmingchallenges to
compute rows of Pascal's Triangle and layers of Pascal's Pyramid/Tetrahedron.
GRE, 4/16/11
"""
from itertools import product
from operator import mul


def prod(nums):
    """
    Why would Python have sum() but not prod()? Silly BDFL, always hating on
    functional programming
    """
    return reduce(mul, nums, 1)


def falling_factorial(n, k):
    """
    Pochammer Symbol: (n)_{n - k} = n * (n - 1) * ... * (n - k + 1) = n!/(n-k)!
    """
    return prod(xrange(n - k + 1, n + 1))


def factorial(n):
    """
    The usual; superfluous, could just use falling_factorial(n, n) instead
    """
    return prod(xrange(1, n + 1))


def binomial(n, k):
    """
    Binomial coefficient a.k.a. n choose k
    """
    return falling_factorial(n, k) // factorial(k)


def pascal(n):
    """
    nth row of Pascal's Triangle
    """
    return [binomial(n, k) for k in xrange(n + 1)]


def trinomial(n, i, j, k):
    """
    Trinomial coefficient
    """
    return factorial(n) // prod(factorial(x) for x in (i, j, k))


def partition_triples(n):
    """
    Triples (i, j, k) such that i + j + k = n
    """
    return sorted(filter(lambda trip: sum(trip) == n, product(xrange(n + 1),
        repeat=3)))


def pascal3(n):
    """
    nth layer of Pascal's Pyramid
    """
    layer = []
    pts = partition_triples(n)
    for i in xrange(1, n + 2):
        layer.append([trinomial(n, *pts.pop()) for _ in xrange(i)])
    return layer


if __name__ == "__main__":
    import sys
    N1 = int(sys.argv[1]) if sys.argv[1:] else 4
    N2 = int(sys.argv[2]) if sys.argv[2:] else 3
    print "Row %s of Pascal's Triangle:" % N1
    print pascal(N1)
    print "Layer %s of Pascal's Pyramid:" % N2
    for row in pascal3(N2):
        print row
