#!/usr/bin/env python

from operator import mul


def falling_factorial(n, k):
    """(n)_k = n! / (n - k)! n * (n - 1) * ... * (n - k + 1)"""
    return reduce(mul, xrange(n - k + 1, n + 1), 1)


def binom(n, k):
    """n! / (k! * (n-k)!) = (n)_k / k!; k! = (k)_k"""
    return falling_factorial(n, k) / falling_factorial(k, k)


if __name__ == "__main__":
    print 100 * binom(24, 2) * binom(3, 1) / float(binom(27, 3))
