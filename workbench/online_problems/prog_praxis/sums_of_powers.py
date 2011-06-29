#!/usr/bin/env python

import numpy as np
from fractions import Fraction
from operator import mul


def a_t(n):
    A = np.zeros(shape=(n + 1, n + 1), dtype=Fraction)
    for j in xrange(n + 1):
        A[0, j] = Fraction(1, j + 1)
    for i in xrange(1, n + 1):
        for j in xrange(n - i + 1):
            A[i, j] = (j + 1) * (A[i - 1, j] - A[i - 1, j + 1])
    return A


def b(n):
    return a_t(n)[n, 0]


def c(n, k):
    num = reduce(mul, xrange(n - k + 1, n + 1), 1)
    denom = reduce(mul, xrange(1, k + 1), 1)
    return num / denom


def s_naive(m, n):
    return sum(pow(k, m) for k in xrange(1, n + 1))


def s(m, n):
    s = sum(c(m + 1, k) * b(k) * pow(n, m + 1 - k) for k in xrange(m + 1))
    return s // (m + 1)


if __name__ == "__main__":
    print b(18)
    print a_t(18)[:, 0]
    print s_naive(10, 3)
    print s(10, 3)
    print s_naive(10, 1000)
    print s(10, 1000)

"""
I used this as an opportunity to play with NumPy's (http://numpy.scipy.org/)
n-dimensional arrays. Also uses the fractions module for rational numbers.
[sourcecode lang="python"]
#!/usr/bin/env python

import numpy as np
from fractions import Fraction
from operator import mul


def a_t(n):
    A = np.zeros(shape=(n + 1, n + 1), dtype=Fraction)
    for j in xrange(n + 1):
        A[0, j] = Fraction(1, j + 1)
    for i in xrange(1, n + 1):
        for j in xrange(n - i + 1):
            A[i, j] = (j + 1) * (A[i - 1, j] - A[i - 1, j + 1])
    return A


def b(n):
    return a_t(n)[n][0]


def c(n, k):
    num = reduce(mul, xrange(n - k + 1, n + 1), 1)
    denom = reduce(mul, xrange(1, k + 1), 1)
    return num / denom


def s_naive(m, n):
    return sum(pow(k, m) for k in xrange(1, n + 1))


def s(m, n):
    s = sum(c(m + 1, k) * b(k) * pow(n, m + 1 - k) for k in xrange(m + 1))
    return s // (m + 1)


if __name__ == "__main__":
    print b(18)
    print a_t(18)[:, 0]
    print s_naive(10, 3)
    print s(10, 3)
    print s_naive(10, 1000)
    print s(10, 1000)
[/sourcecode]
"""
