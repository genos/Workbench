#!/usr/bin/env python

from __future__ import division
from itertools import count, takewhile
from operator import mul


def factorial(n):
    return reduce(mul, xrange(2, n + 1), 1)


def sin_taylor(x, eps):
    a = lambda k: pow(x, 2 * k + 1) / factorial(2 * k + 1)
    return sum(pow(-1, k) * a(k) for k in takewhile(lambda k: a(k) > eps,
        count()))


def sin_limit(x, eps):
    #Loses accuracy if eps < 1e-6...
    if x < eps:
        return eps
    else:
        return 3 * sin_limit(x / 3, eps) - 4 * pow(sin_limit(x / 3, eps), 3)
