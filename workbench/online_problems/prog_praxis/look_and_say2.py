#!/usr/bin/env python
from __future__ import division


eps = 1e-12


def newtons_method(f, fp, x0):
    x, i = x0, 0
    while abs(f(x)) > eps and i < 1e3:
        i += 1
        x -= f(x) / fp(x)
    return x


def horner(cs):
    return lambda x: reduce(lambda a, b: a * x + b, reversed(cs), 0)


if __name__ == "__main__":
    cs = [-6, 3, -6, 12, -4, 7, -7, 1, 0, 5, -2, -4, -12, 2, 7, 12, -7, -10,
            -4, 3, 9, -7, 0, -8,  14,  -3, 9, 2, -3, -10, -2, -6, 1, 10, -3, 1,
            7, -7, 7, -12, -5, 8, 6, 10, -8, -8, -7, -3, 9, 1,  6, 6, -2, -3,
            -10, -2, 3, 5, 2, -1, -1, -1, -1, -1, 1, 2, 2, -1, -2, -1, 0, 1]
    f = horner(cs)
    fp = horner([i * cs[i] for i in xrange(1, len(cs))])
    print newtons_method(f, fp, 1.3)

"""
Come to think of it, I didn't actually use Horner's Rule to construct my
polynomials, just the naive definition. Apologies on misnaming my functions!
A version that actually uses Horner's Rule:
[sourcecode lang="python"]
from __future__ import division

eps = 1e-12

def newtons_method(f, fp, x0):
    x, i = x0, 0
    while abs(f(x)) > eps and i < 1e3:
        i += 1
        x -= f(x) / fp(x)
    return x

def horner(cs):
    return lambda x: reduce(lambda a, b: a * x + b, reversed(cs), 0)

if __name__ == "__main__":
    cs = [-6, 3, -6, 12, -4, 7, -7, 1, 0, 5, -2, -4, -12, 2, 7, 12, -7, -10,
            -4, 3, 9, -7, 0, -8,  14,  -3, 9, 2, -3, -10, -2, -6, 1, 10, -3, 1,
            7, -7, 7, -12, -5, 8, 6, 10, -8, -8, -7, -3, 9, 1,  6, 6, -2, -3,
            -10, -2, 3, 5, 2, -1, -1, -1, -1, -1, 1, 2, 2, -1, -2, -1, 0, 1]
    f = horner(cs)
    fp = horner([i * cs[i] for i in xrange(1, len(cs))])
    print newtons_method(f, fp, 1.3)
[/sourcecode]
"""
