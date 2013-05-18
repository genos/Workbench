#!/usr/bin/env python
# http://mathworld.wolfram.com/GreedyAlgorithm.html
from itertools import ifilter, imap, product, starmap
from operator import mul


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def ggcd(*args):
    return reduce(gcd, args)


def lcm(a, b):
    A, B = abs(a), abs(b)
    M, m = max(A, B), min(A, B)
    return M if m == 0 else (M / gcd(A, B)) * m


def llcm(*args):
    return reduce(lcm, args)


def dot(u, v):
    return sum(starmap(mul, zip(u, v)))


LIM = llcm(6, 9, 20)
NMC = set(xrange(LIM)) - set(ifilter(lambda s: s < LIM,
                                       imap(sum, product(xrange(0, 1 + LIM, 6),
                                                         xrange(0, 1 + LIM, 9),
                                                         xrange(0, 1 + LIM, 20)))))


def greedy(n, a=(6, 9, 20)):
    if n in NMC: return None
    c = [0] * len(a)
    i = len(a) - 1
    c[i] = n // a[i]
    d = n - dot(c, a)
    while d:
        if c[i] > 1:
            c[i] -= 1
            for j in xrange(i): c[j] = 0
            d = n - dot(c, a)
            for j in xrange(i - 1, -1, -1):
                c[j] = d // a[j]
                d = n - dot(c, a)
        else:
            c = [0] * len(a)
            i -= 1
            c[i] = n // a[i]
            d = n - dot(c, a)
    return tuple(c)


if __name__ == "__main__":
    print greedy(43)
    print greedy(62)
    print greedy(12)
