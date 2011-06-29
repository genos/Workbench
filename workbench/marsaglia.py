#!/usr/bin/env python

from collections import deque

def mwc(a, b, xs, c):
    """Multiply With Carry PRNG; see
    http://en.wikipedia.org/wiki/Multiply-with-carry
    Example:
    >>> prng = mwc(7, 10, [1], 3)
    >>> [next(prng) for _ in xrange(20)]
    [1, 0, 1, 7, 9, 7, 5, 0, 4, 8, 8, 1, 3, 2, 6, 3, 5, 7, 2, 9]
    """
    r = len(xs)
    xs = deque(xs)
    while True:
        x_out = xs.popleft()
        x_in = (a * x_out + c) % b
        c = (a * x_out + c) / b
        xs.append(x_in)
        yield x_out


def xor128():
    """Xorshift PRNG; see http://en.wikipedia.org/wiki/Xorshift
    Example:
    >>> prng = xor128()
    >>> [next(prng) for _ in xrange(4)]
    [252977563114L, 646616338854L, 476657867818L, 294684809458L]
    """
    x = 123456789
    y = 362436069
    z = 521288629
    w = 88675123
    while True:
        t = x ^ (x << 11)
        x, y, z = y, z, w
        w = w ^ (w >> 19) ^ (t ^ (t >> 8))
        yield w
