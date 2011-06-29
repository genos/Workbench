#!/usr/bin/env python


def mul(n, m):
    prod = 0
    while n != 1:
        if n & 1: prod += m
        m <<= 1
        n >>= 1
    return prod + m


def div(n, m):
    t = m
    while t <= n: t <<= 1
    t, q, r = t >> 1, 0, n
    while t >= m:
        q <<= 1
        if t <= r: q, r = q + 1, r - t
        t >>= 1
    return q, r


def gcd(n, m):
    if n == 0 or m == 0: return m + n
    elif not n & 1 and not m & 1: return gcd(n >> 1, m >> 1) << 1
    elif not n & 1: return gcd(n >> 1, m)
    elif not m & 1: return gcd(n, m >> 1)
    else: return gcd(min(n, m), max(n, m) - min(n, m))


if __name__ == "__main__":
    print mul(100, 50)
    print div(100, 50)
    print gcd(100, 50)
