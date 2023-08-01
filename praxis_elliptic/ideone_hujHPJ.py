#!/usr/bin/env python3

# elliptic curve factorization

from math import gcd, isqrt, floor, log


def square(x):
    return x * x


def ilog(n, b):
    return int(floor(log(n) / log(b)))


def bits(n):
    if n == 0:
        return []
    return bits(n // 2) + [n % 2]


def sieve(n):
    b, p, ps = [True] * (n + 1), 2, []
    for p in range(2, n + 1):
        if b[p]:
            ps.append(p)
            for i in range(p, n + 1, p):
                b[i] = False
    return ps


def primes(lo, hi, delta):
    result = []
    bits = [True] * delta
    ps = sieve(isqrt(hi))[1:]
    m = len(ps)
    qs = [0] * m
    for i in range(m):
        qs[i] = ((lo + ps[i] + 1) // -2) % ps[i]
    while lo < hi:
        for i in range(delta):
            bits[i] = True
        for i in range(m):
            for j in range(qs[i], delta, ps[i]):
                bits[j] = False
            qs[i] = (qs[i] - delta) % ps[i]
        for i in range(delta):
            t = lo + 2 * i + 1
            if bits[i] and t < hi:
                result.append(t)
        lo = lo + 2 * delta
    return result


def add(p, q, r, n):  # r = p - q
    t1 = ((p[0] + p[1]) * (q[0] - q[1])) % n
    t2 = ((p[0] - p[1]) * (q[0] + q[1])) % n
    return (square(t2 + t1) * r[1]) % n, (square(t2 - t1) * r[0]) % n


def double(p, an, ad, n):
    t1 = square(p[0] + p[1]) % n
    t2 = square(p[0] - p[1]) % n
    t3 = t1 - t2
    return (t1 * t2 * 4 * ad) % n, (((t3 * an) + (t2 * ad * 4)) * t3) % n


def mul(k, p, an, ad, n):
    if k == 0:
        return 0, 0
    if k == 1:
        return p
    if k == 2:
        return double(p, an, ad, n)
    q = double(p, an, ad, n)
    r = p
    for m in bits(k)[1:]:
        if m % 2 == 1:
            r = add(q, r, p, n)
            q = double(q, an, ad, n)
        else:
            q = add(r, q, p, n)
            r = double(r, an, ad, n)
    return r


def factor(n, b1, b2, m, s):
    u = (s * s - 5) % n
    v = (4 * s) % n
    an = (pow(v - u, 3) * (3 * u + v)) % n
    ad = (4 * pow(u, 3) * v) % n
    q = pow(u, 3) % n, pow(v, 3) % n
    for p in sieve(b1):
        q = mul(pow(p, ilog(b1, p)), q, an, ad, n)
    g = gcd(q[1], n)
    if 1 < g < n:
        return 1, g
    c = [(0, 0)] * (m + 1)
    b = [(0, 0)] * (m + 1)
    c[1] = double(q, an, ad, n)
    c[2] = double(c[1], an, ad, n)
    for i in range(1, m + 1):
        if i > 2:
            c[i] = add(c[i - 1], c[1], c[i - 2], n)
        b[i] = (c[i][0] * c[i][1]) % n
    g = 1
    t = mul(b1 - 1 - 2 * m, q, an, ad, n)
    r = mul(b1 - 1, q, an, ad, n)
    for i in range(b1 - 1, b2, 2 * m):
        a = (r[0] * r[1]) % n
        for p in primes(i + 2, i + 2 * m, m):
            d = (p - i) // 2
            g = g * (((r[0] - c[d][0]) * (r[1] + c[d][1])) - a + b[d]) % n
        r, t = add(r, c[m], t, n), r
    g = gcd(g, n)
    if 1 < g < n:
        return 2, g
    return "factorization failed"


# both return 1113491139767

print(
    factor(
        828519338246984285695697941578258145583831516583983,
        50000,
        2500000,
        210,
        3902383298,
    )
)  # first stage

print(
    factor(
        828519338246984285695697941578258145583831516583983,
        25000,
        2500000,
        210,
        3902383298,
    )
)  # second stage
