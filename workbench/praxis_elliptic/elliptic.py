# elliptic curve factorization

from collections import namedtuple
from fractions import gcd
from math import sqrt, log


#Point = namedtuple("Point", ["x", "z"])
Point = namedtuple("Point", "x z")


def isqrt(n):
    return int(sqrt(n))


def ilog(n, b):
    return int(log(n) / log(b))


def bits(n):
    # return [int(b) for b in bin(n)[2:]]
    bs = []
    while n > 0:
        n, r = divmod(n, 2)
        bs.append(r)
    return list(reversed(bs))


def sieve(n):
    b, p, ps = [True] * (n + 1), 2, []
    for p in xrange(2, n + 1):
        if b[p]:
            ps.append(p)
            for i in xrange(p, n + 1, p):
                b[i] = False
    return ps


def primes(lo, hi, delta):
    result = []
    bits = [True] * delta
    ps = sieve(isqrt(hi))[1:]
    m = len(ps)
    qs = [0] * m
    for i in xrange(m):
        qs[i] = ((lo + ps[i] + 1) / -2) % ps[i]
    while lo < hi:
        for i in xrange(delta):
            bits[i] = True
        for i in xrange(m):
            for j in xrange(qs[i], delta, ps[i]):
                bits[j] = False
            qs[i] = (qs[i] - delta) % ps[i]
        for i in xrange(delta):
            t = lo + 2 * i + 1
            if bits[i] and t < hi:
                result.append(t)
        lo = lo + 2 * delta
    return result


def add(p, q, r, n):  # r = p - q
    t1 = ((p.x + p.z) * (q.x - q.z)) % n
    t2 = ((p.x - p.z) * (q.x + q.z)) % n
    return Point((pow(t2 + t1, 2, n) * r.z) % n,
            (pow(t2 - t1, 2, n) * r[0]) % n)


def double(p, an, ad, n):
    t1 = pow(p.x + p.z, 2, n)
    t2 = pow(p.x - p.z, 2, n)
    t3 = t1 - t2
    return Point((t1 * t2 * 4 * ad) % n,
            ((t3 * an + t2 * ad * 4) * t3) % n)


def mul(k, p, an, ad, n):
    if k == 0:
        return Point(0, 0)
    elif k == 1:
        return p
    elif k == 2:
        return double(p, an, ad, n)
    else:
        q = double(p, an, ad, n)
        r = p
        for m in bits(k)[1:]:
            if m & 1 == 1:
                r = add(q, r, p, n)
                q = double(q, an, ad, n)
            else:
                q = add(r, q, p, n)
                r = double(r, an, ad, n)
        return r


def factor(n, b1, b2, m, s):
    u = (s * s - 5) % n
    v = (4 * s) % n
    an = (pow(v - u, 3, n) * (3 * u + v)) % n
    ad = (4 * pow(u, 3) * v) % n
    q = Point(pow(u, 3, n), pow(v, 3, n))
    for p in sieve(b1):
        q = mul(pow(p, ilog(b1, p), n), q, an, ad, n)
    g = gcd(q.z, n)
    if 1 < g < n:
        return 1, g
    c = [Point(0, 0)] * (m + 1)
    b = [Point(0, 0)] * (m + 1)
    c[1] = double(q, an, ad, n)
    c[2] = double(c[1], an, ad, n)
    for i in xrange(1, m + 1):
        if i > 2:
            c[i] = add(c[i - 1], c[1], c[i - 2], n)
        b[i] = (c[i].x * c[i].z) % n
    g = 1
    t = mul(b1 - 1 - 2 * m, q, an, ad, n)
    r = mul(b1 - 1, q, an, ad, n)
    for i in xrange(b1 - 1, b2, 2 * m):
        a = (r.x * r.z) % n
        for p in primes(i + 2, i + 2 * m, m):
            d = (p - i) // 2
            g = (g * (((r.x - c[d].x) * (r.z + c[d].z)) - a + b[d])) % n
        r, t = add(r, c[m], t, n), r
    g = gcd(g, n)
    if 1 < g < n:
        return 2, g
    return "factorization failed"


if __name__ == "__main__":
    print factor(828519338246984285695697941578258145583831516583983, \
            50000, 2500000, 210, 3902383298)  # first stage

    print factor(828519338246984285695697941578258145583831516583983, \
            25000, 2500000, 210, 3902383298)  # second stage
