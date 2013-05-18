# elliptic curve factorization

from fractions import gcd
from math import log

def ilog(n, b): return int(log(n) / log(b))

def genPrimes():
    def isPrime(n):
        if n % 2 == 0: return n == 2
        d = 3
        while d * d <= n:
            if n % d == 0: return False
            d += 2
        return True
    def init():
        ps, qs, sieve = [], [], [True] * 50000
        p, m = 3, 0
        while p * p <= 100000:
            if isPrime(p):
                ps.insert(0, p)
                qs.insert(0, p + (p-1) / 2)
                m += 1
            p += 2
        for i in xrange(m):
            for j in xrange(qs[i], 50000, ps[i]):
                sieve[j] = False
        return m, ps, qs, sieve
    def advance(m, ps, qs, sieve, bottom):
        for i in xrange(50000): sieve[i] = True
        for i in xrange(m):
            qs[i] = (qs[i] - 50000) % ps[i]
        p = ps[0] + 2
        while p * p <= bottom + 100000:
            if isPrime(p):
                ps.insert(0, p)
                qs.insert(0, (p*p - bottom - 1)/2)
                m += 1
            p += 2
        for i in xrange(m):
            for j in xrange(qs[i], 50000, ps[i]):
                sieve[j] = False
        return m, ps, qs, sieve
    m, ps, qs, sieve = init()
    bottom, i = 0, 1
    yield 2
    while True:
        if i == 50000:
            bottom = bottom + 100000
            m, ps, qs, sieve = advance(m, ps, qs, sieve, bottom)
            i = 0
        elif sieve[i]:
            yield bottom + i + i + 1
            i += 1
        else: i += 1

def add(p, q, r, n): # r = p - q
    t1 = ((p[0] + p[1]) * (q[0] - q[1])) % n
    t2 = ((p[0] - p[1]) * (q[0] + q[1])) % n
    return (pow(t2+t1, 2, n) * r[1]) % n, \
           (pow(t2-t1, 2, n) * r[0]) % n

def double(p, an, ad, n):
    t1 = pow(p[0] + p[1], 2, n)
    t2 = pow(p[0] - p[1], 2, n)
    t3 = t1 - t2
    return (t1 * t2 * 4 * ad) % n, \
           (((t3 * an) + (t2 * ad * 4)) * t3) % n

def mul(k, p, an, ad, n):
    def bits(n):
        bs = []
        while n > 0:
            n, b = divmod(n, 2)
            bs.append(b)
        return list(reversed(bs))
    if k == 0: return 0, 0
    if k == 1: return p
    if k == 2: return double(p, an, ad, n)
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
    u = (s*s - 5) % n
    v = (4 * s) % n
    an = (pow(v-u, 3, n) * (3*u + v)) % n
    ad = (4 * pow(u, 3, n) * v) % n
    q = pow(u, 3, n), pow(v, 3, n)
    prime = iter(genPrimes())
    p = next(prime)
    while p < b1:
        q = mul(pow(p, ilog(b1,p), n), q, an, ad, n)
        p = next(prime)
    g = gcd(q[1], n)
    if 1 < g < n: return 1, g
    c = [(0,0)] * (m+1)
    b = [(0,0)] * (m+1)
    c[1] = double(q, an, ad, n)
    c[2] = double(c[1], an, ad, n)
    for i in xrange(1, m+1):
        if i > 2:
            c[i] = add(c[i-1], c[1], c[i-2], n)
        b[i] = (c[i][0] * c[i][1]) % n
    g = 1
    t = mul(b1 - 1 - 2*m, q, an, ad, n)
    r = mul(b1 - 1, q, an, ad, n)
    for i in xrange(b1-1, b2, 2*m):
        a = (r[0] * r[1]) % n
        while p <= i+2*m:
            d = (p - i) / 2
            g = g * ( ( (r[0] - c[d][0])
                      * (r[1] + c[d][1]) )
                    - a + b[d]) % n
            p = next(prime)
        r, t = add(r, c[m], t, n), r
    g = gcd(g, n)
    if 1 < g < n: return 2, g
    return "factorization failed"

# both return 1113491139767

print factor(828519338246984285695697941578258145583831516583983, \
             50000, 100000, 210, 3902383298) # first stage

print factor(828519338246984285695697941578258145583831516583983, \
             25000, 100000, 210, 3902383298) # second stage


