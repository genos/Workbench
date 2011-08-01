from itertools import count, takewhile

def erat2():
    D = {}
    yield 2
    for q in count(3, 2):
        p = D.pop(q, None)
        if p is None:
            D[q * q] = q
            yield q
        else:
            x = p + q
            while x in D or not (x & 1):
                x += p
            D[x] = p


def primes(n):
    return takewhile(lambda p: p < n, erat2())

#################
from math import log
from operator import mul

GAMMA = 0.57721566490153286061

def li(x):
    return GAMMA + log(log(x)) + sum(pow(log(x), n) /
                                     (reduce(mul, xrange(1, n + 1)) * n) for
                                      n in xrange(1, 101))

def factors(n):
    return (p for p in primes(n + 1) if n % p == 0)

def mu(n):
    m = lambda p: -1 if (n / p) % p else 0
    return reduce(mul, (m(p) for p in factors(n)), 1)

def r(x):
    return sum(mu(n) / n * li(pow(x, 1.0 / n)) for n in xrange(1, 101))

if __name__ == "__main__":
    print [mu(x) for x in xrange(1, 11)]
