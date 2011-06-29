#!/usr/bin/env python

############################
#     Behind the Scenes    #
############################

from random import randrange


def split(n):
    s = 0
    while (n > 0) and (n % 2 == 0):
        s += 1
        n >>= 1
    return (s, n)


def P(a, r, s, n):
    if pow(a, r, n) == 1:
        return True
    elif (n - 1) in [pow(a, r * (2 ** j), n) for j in range(s)]:
        return True
    else:
        return False


def miller_rabin(n, t):
    (s, r) = split(n - 1)
    for i in range(t):
        a = randrange(2, n)
        if not P(a, r, s, n):
            return False
    return True


def is_prime(n):
    return miller_rabin(n, 50)


# from Python Cookbook
from itertools import count, islice


def erat2():
    D = {}
    yield 2
    for q in islice(count(3), 0, None, 2):
        p = D.pop(q, None)
        if p is None:
            D[q * q] = q
            yield q
        else:
            x = p + q
            while x in D or not (x & 1):
                x += p
            D[x] = p


# leads to primes < n:
def primes(n):
    e = erat2()
    ps = []
    x = next(e)
    while x <= n:
        ps.append(x)
        x = next(e)
    return ps


############################
#    Start of Excercise    #
############################

def times(x, n):
    t = 0
    while not n % x:
        t += 1
        n //= x
    return t


from itertools import chain


def prime_factors(n):
    pfs = ()
    for p in primes(n):
        if not n % p:
            pfs = chain(pfs, [p for i in xrange(times(p, n))])
    return pfs


def home_prime(n):
    while not is_prime(n):
        n = int(''.join(str(p) for p in prime_factors(n)))
    return n


def lpf(n):
    return next(prime_factors(n))


def euclid_mullin():
    a = 2
    p = 1
    while True:
        yield a
        p *= a
        a = lpf(1 + p)


r"""
My solution isn't very long, but the machinery behind the scenes is rather
extensive. In an effort to save memory, I've tried to make heavy use of
iterators---sort of like lazy evaluation of lists in Haskell.  Below is what I
wrote for this exercise; I also used an implementation of the Sieve of
Eratosthenes for primes and a Miller-Rabin primality test.
The full code is available <a href="http://codepad.org/b5oGN9hi">here</a>.
[sourcecode lang="python"]
def times(x, n):
    t = 0
    while not n % x:
        t += 1
        n //= x
    return t


from itertools import chain


def prime_factors(n):
    pfs = ()
    for p in primes(n):
        if not n % p:
            pfs = chain(pfs, [p for i in xrange(times(p, n))])
    return pfs


def home_prime(n):
    while not is_prime(n):
        n = int(''.join(str(p) for p in prime_factors(n)))
    return n


def lpf(n):
    return next(prime_factors(n))


def euclid_mullin():
    a = 2
    p = 1
    while True:
        yield a
        p *= a
        a = lpf(1 + p)
[/sourcecode]
"""
