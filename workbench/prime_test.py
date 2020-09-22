#!/usr/bin/env python3

# Phil Bewig's sieve
def sieve(n):
    m = (n-1) // 2
    b = [True]*m
    i,p,ps = 0,3,[2]
    while p*p < n:
        if b[i]:
            ps.append(p)
            j = 2*i*i + 6*i + 3
            while j < m:
                b[j] = False
                j = j + 2*i + 3
        i+=1; p+=2
    while i < m:
        if b[i]:
            ps.append(p)
        i+=1; p+=2
    return ps


# revised version of Python Cookbook recipe
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


import itertools as it
def erat3( ):
    D = { 9: 3, 25: 5 }
    yield 2
    yield 3
    yield 5
    MASK= 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0,
    MODULOS= frozenset( (1, 7, 11, 13, 17, 19, 23, 29) )

    for q in it.compress(
            it.islice(it.count(7), 0, None, 2),
            it.cycle(MASK)):
        p = D.pop(q, None)
        if p is None:
            D[q*q] = q
            yield q
        else:
            x = q + 2*p
            while x in D or (x%30) not in MODULOS:
                x += 2*p
            D[x] = p

def new_prime(n):
    return takewhile(lambda p: p < n, erat3())

if __name__ == "__main__":
    from timeit import Timer
    t1 = Timer("sieve(int(1e6))", "from __main__ import sieve")
    t2 = Timer("list(primes(int(1e6)))", "from __main__ import primes")
    t3 = Timer("list(new_prime(int(1e6)))", "from __main__ import new_prime")
    n = 10
    print(t1.timeit(number=n) / n)
    print(t2.timeit(number=n) / n)
    print(t3.timeit(number=n) / n)
