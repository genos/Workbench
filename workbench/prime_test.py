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


if __name__ == "__main__":
    from timeit import Timer
    t1 = Timer("sieve(1000)", "from __main__ import sieve")
    t2 = Timer("list(primes(1000))", "from __main__ import primes")
    n = 1000
    print t1.timeit(number=n) / n
    print t2.timeit(number=n) / n
