#!/usr/bin/env python


def _drop_mults(xs):
    for i in xrange(0, len(xs), xs[0]):
        xs[i] = 0
    return filter(None, xs)


def euler(n):
    xs = range(3, n, 2)
    ps = [2]
    while xs:
        p = xs[0]
        ps.append(p)
        xs = _drop_mults(xs)
    return ps


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


# Make a list using erat2()
def erat(n):
    ps = []
    e = erat2()
    p = e.next()
    while p <= n:
        ps.append(p)
        p = e.next()
    return ps


if __name__ == "__main__":
    import time

    def time_method(method, n):
        start = time.time()
        ps = method(n)
        stop = time.time()
        return "%e" % (stop - start)

    for method in [euler, erat]:
        print method.func_name
        print time_method(method, int(1e5))
        print "\n"
