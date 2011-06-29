#!/usr/bin/env python

import functools
from fractions import Fraction


def b_list1(n):
    bs, ns, ds, d = [Fraction(-1, 2)], [], [], 1
    for k in xrange(1, n):
        x, y, z = 2 * k, 2 * k - 1, 2 * k + 1
        ns = [x] + [j * x * y for j in ns]
        b = -sum(p * q * r  for (p, q, r) in
                zip(bs, [y] + ns[:-1], [1 / j for j in ([Fraction(z)] + ds)]))
        bs.append(b)
        d *= Fraction(x * y, 1)
        ds.append(d)
    return bs


def memoize(func):
    func.memo = {}
    def memoizer(*args):
        try:
# Try using the memo dict, or else update it
            return func.memo[args]
        except KeyError:
            func.memo[args] = result = func(*args)
            return result
    return functools.update_wrapper(memoizer, func)


def c(m, k):
    n = d = 1
    for j in xrange(1, m - k + 1):
        n *= k + j
        d *= j
    return n / d


@memoize
def b_rec(n):
    if n == 0:
        return Fraction(1)
    else:
        return -sum(c(n + 1, j) * b_rec(j) for j in xrange(n)) / (n + 1)


def b_list2(n):
    return filter(lambda x: x != 0, (b_rec(k) for k in xrange(2 * n)))


if __name__ == "__main__":
    for b in b_list1(10):
        print b

    print "-" * 17
    for b in b_list2(10):
        print b


r"""My Python solution.
The first version is a translation of the provided solution into scheme. The
second is a memoized version of the recursive relation that for B_n that can be
found in a bunch of places (I got mine from p. 180 of the "CRC Handbook of
Discrete and Combinatorial Mathematics"). I had to throw in the filter statement
due to differing ideas about the proper enumeration of the Bernoulli numbers.
Note: requires the <code>fractions</code> module for rational numbers.
[sourcecode lang="python"]
#!/usr/bin/env python

import functools
from fractions import Fraction


def b_list1(n):
    bs, ns, ds, d = [Fraction(-1, 2)], [], [], 1
    for k in xrange(1, n):
        x, y, z = 2 * k, 2 * k - 1, 2 * k + 1
        ns = [x] + [j * x * y for j in ns]
        b = -sum(p * q * r  for (p, q, r) in
                zip(bs, [y] + ns[:-1], [1 / j for j in ([Fraction(z)] + ds)]))
        bs.append(b)
        d *= Fraction(x * y, 1)
        ds.append(d)
    return bs


def memoize(func):
    func.memo = {}
    def memoizer(*args):
        try:
# Try using the memo dict, or else update it
            return func.memo[args]
        except KeyError:
            func.memo[args] = result = func(*args)
            return result
    return functools.update_wrapper(memoizer, func)


def c(m, k):
    n = d = 1
    for j in xrange(1, m - k + 1):
        n *= k + j
        d *= j
    return n / d


@memoize
def b_rec(n):
    if n == 0:
        return Fraction(1)
    else:
        return -sum(c(n + 1, j) * b_rec(j) for j in xrange(n)) / (n + 1)


def b_list2(n):
    return filter(lambda x: x != 0, (b_rec(k) for k in xrange(2 * n)))


if __name__ == "__main__":
    for b in b_list1(10):
        print b

    print "-" * 17
    for b in b_list2(10):
        print b
[/sourcecode]
"""
