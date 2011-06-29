#!/usr/bin/env python


def obvious(k, xs):
    mins = []
    for i in xrange(len(xs) - k + 1):
        mins.append(min(xs[i:i + k]))
    return mins


def _init_queue(k, xs):
    q = []
    for i in xrange(k):
        m = min(xs[i:k])
        d = xs.index(m, i, k) + k
        if q == [] or q[-1] != (m, d):
            q.append((m, d))
    return q


def harter(k, xs):
    q = _init_queue(k, xs)
    mins = []
    for i in xrange(1, len(xs) - k + 2):
        mins.append(q[0][0])
        m = min(xs[i:i + k])
        d = xs.index(m, i, i + k) + k
        q = [p for p in q if p[0] <= m]
        if q != [] and q[0][1] > i + 1:
            q.pop(0)
        if q == [] or q[-1] != (m, d):
            q.append((m, d))
    return mins


### Added later: testing
from random import randrange


def comp_methods(k, xs):
    os = obvious(k, xs)
    hs = harter(k, xs)
    return all(map(lambda (o, h): o == h, zip(os, hs))) and len(os) == len(hs)


def n_tests(n):
    for _ in xrange(n):
        l = randrange(10, 1001)
        k = randrange(5, l // 2 + 1)
        xs = [randrange(10, 1001) for _ in xrange(l)]
        if not comp_methods(k, xs):
            print k
            print xs
            print obvious(k, xs)
            print harter(k, xs)
            return False
    return True


if __name__ == "__main__":
    xs = [4, 3, 2, 1, 5, 7, 6, 8, 9]
    print obvious(3, xs)
    print harter(3, xs)
    print n_tests(100)

"""
Congrats on two great years, and here's to many more!
[sourcecode lang="python"]
#!/usr/bin/env python


def obvious(k, xs):
    mins = []
    for i in xrange(len(xs) - k + 1):
        mins.append(min(xs[i:i + k]))
    return mins


def _init_queue(k, xs):
    q = []
    for i in xrange(k):
        m = min(xs[i:k])
        d = xs.index(m, i, k) + k
        if q == [] or q[-1] != (m, d):
            q.append((m, d))
    return q


def harter(k, xs):
    q = _init_queue(k, xs)
    mins = []
    for i in xrange(1, len(xs) - k + 2):
        mins.append(q[0][0])
        m = min(xs[i:i + k])
        d = xs.index(m, i, i + k) + k
        q = [p for p in q if p[0] <= m]
        if q != [] and q[0][1] > i + 1:
            q.pop(0)
        if q == [] or q[-1] != (m, d):
            q.append((m, d))
    return mins


### Added later: testing
from random import randrange


def comp_methods(k, xs):
    os = obvious(k, xs)
    hs = harter(k, xs)
    return all(map(lambda (o, h): o == h, zip(os, hs))) and len(os) == len(hs)


def n_tests(n):
    for _ in xrange(n):
        l = randrange(10, 1001)
        k = randrange(5, l // 2 + 1)
        xs = [randrange(10, 1001) for _ in xrange(l)]
        if not comp_methods(k, xs):
            print k
            print xs
            print obvious(k, xs)
            print harter(k, xs)
            return False
    return True


if __name__ == "__main__":
    xs = [4, 3, 2, 1, 5, 7, 6, 8, 9]
    print obvious(3, xs)
    print harter(3, xs)
    print n_tests(100)
[/sourcecode]
"""
