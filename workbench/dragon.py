#!/usr/bin/env python

from pylab import scatter, show


f = lambda z: (1 + 1j) * z / 2
g = lambda z: 1 - (1 - 1j) * z / 2


def heighway(n):
    s = set([0, 1])
    for _ in xrange(n):
        s = set(map(f, s)) | set(map(g, s))
    return s


def plot(n):
    h = heighway(n)
    x, y = [], []
    for z in h:
        x.append(z.real), y.append(z.imag)
    scatter(x, y)
    show()

if __name__ == "__main__":
    from sys import argv
    plot(int(argv[1]) if len(argv) > 1 else 10)
