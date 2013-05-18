#!/usr/bin/env python3


def count(xs, n):
    cs = [0] * (n + 1)
    cs[0] = 1
    for x in xs:
        for i in range(x, n + 1):
            cs[i] += cs[i - x]
    return cs[n]


def addEach(x, xss):
    return [[x] + xs for xs in xss]


def coins(xs, n):
    cs = [[]] * (n + 1)
    cs[0] = [[]]
    for x in xs:
        for i in range(x, n + 1):
            cs[i] = cs[i] + addEach(x, cs[i - x])
    return cs[n]


if __name__ == "__main__":
    print(count([25, 10, 5, 1], 40))
    for c in coins([25, 10, 5, 1], 40):
        print(c)
