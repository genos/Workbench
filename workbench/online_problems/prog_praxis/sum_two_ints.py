#!/usr/bin/env python
# -*- coding: utf-8 -*-


def quad(xs, t):
    l = len(xs)
    for i in xrange(l):
        for j in xrange(i + 1, l):
            if xs[i] + xs[j] == t:
                return (xs[i], xs[j])
    return None


def nlogn(xs, t):
    ys = sorted(xs)
    i, j = 0, len(ys) - 1
    while i != j:
        s = ys[i] + ys[j]
        if s == t:
            return (ys[i], ys[j])
        elif s < t:
            i += 1
        else:
            j -= 1
    return None


def linear(xs, t):
    d = set()
    for x in xs:
        if t - x in d:
            return (t - x, x)
        else:
            d.add(x)
    return None
