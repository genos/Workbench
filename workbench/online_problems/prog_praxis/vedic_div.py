#!/usr/bin/env python
# -*- coding: utf-8 -*-


def is_divisible(n, d):
    osc = 1 + (d * {1: 9, 3: 3, 7: 7, 9: 1}[d % 10]) // 10
    old, new = float('inf'), n
    while new < old:
        a, b = divmod(new, 10)
        old, new = new, a + b * osc
    return new % d == 0

if __name__ == "__main__":
    for (n, d) in [(13174584, 23), (175121, 37), (134567, 29)]:
        print "Is %d divisible by %d?%7s" % (n, d, "Yes." if is_divisible(n, d)
                                             else "No.")
