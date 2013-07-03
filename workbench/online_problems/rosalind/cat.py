#!/usr/bin/env python
# coding: utf-8

from collections import Counter


def catalan(n):
    if n == 0:
        return 0
    else:
        c = 1
        for i in range(n):
            c *= (2 * (2 * i + 1))
            c //= (i + 2)
        return c


def cat(string):
    c = Counter(string)
    if c['A'] != c['U'] or c['C'] != c['G']:
        raise ValueError("Occurrences don't match")
    else:
        return catalan(c['A']) + catalan(c['C'])


if __name__ == "__main__":
    with open("data/rosalind_cat.txt") as f:
        print(cat(f.read().split("\n")[1]))
