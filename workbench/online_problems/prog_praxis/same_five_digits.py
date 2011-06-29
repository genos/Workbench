#!/usr/bin/env python

from collections import Counter
from itertools import combinations


def histogram(triple):
    return Counter(int(d) for d in ''.join(str(x) for x in triple))


def meets_criteria(triple):
    hist = histogram(triple)
    digits, counts = set(hist.keys()), set(hist.values())
    return digits == counts and not any(hist[k] == k for k in hist)


def unique_single(triples):
    digits = {i: [] for i in xrange(1, 6)}
    for t in triples:
        hist = histogram(triple)
        digits[min(hist.iterkeys(), key=lambda k: hist[k])].append(t)
    return filter(lambda v: len(v) == 1, digits.itervalues())[0][0]


def main():
    nums = (n**2 for n in xrange(100, 236) if set(str(n**2)).issubset('12345'))
    return unique_single(t for t in combinations(nums, 3) if meets_criteria(t))


if __name__ == "__main__":
    print main()
