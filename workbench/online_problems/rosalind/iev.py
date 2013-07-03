#!/usr/bin/env python
# coding: utf-8

from itertools import starmap
from operator import mul


PROBS = [1.0, 1.0, 1.0, .75, .5, 0]


def iev(xs):
    return 2 * sum(starmap(mul, zip(PROBS, xs)))


if __name__ == "__main__":
    with open("data/rosalind_iev.txt") as f:
        xs = map(int, f.read().split())
        print(iev(xs))
