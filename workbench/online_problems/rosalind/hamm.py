#!/usr/bin/env python3
# coding: utf-8

from itertools import izip

def hamming_dist(s1, s2):
    return sum(int(x != y) for (x, y) in izip(s1, s2))


if __name__ == "__main__":
    print hamming_dist("GAGCCTACTAACGGGAT", "CATCGTAATGACGGCCT")
