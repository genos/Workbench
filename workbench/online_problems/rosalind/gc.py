#!/usr/bin/env python3
# coding: utf-8

from collections import Counter
from rosalind import fasta


def gc_content(s):
    c = Counter(s)
    total = sum(map(float, c.values()))
    return 100 * ((c['G'] + c['C']) / total)


def max_gc_content(d):
    keys_vals = map(lambda k: (k, gc_content(d[k])), d)
    (k, v) = max(keys_vals, key=lambda xy: xy[1])
    return "{0}\n{1}".format(k, v)


if __name__ == "__main__":
    filename = "data/rosalind_gc.txt"
    with open(filename) as f:
        print(max_gc_content(fasta(f)))
