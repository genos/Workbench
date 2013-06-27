#!/usr/bin/env python
# coding: utf-8

import sys
from collections import Counter
from rosalind import fasta


ACIDS = "ACGT"


def consensus(strings):
    cons, matrix = [], dict((x, []) for x in ACIDS)
    for c in map(Counter, zip(*strings)):
        cons.append(c.most_common(1)[0][0])
        for x in ACIDS:
            matrix[x].append(c.get(x, 0))
    return cons, matrix

if __name__ == "__main__":
    with open("data/rosalind_cons.txt") as f:
        cons, matrix = consensus(fasta(f.read()).values())
        print(''.join(cons))
        for x in ACIDS:
            print("{0}: {1}".format(x, ' '.join(map(str, matrix[x]))))
