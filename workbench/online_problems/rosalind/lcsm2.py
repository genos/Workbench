#!/usr/bin/env python2
# coding: utf-8

from rosalind import fasta
# http://www.daimi.au.dk/~mailund/suffix_tree.html
from suffix_tree import GeneralisedSuffixTree


def lcsm(strings):
    stree = GeneralisedSuffixTree(strings)
    try:
        max_tuples = max(stree.sharedSubstrings(),
                key=lambda ss: ss[0][2] - ss[0][1])
        num, start, stop = max_tuples[0]
        return stree.sequences[num][start:stop]
    except TypeError:
        return None


if __name__ == "__main__":
    with open("data/rosalind_lcsm.txt") as f:
        print(lcsm(fasta(f.read()).values()))
