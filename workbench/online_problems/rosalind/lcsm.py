#!/usr/bin/env python2
# coding: utf-8

# my tweaks to http://www.daimi.au.dk/~mailund/suffix_tree.html
from suffix_tree import GeneralisedSuffixTree
from rosalind import fasta


def lcsm(strings):
    gst = GeneralisedSuffixTree(strings)
    max_tuples = max(gst.sharedSubstrings(),
            key=lambda xs: xs[0][2] - xs[0][1])
    index, start, stop = max_tuples[0]
    return strings[index][start:stop]



if __name__ == "__main__":
    with open("data/rosalind_lcsm.txt") as f:
        print(lcsm(list(fasta(f.read()).values())))
