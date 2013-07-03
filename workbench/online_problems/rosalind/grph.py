#!/usr/bin/env python
# coding: utf-8

from itertools import permutations
from networkx import DiGraph
from rosalind import fasta


def is_edge(s, t, k):
    """There is an edge from s to t if there is a length k suffix of s that
    matches a length k prefix as t (and s != t).
    """
    if s == t:
        return False
    else:
        return (s[1][-k:] == t[1][:k])


def build_digraph(fasta_dict, k):
    """Build a digraph from the data given by the fasta dict, using k and our
    is_edge predicate.
    """
    d = DiGraph()
    d.add_nodes_from(fasta_dict.items())
    for (s, t) in permutations(fasta_dict.items(), 2):
        if is_edge(s, t, k):
            d.add_edge(s, t)
    return d


def adjacency_list(fasta_dict, k):
    """Formatted adjacency list from the digraph for fasta_dict, k."""
    adj_list = []
    d = build_digraph(fasta_dict, k)
    for e in d.edges_iter():
        adj_list.append("{0} {1}".format(e[0][0], e[1][0]))
    return adj_list


if __name__ == "__main__":
    with open("data/rosalind_grph.txt") as f:
        for line in adjacency_list(fasta(f.read()), 3):
            print(line)
