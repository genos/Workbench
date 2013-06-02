#!/usr/bin/env python3
# coding: utf-8

from rosalind import binom


def iprb(k, m, n):
    """Given
    k:      # homozygous dominant
    m:      # heterozygous
    n:      # homozygous recessive
    return P(offspring of 2 randomly chosen organisms posseses dominant allele)
    = 1 - P(recessive)
    """
    s = k + m + n
    total_choices = binom(s, 2)
    first = lambda x: x / s
    second = lambda x: x / (s - 1)
    prob_pair = {
        'DD': first(k) * second(k - 1),
        'DH': first(k) * second(m),
        'DR': first(k) * second(n),
        'HD': first(m) * second(k),
        'HH': first(m) * second(m - 1),
        'HR': first(m) * second(n),
        'RD': first(n) * second(k),
        'RH': first(n) * second(m),
        'RR': first(n) * second(n - 1)
    }
    prob_recessive = {
        'DD': 0,    'DH': 0,    'DR': 0,
        'HD': 0,    'HH': .25,  'HR': .5,
        'RD': 0,    'RH': .5,   'RR': 1
    }
    return 1 - sum(prob_pair[p] * prob_recessive[p] for p in prob_pair)



if __name__ == "__main__":
    with open("data/rosalind_iprb.txt") as f:
        k, m, n = map(int, f.readline().split(' '))
        print(iprb(k, m, n))
