#!/usr/bin/env python
# coding: utf-8


from itertools import product
from rosalind import group


RNA_CODONS = (''.join(p) for p in product('UCAG', repeat=3))
AMINO_ACIDS = ''.join(["FFLLSSSSYYxxCCxW",
                       "LLLLPPPPHHQQRRRR",
                       "IIIMTTTTNNKKSSRR",
                       "VVVVAAAADDEEGGGG"])
TABLE = dict(zip(RNA_CODONS, AMINO_ACIDS))


def prot(dataset):
    acids = (''.join(TABLE[''.join(triple)]) for triple in group(dataset, 3))
    return ''.join(acids)


if __name__ == "__main__":
    with open("data/rosalind_prot.txt") as f:
        print(prot(f.read().strip())[:-1])
