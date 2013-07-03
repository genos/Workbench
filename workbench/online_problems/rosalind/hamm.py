#!/usr/bin/env python
# coding: utf-8


def hamming_dist(s1, s2):
    return sum(int(x != y) for (x, y) in zip(s1, s2))


if __name__ == "__main__":
    with open("data/rosalind_hamm.txt") as f:
        s1, s2 = f.read().strip().split('\n')
        print(hamming_dist(s1, s2))
