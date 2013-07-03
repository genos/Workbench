#!/usr/bin/env python
# coding: utf-8

from rosalind import binom


def pbinom(n, p, r):
    return binom(n, r) * pow(p, r) * pow(1 - p, n - r)


def dbinom(n, p, r):
    return sum(pbinom(n, p, i) for i in range(r + 1))


def lia(k, N):
    t = 1 << k
    return 1 - dbinom(t, .25, N - 1)


if __name__ == "__main__":
    with open("data/rosalind_lia.txt") as f:
        k, N = map(int, f.read().split())
        print(lia(k, N))
