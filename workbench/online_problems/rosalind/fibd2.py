#!/usr/bin/env python
# coding: utf-8


def mortal_fib(n, m):
    """rabbbits[0] = newborn, rabbits[-1] = almost dead"""
    rabbits = [0] * m
    rabbits[0] = 1
    for i in range(n - 1):
        tmp, total = rabbits[0], 0
        for j in range(1, m):
            total += rabbits[j]
            rabbits[j], tmp = tmp, rabbits[j]
        rabbits[0] = total
    return sum(rabbits)


if __name__ == "__main__":
    with open("data/rosalind_fibd.txt") as f:
        n, m = map(int, f.read().strip().split())
        print(mortal_fib(n, m))
