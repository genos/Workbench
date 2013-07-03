#!/usr/bin/env python
# coding: utf-8


def mortal_fib(n, m):
    """Per someone else's solution (hangs head in shame), keep track of:
        (gestating, newborns, reproducing)
    """
    rabbits = [(0, 1, 0)]
    while len(rabbits) < n:
        gestating, newborn, reproducing = rabbits[-1]
        if len(rabbits) >= m:  # old rabbits die
            # kill those who were newly born m months ago
            reproducing -= rabbits[-m][1]
        reproducing += newborn
        newborn = gestating
        gestating = reproducing
        rabbits.append((gestating, newborn, reproducing))
    gestating, newborn, reproducing = rabbits[-1]
    return newborn + reproducing

if __name__ == "__main__":
    with open("data/rosalind_fibd.txt") as f:
        n, m = map(int, f.read().strip().split())
        print(mortal_fib(n, m))
