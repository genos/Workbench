#!/usr/bin/env python
# coding: utf-8

from rosalind import memoize


@memoize
def fib_memo(nk):
    n, k = nk
    if n < 3:
        return 1
    else:
        n1k = (n - 1, k)
        n2k = (n - 2, k)
        return fib_memo(n1k) + k * fib_memo(n2k)


if __name__ == "__main__":
    with open("data/rosalind_fib.txt") as f:
        nk = tuple(map(int, f.read().strip().split()))
        print(fib_memo(nk))
