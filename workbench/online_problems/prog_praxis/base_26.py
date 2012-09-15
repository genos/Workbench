#!/usr/bin/env python

from itertools import count, imap, izip, repeat, starmap
from operator import mul
from string import ascii_uppercase

def powers(n):
    return imap(pow, repeat(n), count())

def digits(n):
    return imap(int, str(n))

def b26_to_int(s):
    return sum(starmap(mul, izip(powers(26), reversed([ascii_uppercase.index(k)
                                                       for k in s]))))

def int_to_b26(n):
    if n < 26:
        return ascii_uppercase[n]
    else:
        q, r = divmod(n, 26)
        return int_to_b26(q) + ascii_uppercase[r]

def b26_mul(s, t):
    return int_to_b26(b26_to_int(s) * b26_to_int(t))

if __name__ == "__main__":
    print b26_mul("CSGHJ", "CBA")
