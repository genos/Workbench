#!/usr/bin/env python
"""upside_up.py

My submission to http://programmingpraxis.com/2011/05/27/upside-up/
A function to determine whether a number is upside up, finding the next, and
counting how many below a certain threshold.

GRE, 5/27/11
"""

from itertools import count, ifilter

UPSIDE_DICT = dict(zip("01689", "01986"))   # matches digit to rotated version


def is_upside_up(n):
    """Determines whether n reads the same after being rotated 180 degrees"""
    digit_pairs = zip(str(n), str(n)[::-1])
    return all(UPSIDE_DICT.get(x, False) == y for (x, y) in digit_pairs)


def next_upside_up(n):
    """Next upside up number after n"""
    return next(ifilter(is_upside_up, count(n + 1)))


if __name__ == "__main__":
    print next_upside_up(1961)
    print sum(1 for n in xrange(10000) if is_upside_up(n))
