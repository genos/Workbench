#!/usr/bin/env python
"""two_bad_sorts.py

My submission to http://programmingpraxis.com/2011/05/17/two-bad-sorts/

GRE, 5/17/11
"""

from random import shuffle


def stoogesort(seq, is_gt, a=0, b=-1):
    """A (bad) in-place sort"""
    b += len(seq) if b == -1 else 0
    if len(seq) <= 1 or a > b:
        return None
    if is_gt(seq[a], seq[b]):
        seq[a], seq[b] = seq[b], seq[a]
    if b - a > 1:
        t = (b - a + 1) / 3
        stoogesort(seq, is_gt, a, b - t)
        stoogesort(seq, is_gt, a + t, b)
        stoogesort(seq, is_gt, a, b - t)


def bogosort(seq, is_gt):
    """Use shuffle to try to sort in-place"""
    while any(is_gt(x, y) for (x, y) in zip(seq, seq[1:])):
        shuffle(seq)
    return None


if __name__ == "__main__":
    from operator import gt
    SEQ = range(5)
    shuffle(SEQ)
    print SEQ
    stoogesort(SEQ, gt)
    print SEQ
    shuffle(SEQ)
    print SEQ
    bogosort(SEQ, gt)
    print SEQ
