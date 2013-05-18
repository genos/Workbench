#!/usr/bin/env python3

from random import randrange, seed

UINT32 = 1 << 32


def djb_hash(string):
    h = 5381
    for s in string:
        h = ((h << 5) + h + ord(s)) % UINT32
    return h


class BloomFilter(object):
    def __init__(self, m, k):
        self.m = m
        self.k = k
        self.bits = [False] * m

    def add(self, s):
        seed(djb_hash(s))
        for i in range(self.k):
            self.bits[randrange(self.m)] = True

    def __contains__(self, s):
        seed(djb_hash(s))
        for i in range(self.k):
            if not self.bits[randrange(self.m)]:
                return False
        return True


if __name__ == "__main__":
    bf = BloomFilter(1024, 16)
    added_words = "gre grenos bloom_filter 1729".split()
    words = "gre GRE grenos gReNoS bloom_filter BLOOMFILTER 1729 42".split()
    for w in added_words:
        bf.add(w)
    for w in words:
        print("Is {0} in bf?\t{1}!".format(w, "Maybe" if w in bf else "No"))
