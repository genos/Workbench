#!/usr/bin/env python

from itertools import permutations


def tails(xs):
    return (xs[i:] for i in xrange(len(xs)))


def chain(word):
    r = range(len(word))
    return set(tuple(''.join(word[i] for i in set(r).intersection(ix))
        for ix in t) for t in map(tails, permutations(r)))


def chop(word, dict_set):
    return filter(lambda cs: all(c in dict_set for c in cs), chain(word))


if __name__ == "__main__":
    from pprint import pprint
    with open("/usr/share/dict/words") as f:
        dict_set = set(line.strip() for line in f)
        pprint(chop("planet", dict_set))
