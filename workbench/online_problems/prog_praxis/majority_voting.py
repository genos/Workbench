#!/usr/bin/env python

from collections import Counter


def majority(votes):
    C = Counter(votes)
    maj = sum(C.itervalues()) / 2.0
    pair = C.most_common(1)[0]
    return pair[0] if pair[1] > maj else None


if __name__ == "__main__":
    print majority(['a', 'b', 'a', 'b', 'a'])
    print majority(['a', 'a', 'a', 'c', 'c', 'b', 'b', 'c', 'c', 'c', 'b', 'c', 'c'])
    print majority(['a', 'b', 'c', 'a', 'b', 'a'])
