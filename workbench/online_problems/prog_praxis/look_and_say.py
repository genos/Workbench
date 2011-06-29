#!/usr/bin/env python

from itertools import groupby


def next_iter(s):
    return ''.join(str(len(list(g))) + k for k, g in groupby(s))


if __name__ == "__main__":
    s = '1'
    for _ in xrange(10):
        print s
        s = next_iter(s)

"""
Using the <code>groupby()</code> function from Python's itertools module:
[sourcecode lang="python"]
from itertools import groupby

def next_iter(s):
    return ''.join([str(len(list(g))) + k for k, g in groupby(s)])

if __name__ == "__main__":
    s = '1'
    for _ in xrange(10):
        print s
        s = next_iter(s)
[/sourcecode]
"""
