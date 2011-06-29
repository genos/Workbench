#!/usr/bin/env python

from collections import defaultdict
from re import compile


def xref(file_name):
    exclude = compile(r"[\\\"'\(\)\[\]:?.,]")       # unwanted characters
    comment = compile(r"\#.*")                      # comments
    with open(file_name) as f:
        lines = f.readlines()
        d = defaultdict(list)
        for line_num in xrange(len(lines)):
            line = exclude.sub(' ', lines[line_num])
            line = comment.sub('', line).strip()
            for word in line.split():
                d[word].append(line_num + 1)
    return d


def output(x_d):
    for entry in sorted(str(k) + "\t" + ", ".join(str(i) for i in v) for (k, v)
            in x_d.iteritems()):
        print entry
    return None


if __name__ == "__main__":
    import sys
    output(xref(sys.argv[1] if sys.argv[1:] else "xref.py"))
