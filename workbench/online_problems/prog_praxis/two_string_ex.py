#!/usr/bin/env python

# 1st exercise
def rem_dup_char(str):
    cs = set()
    out = []
    for c in str:
        if c not in cs:
            cs.add(c)
            out.append(c)
    return ''.join(out)

# 2nd exercise
def squash_space(str):
    return ' '.join(str.split())
