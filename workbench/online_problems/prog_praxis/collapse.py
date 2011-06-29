#!/usr/bin/env python

def collapse(x):
    if not isinstance(x, list):
        return [x]
    elif len(x) == 0:
        return x
    else:
        return collapse(x[0]) + collapse(x[1:])
