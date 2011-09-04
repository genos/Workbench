# Mike's answer
from collections import Counter, OrderedDict

class OrderedCounter(Counter, OrderedDict):
    pass

def unique(s):
    return (k for k,c in OrderedCoutner(s).items() if c == 1)

def first_unique(s):
    return next(unique(s), '')
