#!/usr/bin/env python

# Mostly stolen from dannyturner.net
import fractions

def e_rep_gen(limit):
    c = 0
    for i in xrange(limit):
        if ((i + 2) % 3 == 0):
            c += 2
            yield c
        else:
            yield 1

ecr = [x for x in e_rep_gen(99)][::-1]
f = fractions.Fraction(0, 1)
for n in ecr:
    f = 1/(n + f)

f += fractions.Fraction(2,1)
print sum([int(x) for x in str(f.numerator)])
