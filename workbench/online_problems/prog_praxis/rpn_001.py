#!/usr/bin/env python
"""
Clean python Reverse Polish Notation calculator, with help from someone else's
version on website.

GRE, 6/28/10
"""

import operator as op

ops = {'+': op.add, '-': op.sub, '*': op.mul, '/': op.div}

def rpn(prompt):
    stack = []
    for x in raw_input(prompt).split():
        if x in ops:
            z = stack.pop()
            y = stack.pop()
# Little tricky: order matters for noncommutative - and /
            stack.append(ops[x](y, z))
        else:
            stack.append(float(x))
    return stack.pop()

if __name__ == "__main__":
    print(rpn("> "))
