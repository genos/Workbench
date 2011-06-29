#!/usr/bin/env python

from __future__ import print_function
# If using Python2.x series, print is an expression (not a function)


def l1(n):
# Mapping print function over an xrange object
    map(print, xrange(1, n + 1))
    return None


def l2(n):
# Stolen from scheme solution; use and to shortcircuit evaluation
    n > 1 and l2(n - 1)
    print(n)
    return None


def l3(n):
# Similar to l2
    len(xrange(1, n)) > 0 and l3(n - 1)
    print(n)
    return None


def l4(n):
# Do exceptions count as conditionals?
    def aux(i):
        try:
            print(n - i + 1)
            x = 1.0 / (i - 1)
            aux(i - 1)
        except ZeroDivisionError:
            return None
    return aux(n)


def l5(n):
# Do asserts count as conditionals? Won't work if python is run with -O option
    def aux(i):
        try:
            assert i > 0
            print(n - i + 1)
            aux(i - 1)
        except AssertionError:
            return None
    return aux(n)
