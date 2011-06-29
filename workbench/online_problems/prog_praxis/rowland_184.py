#!/usr/bin/env python2.6
"""
rowland.py
See http://programmingpraxis.com/2010/11/12/rowlands-prime-generating-function/

We make heavy use of memoization in order to trade space for time whenever we
define a recursive function or a function that is used by another.

GRE, 11/14/10
"""

import time
import functools

def gcd(a, b):
   while b:
       a, b = b, a % b
   return a

def memoize(func):
    func.memo = {}
    def memoizer(arg):
        try:
# Try using the memo dict, or else update it
            return func.memo[arg]
        except KeyError:
            func.memo[arg] = result = func(arg)
            return result
    return functools.update_wrapper(memoizer, func)

@memoize
def a106108(n):
    if n == 1:
        return 7
    else:
        return a106108(n-1) + gcd(n, a106108(n-1))

@memoize
def a132199(n):
    return a106108(n+1) - a106108(n)

def a137613(n):
    k, nc, a = 1, 1, 5
    while nc < n:
        k += 1
        a = a132199(k)
        if a != 1:
            nc += 1
    return a

def lpd(n):
    d = 3
    while n % d:
        d += 2
    return d

@memoize
def shortcut(n):
    if n <= 1:
        return 5
    else:
        return lpd(6 - n + sum([shortcut(k) for k in xrange(1, n)]))

def tester(a, n):
    print "%s:" % a.__name__
    tic = time.time()
    for k in xrange(1, n + 1):
        print a(k),
    toc = time.time() - tic
    print "\nIt took me %g seconds." % toc


if __name__ == '__main__':
    for (a, n) in zip([a106108, a132199, a137613, shortcut], [65, 104, 72, 72]):
        tester(a, n)
