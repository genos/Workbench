#!/usr/bin/env python
"""miscellanea.py

My submission to http://programmingpraxis.com/2011/04/26/miscellanea/
GRE, 4/26/11
"""
from itertools import count


# 1: FizzBuzz, two versions
def fizz_buzz_standard(n):
    """
    The usual FizzBuzz answer, chained elifs
    """
    for i in xrange(1, n + 1):
        if i % 15 == 0:
            i = "FizzBuzz"
        elif i % 3 == 0:
            i = "Fizz"
        elif i % 5 == 0:
            i = "Buzz"
        print i
    return None


def fb_gen():
    """
    Generator for FizzBuzz output, idea from Dan Harasty's solution
    """
    for n in count(1):
        out = ""
        for word, mod in (("Fizz", 3), ("Buzz", 5)):
            out += word if n % mod == 0 else ""
        yield (out if out else n)


def fizz_buzz_gen(n):
    """
    Uses fb_gen for FizzBuzz, idea from Dan Harasty's solution
    """
    fb = fb_gen()
    for i in xrange(1, n + 1):
        print next(fb)
    return None


# 2: Prime Words, two versions
def is_prime(n):
    """
    Simple primality check via trial division
    """
    return n > 1 and all(n % k for k in xrange(2, int(pow(n, 0.5)) + 1))


def is_prime_word_horner(word):
    """
    Horner's Method to send the word to base 36, simple primality check
    """
    return is_prime(reduce(lambda x, y: 36 * x + ord(y) - 55, word, 0))


def is_prime_word_builtin(word):
    """
    It's more Pythonic to use the builtin functionality of int(), I suppose...
    """
    return is_prime(int(word, 36))


# 3: Split A List, two versions
def split_imperative(lst):
    """
    Imperatively splits the lst into two equal (unless len(lst) is odd) halves
    with a single scan of lst
    """
    l = [lst[0]]
    r = []
    i = 1
    j = 0
    for item in lst[1:]:
        if i < j + 1:
            l.append(r[0])
            r = r[1:] + [item]
            i += 1
        else:
            r.append(item)
            j += 1
    return (l, r)


def split_recursive(lst):
    """
    Recursively splits the lst into two equal (unless len(lst) is odd) halves
    with a single scan of lst, driver function for aux
    """

    def aux(lst, l, r, i, j):
        """
        Main recursive function for split_recursive
        """
        if not lst:
            return (l, r)
        elif i < j + 1:
            return aux(lst[1:], l + [r[0]], r[1:] + [lst[0]], i + 1, j)
        else:
            return aux(lst[1:], l, r + [lst[0]], i, j + 1)

    return aux(lst[1:], [lst[0]], [], 1, 0)
