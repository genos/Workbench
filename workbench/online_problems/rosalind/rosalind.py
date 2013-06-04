#!/usr/bin/env python3
# coding: utf-8

import functools
import operator


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


def fasta(string):
    """>DATA_1
    ABCDEFG
    HIJKLMNOP
    >DATA_2
    1234567890

    --> {'DATA_1': 'ABCDEFGHIJKLMNOP',
         'DATA_2': '1234567890'}
    """
    strings = filter(None, string.strip().split('>'))
    ret = {}
    for s in strings:
        header, body = s.split('\n', 1)
        body = body.replace('\n', '')
        ret[header] = body
    return ret


def binom(n, k):
    factorial = lambda n: functools.reduce(operator.mul, range(1, n + 1), 1)
    return factorial(n) / (factorial(k) * factorial(n - k))


def group(xs, n):
    """group xs into groups of n"""
    ys = iter(xs)
    while True:
        yield (next(ys), next(ys), next(ys))
