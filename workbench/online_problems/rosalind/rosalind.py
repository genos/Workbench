#!/usr/bin/env python3
# coding: utf-8

import functools


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


def fasta(open_file):
    """>DATA_1
    ABCDEFG
    HIJKLMNOP
    >DATA_2
    1234567890

    --> {'DATA_1': 'ABCDEFGHIJKLMNOP',
         'DATA_2': '1234567890'}
    """
    strings = filter(None, open_file.read().strip().split('>'))
    ret = {}
    for s in strings:
        header, body = s.split('\n', 1)
        body = body.replace('\n', '')
        ret[header] = body
    return ret
