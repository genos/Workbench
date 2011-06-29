#!/usr/bin/env python


def rev_string(s):
    return s[::-1]


def fib(n):
    a, b = 0, 1
    for _ in xrange(n):
        a, b = b, a + b
    return a


def mult_table_12():
    print '\n'.join(''.join("{:^6d}".format(a * b) for a in xrange(1, 13))
                    for b in xrange(1, 13))
    return None


def file_sum(file_name):
    with open(file_name) as f:
        return sum(int(n) for n in f.readlines())


def odds_1_to_99():
    print range(1, 100, 2)
    return None


def max_int(seq):
    assert all(isinstance(x, int) for x in seq), "Not a sequence of ints!"
    # Because return max(seq) is cheating :-)
    return reduce(lambda a, b: a if a > b else b, seq, 0)


def rgb_to_hex(r, g, b):
    return ''.join(h[2:] if len(h) == 4 else '0' + h[2:] for h in
                   map(hex, (r, g, b)))
