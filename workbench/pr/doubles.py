#!/usr/bin/env python3

import re
import string
import random
import myrustlib


def count_doubles(val):
    return sum(a == b for a, b in zip(val, val[1:]))


DOUBLE_RE = re.compile(r"""(?=(.)\1)""")


def count_doubles_rx(val):
    return len(DOUBLE_RE.findall(val))


def count_mult_doubles(vals):
    return sum(count_doubles(val) for val in vals)


def count_mult_doubles_rx(vals):
    return sum(count_doubles_rx(val) for val in vals)


def count_mult_doubles_py_rust(vals):
    return sum(myrustlib.count_doubles(val) for val in vals)


VAL = "".join(random.choice(string.ascii_letters) for _ in range(int(1e6)))

VALS = [VAL] * 100


def test_mult_pure_python(benchmark):
    benchmark(count_mult_doubles, VALS)


def test_mult_regex(benchmark):
    benchmark(count_mult_doubles_rx, VALS)


def test_mult_py_rust(benchmark):
    benchmark(count_mult_doubles_py_rust, VALS)


def test_mult_rust(benchmark):
    benchmark(myrustlib.count_mult_doubles, VALS)
