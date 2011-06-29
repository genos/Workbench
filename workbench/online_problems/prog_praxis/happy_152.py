#!/usr/bin/env python
"""
Determines all the happy numbers less than upper bound of 100.

GRE, 7/24/10
"""

def is_happy(n):
    """
    Determines whether the number n is a "happy number".
    Int -> Bool

    Notes:
    Uses str() to split digits (slower, less efficient)
    Uses a set to contain the sequence generated from n
    """
    n_sequence = set()
    while n != 1:
        s = str(n)
        n = sum(pow(int(x),2) for x in s)
        if n in n_sequence: return False
        n_sequence.add(n)
    return True

def is_happy_v2(n):
    """
    Determines whether the number n is a "happy number".
    Int -> Bool

    Notes:
    Differs from first version in two ways:
        1. Uses only // and % to split digits (more efficient)
        2. Uses a dictionary (hash) to store the sequence
    Uses _digits() to get digits of n (faster than str() for large n)
    """
    n_sequence = {n : 1}
    while n != 1:
        n = sum(pow(x,2) for x in _digits(n))
        if n in n_sequence: return False
        n_sequence[n] = 1
    return True


def _digits(n):
    """
    Outputs a list of the digits of the input integer.
    Int -> [Int]

    Note:
    Returns list in reverse order from what one might expect. It doesn't matter
    for our purposes since we're summing the squares of the digits anyway, but
    this can be fixed by returning res[::-1] if it bothers you.
    """
    res = []
    while n != 0:
        res.append(n % 10)
        n //= 10
    return res


def main(upper_limit, version = 1):
    """
    The main event; for all integers x in {1, 2, ..., upper_limit}, prints x if
    x is happy.

    (Int{, optional Int}) -> None, with printing side-effects

    Notes:
    Also takes in an optional integer "version" to determine which test to use
    for happiness.
    """
    happy_test = [is_happy, is_happy_v2][version]
    happy_list = [x for x in range(1, upper_limit) if is_happy(x)]
    for h in happy_list: print h
    return None


if __name__ == "__main__":
    main(100)
