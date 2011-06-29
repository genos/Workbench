#!/usr/bin/env python
"""
partition_nums.py

My submission to Programming Praxis for
http://programmingpraxis.com/2011/04/15/partition-numbers/
Uses memoization to speed up recursion (memory--time trade off).

GRE, 4/15/11
"""

from functools import update_wrapper


def memoize(func):
    """
    Memoizes recursive computation so we can look up previously calculated
    results instead of recalculating every time
    """
    func.memo = {}

    def memoizer(arg):
        """
        Try using the memo dict, or else update it
        """
        if arg in func.memo:
            return func.memo[arg]
        else:
            func.memo[arg] = result = func(arg)
            return result

    return update_wrapper(memoizer, func)


@memoize
def partitions(n):
    """
    Integer partitions of n
    """
    if n in xrange(11):
        return (1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42)[n]
    else:
        return sum(pow(-1, k + 1) * (partitions(n - k * (3 * k - 1) // 2) +
            partitions(n - k * (3 * k + 1) // 2)) for k in xrange(n, 0, -1))


# Faster, based on formula from Mathworld:
@memoize
def sigma_1(n):
    """
    Sum of n's divisors
    """
    return sum(filter(lambda k: n % k == 0, xrange(1, n + 1)))


@memoize
def partitions2(n):
    """
    Integer partitions of n
    """
    if n in xrange(11):
        return (1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42)[n]
    else:
        return sum(sigma_1(n - k) * partitions2(k) for k in xrange(n)) // n


if __name__ == "__main__":
    from time import time
    start1 = time()
    print partitions(1000)
    print time() - start1
    start2 = time()
    print partitions2(1000)
    print time() - start2

