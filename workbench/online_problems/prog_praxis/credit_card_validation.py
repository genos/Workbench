#!/usr/bin/env python
"""
credit_card_validation.py

My 4/8/11 submission to Programming Praxis for
http://programmingpraxis.com/2011/04/08/credit-card-validation/
I came up with two versions of luhn_sum(), one that modifies a list in-place
and one that uses iterators.

GRE, 4/8/11
"""

from itertools import izip_longest, starmap


def luhn_sum_v1(num):
    """
    First version of luhn_sum; uses a list which it modifies in-place.
    """
    nums = [int(i) for i in reversed(str(num))]
    for i in xrange(1, len(nums), 2):
        nums[i] *= 2
    return sum(sum(divmod(i, 10)) for i in nums)


def luhn_sum_v2(num):
    """
    Second version of luhn_sum; uses iterators.
    """
    nums = [int(i) for i in reversed(str(num))]
    nums = ((x, 2 * y) for (x, y) in izip_longest(nums[::2], nums[1::2],
        fillvalue=0))
    return sum(starmap(lambda x, y: x + sum(divmod(y, 10)), nums))


def is_luhn(num, lsum=luhn_sum_v2):
    """
    Simple test to see whether a number is Luhn-valid; True iff luhn_sum = 0
    mod 10.
    """
    return lsum(num) % 10 == 0


def add_luhn_digit(num, lsum=luhn_sum_v2):
    """
    Returns a Luhn-valid number by adding the appropriate check digit to the
    end of num.
    """
    if is_luhn(10 * num):
        return 10 * num
    else:
        return 10 * num + (10 - (lsum(10 * num) % 10))


if __name__ == "__main__":
    print is_luhn(49927398716, lsum=luhn_sum_v1)
    print is_luhn(49927398716)
    print add_luhn_digit(4992739871)
