#!/usr/bin/env python
"""
Good ol' Sieve.

GRE, 6/28/10
"""


def _starter_helper(x):
    """
    Initialization condition, to deal with Optimization 1.
    """
# Special cases; handles evens, 0, and 1 separately
    if (x == 2): return True
    elif (x == 0) or (x == 1): return False
    elif (x % 2 == 0): return False
    else: return True


def _sieve_helper(n):
    """
    Creates a list result of Trues and Falses, determined by whether result[i]
    is prime or not.

    Uses:   _starter_helper(x)
    """
# Optimization 1:
# Only odd numbers considered; 0, 1, 2, and evens handled separately.
    result = [_starter_helper(x) for x in range(n + 1)]
# Optimization 3:
# Only check up to square root of n.
    for x in range(3, int(n**0.5) + 1, 2):
        if result[x] == True:
            counter = pow(x, 2)
# Optimization 2:
# Start at x^2, because smaller multiples of x have already been checked.
            while counter < (n + 1):
                result[counter] = False
                counter += x
    return result


def sieve(n):
    """
    Main function: returns a list of primes <= n.

    Uses:   _sieve_helper(n)
    """
    tf_list = _sieve_helper(n)
    return [x for x in range(n+1) if tf_list[x]]


if __name__ == "__main__":
    n = int(raw_input("What number for n?  "))
    print(sieve(n))
