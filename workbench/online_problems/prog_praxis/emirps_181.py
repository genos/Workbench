#!/usr/bin/env python2.6

import math

def reverse(n):
    """
    Given integer n, returns n written backwards.
    Note: uses only numerical operations (not string ones) for speed.
    """
    d, r = 0, 0
    while n > 0:
        l = int(math.log10(n))
        r += (10 ** d) * (n // (10 ** (l)))
        d += 1
        n %= (10**l)
    return r

def is_prime(n):
    """
    Simple check for primality, testing n mod k for odd k up to 1 + sqrt(n).
    """
    if n == 2:
        return True
    elif n % 2 == 0:
        return False
    else:
        for k in xrange(3, 1 + int(math.sqrt(n)), 2):
            if n % k == 0:
                return False
        return True

def main(n):
    """
    Prints out all emirps less than n.
    """
    for p in xrange(13, n, 2):
        r = reverse(p)
        if (is_prime(p)) and (r != p) and (is_prime(r)):
            print p

if __name__ == '__main__':
    import sys
    main(int(sys.argv[1]))
