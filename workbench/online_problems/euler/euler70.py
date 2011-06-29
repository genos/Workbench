#!/usr/bin/env python

# euler70.py

from __future__ import division
import subprocess

def prime_list_faster(m, n):
    """
    Returns a list of primes in [m, n].
    Requires the outside program primes from http://cr.yp.to/primegen.html, so
    this is really just an exercise in using the subprocess module.
    """
    primes_output = subprocess.Popen(['primes', '%s'%m, '%s'%n], shell=False,
                                      stdout=subprocess.PIPE).communicate()[0]
    return [int(p) for p in primes_output.split()]

def totient(p, q): 
    if (q == 1):
        return p - 1
    elif (p == 1):
        return q - 1
    else:
        return (p - 1) * (q - 1)

def is_perm(a, b):
    return sorted(str(a)) == sorted(str(b))

def main():
    p_list = prime_list_faster(1e3, 5e4)
    min_n, min_phi, min_ratio = 6, 2, 3
    for p in p_list:
        for q in p_list:
            n = p*q
            if n < 1e7:
                phi = totient(p, q)
                ratio = n/phi
                if (is_perm(n, phi) and ratio < min_ratio):
                    min_n, min_phi, min_ratio = n, phi, ratio
    print(min_n, min_phi, min_ratio)
    return


if __name__ == '__main__':
    main()
