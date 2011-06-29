#!/usr/bin/env python2.6
"""
rsa.py

Key generation, encryption, decryption via standard RSA.
http://programmingpraxis.com/2010/11/16/rsa-cryptography/

GRE, 11/19/10
"""

##########################################
#                                        #
#            Preliminaries               #
#                                        #
##########################################

import random
try:
# If we can use gmpy's fast procedures, we will...
    import gmpy

    def prime_gen(k):
        """
        Prime number p in [2^(k/2-1), 2^(k/2).
        """
        assert k >= 1, "Sorry, need a bigger input"
        p = 0
        while not gmpy.is_prime(p):
            p = random.randrange(pow(2, k//2-1) + 1, pow(2, k//2), 2)
        return p

    def extended_euclid(a, m):
        return [int(x) for x in gmpy.gcdext(a, m)]

except ImportError:
# ...if we can't, we build our own.
    def gcd(a, b):
        while b:
            a, b = b, a % b
        return a

    def extended_euclid(a, b):
        (x1, x2, x3) = (1, 0, a)
        (y1, y2, y3) = (0, 1, b)
        while (y3 != 0):
            quotient = x3 / y3
            tmp1 = x1 - quotient * y1
            tmp2 = x2 - quotient * y2
            tmp3 = x3 - quotient * y3
            (x1, x2, x3) = (y1, y2, y3)
            (y1, y2, y3) = (tmp1, tmp2, tmp3)
        return x3, x1, x2

# Miller-Rabin primality stuff:

    def split(n):
        """
        Splits n into 2^s * r for an odd r; used in Miller-Rabin.
        """
        s = 0
        while (n > 0) and (n % 2 == 0):
            s += 1
            n >>= 1
        return (s,n)


    def P(a,r,s,n):
        """
        Condition for primality in Miller-Rabin test.
        """
        if pow(a, r, n) == 1:
            return True
        elif (n - 1) in [pow(a, r*(2**j), n) for j in range(s)]:
            return True
        else:
            return False


    def miller_rabin(n, t):
        """
        Tests n for primality t times.
        """
        (s, r) = split(n - 1)
        for i in xrange(t):
            a = random.randint(2, n-1)
            if not P(a, r, s, n):
                return False
        return True


    def prime_gen(k):
        """
        Generates an odd that passes the Miller Rabin primality test for t = 50
        in the interval [2^(k-1), 2^k].
        """
        assert k >= 1, "Sorry, need a bigger input"
        p = 0
        while (p == 0):
            p = random.randrange(pow(2,k//2-1) + 1, pow(2, k//2), 2)
            if not miller_rabin(p, 50):
                p = 0
        return p

finally:
# Modular inversion
    def mod_inv(a, m):
        g, s, t = extended_euclid(a, m)
        if g == 1:
            return s % m
        else:
            return None

##########################################
#                                        #
#              Main Work                 #
#                                        #
##########################################

def key_gen(k = 32, e = prime_gen(32)):
    """
    Build keys for RSA
    """
    p = prime_gen(k)
    q = prime_gen(k)
    d = mod_inv(e, (p-1)*(q-1))
    return p*q, d



def crypt(message, key, mod):
    return pow(message, key, mod)


##########################################
#                                        #
#               Testing                  #
#                                        #
##########################################
if __name__ == '__main__':
    k = 32
    e = 65537
    n, d = key_gen(k, e)
    print "n = %s\nd = %s" % (n, d)
    m = 42
    print "Message m = %s" % m
    c = crypt(m, e, n)
    print "Encrypted c = %s" % c
    m2 = crypt(c, d, n)
    print "Decrypted m2 = %s" % m2
