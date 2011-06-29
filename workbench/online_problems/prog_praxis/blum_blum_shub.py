#!/usr/bin/env python
"""blum_blum_shub.py
My submission to http://programmingpraxis.com/2009/08/18/blum-blum-shub/
Uses Blum Blum Shub pseudorandom number generator to construct a cipher via
bitwise XOR.

GRE, 6/1/11
"""


def bbs_gen(n, seed):
    """Blum Blum Shub PRNG"""
    x = pow(seed, 2, n)
    while True:
        yield x % 256
        x = pow(x, 2, n)


def xor_cipher(text, n, seed):
    """Uses Blum Blum Shub generator to en/decipher text via XOR"""
    bbs = bbs_gen(n, seed)
    return ''.join(chr(x) for x in (ord(t) ^ next(bbs) for t in text))


if __name__ == "__main__":
    P, Q, S = 983, 991, 17
    PLAIN = "PROGRAMMING PRAXIS"
    CIPHER = xor_cipher(PLAIN, P * Q, S)
    print CIPHER
    print xor_cipher(CIPHER, P * Q, S)
