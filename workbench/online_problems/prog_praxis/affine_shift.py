#!/usr/bin/env python


def char_to_int(c):
    return ord(c) - 65


def int_to_char(i):
    return chr(65 + i)


def encipher(a, b, x):
    return (a * x + b) % 26


def inv(a):
    return next(i for i in xrange(1, 26) if (i * a) % 26 == 1)


def decipher(a, b, x):
    return (inv(a) * (x - b)) % 26


def encrypt(p_text, a, b):
    return ''.join(int_to_char(encipher(a, b, char_to_int(p))) for p in p_text)


def decrypt(c_text, a, b):
    return ''.join(int_to_char(decipher(a, b, char_to_int(c))) for c in c_text)


if __name__ == "__main__":
    P_TEXT = "PROGRAMMINGPRAXIS"
    A, B = 5, 8
    C_TEXT = encrypt(P_TEXT, A, B)
    P_PRIME = decrypt(C_TEXT, A, B)
    print P_TEXT
    print C_TEXT
    print P_PRIME
