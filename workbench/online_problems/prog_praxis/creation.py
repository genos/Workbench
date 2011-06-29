#!/usr/bin/env python
"""creation.py

My submission for http://programmingpraxis.com/2009/03/03/creation/
Trying to break a Vigenere Cipher

GRE, 5/19/11
"""

from collections import Counter
from pprint import pprint


def split_n(seq, n):
    """Split sequence into groupings by every nth item"""
    return (seq[i::n] for i in xrange(n))


def most_freq(seq):
    """Most frequent item in sequence"""
    return Counter(seq).most_common(1)[0][0]


def pass_n(c_text, n):
    """Assume that most frequent item is a space (with ord 32); return a
    password of length n from those most frequent items"""
    most_freqs = (most_freq(seq) for seq in split_n(c_text, n))
    return ''.join(chr(x ^ 32) for x in most_freqs)


def decipher(c_text, password):
    """Given a password, use it to give clear text"""
    return ''.join(chr(x ^ ord(y)) for (x, y) in zip(c_text,
        password * (len(c_text) / len(password))))


def decrypt(c_text, dictionary):
    """Given a dictionary of words, try successive ns in pass_n until the
    password is in dictionary. Then, return deciphered text via decipher."""
    possibles = []
    for n in xrange(len(max(dictionary, key=len))):
        password = pass_n(c_text, n)
        if password.lower() in dictionary:
            possibles.append((n, password, decipher(c_text, password)))
    return possibles


if __name__ == "__main__":
    with open("creation_ctext.txt") as C_FILE:  # Numbers in single line file
        C_TEXT = [int(x) for x in C_FILE.read().split()]
        with open("/usr/share/dict/web2") as DICT_FILE:
            DICTIONARY = set(line.strip() for line in DICT_FILE.readlines())
            pprint(decrypt(C_TEXT, DICTIONARY))
