#!/usr/bin/env python
"""bishop.py

My solution to http://programmingpraxis.com/2011/05/03/squaring-the-bishop/
Not as fast as I might like, but relatively straightforward.

GRE, 5/3/11
"""
from pprint import pprint


def prefs(word_list, prefix):
    """
    All words in word_list that start with prefix
    """
    return (w for w in word_list if w.startswith(prefix))


def candidates(words, word_list):
    """
    List of possible words that come from the appropriate prefix, or None if no
    words exist. Note that when making the nth prefix, we have n words; just
    grab the nth letter of those words, where n = len(words)
    """
    nexts = prefs(word_list, ''.join(w[len(words)] for w in words))
    if nexts:
        return [words + [n] for n in nexts]
    else:
        return None


def squares(word, word_list):
    """
    List of word squares for word
    """
    sqrs = [[word]]
    for _ in xrange(1, len(word)):
        if not sqrs:
            return None
        else:
            sqrs = sum((c_list for c_list in (candidates(s, word_list) for s in
                sqrs) if c_list), [])
    return sqrs


def is_square(words):
    """
    Just double checking...
    """
    if len(words) <= 1:
        return True
    else:
        return all(word[0] in words[0] for word in words[1:]) and is_square(
                [word[1:] for word in words[1:]])


if __name__ == "__main__":
    import sys
    TARGET = sys.argv[1] if sys.argv[1:] else "bishop"
    with open("113809of.fic") as wf:
        WORDS = [w for w in (x.strip() for x in wf.readlines()) if w.isalpha()
                and len(w) == len(TARGET)]
        SQRS = squares(TARGET, WORDS)
        pprint(SQRS)
        print all(is_square(s) for s in SQRS)
        print len(SQRS)
