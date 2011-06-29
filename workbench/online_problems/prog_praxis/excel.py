#!/usr/bin/env python


def column1(exc):
    c = 0
    for letter in exc.upper():
        c = 26 * c + (ord(letter) - 64)
    return c


def column2(exc):
    return reduce(lambda c, l: 26 * c + (ord(l) - 64), exc.upper(), 0)


def excel1(col):
    e = []
    while col > 0:
        col -= 1
        r = col % 26
        col //= 26
        e.append(chr(r + 65))
    return "".join(e[::-1])


def excel2(col):
    ds = []
    while col > 0:
        col, d = divmod(col - 1, 26)
        ds.append(d)
    return "".join(chr(d + 65) for d in ds[::-1])


from itertools import imap


def test_excel():
    print all(imap(lambda i: column2(excel2(i)) == i, xrange(1, 2 ** 16)))
    return None


if __name__ == "__main__":
    for col in [1, 26, 27, 256]:
        print excel1(col)
        print excel2(col)
    for exc in ["A", "Z", "AA", "IV"]:
        print column1(exc)
        print column2(exc)
    test_excel()


"""My <a href="http://codepad.org/AGTiPoy1">Python</a> solution.
I made two versions of each, trying to practice a more functional style. I'm
indebted to Remco's work for noticing that finding the correct digits is
<code>divmod</code> in disguise.
"""
