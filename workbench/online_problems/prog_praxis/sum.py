#!/usr/bin/env python

import sys


def checksum(f):
    s = b = 0
    for c in f.read():
        s = (s + ord(c)) % 65535
        b += 1
    p = 0 if (b % 512 == 0) else 1
    return s, (b // 512) + p


def main(args=None):
    if args:
        for arg in args:
            with open(arg) as f:
                s, b = checksum(f)
                print "{0}\t{1}\t{2}".format(s, b, arg)
    else:
        s, b = checksum(sys.stdin)
        print "{0}\t{1}".format(s, b)
    return None


if __name__ == "__main__":
    main(sys.argv[1:])
