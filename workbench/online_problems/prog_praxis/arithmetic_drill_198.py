#!/usr/bin/env python

import random


def drill(n):
    x, y = random.randrange(n), random.randrange(n)
    answer = int(raw_input("{0} + {1} =\t".format(x, y)))
    while x + y != answer:
        print "Wrong, try again!"
        answer = int(raw_input("{0} + {1} =\t".format(x, y)))
    print "Right!"
    return

if __name__ == "__main__":
    import sys
    n = 10 if len(sys.argv) == 1 else int(sys.argv[1])
    try:
        while True: drill(n)
    except (KeyboardInterrupt, EOFError):
        print "\nGoodbye!"
