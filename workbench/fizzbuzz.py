#!/usr/bin/env python3

from sys import argv

cases = [(3, "Fizz"), (5, "Buzz"), (7, "Bazz"), (11, "Boo"), (13, "Blip")]

upTo = int(argv[1]) if len(argv) > 1 else 101

for i in range(1, upTo):
    out = [w for (n, w) in cases if i % n == 0]
    if out:
        print("".join(out))
    else:
        print(i)
