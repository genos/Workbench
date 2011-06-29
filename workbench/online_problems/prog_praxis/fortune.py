#!/usr/bin/env python

from random import choice, randrange, seed
from sys import argv


def whole_file(f):
# easily written, good if f is smallish
    return choice(f.readlines())


def iter_file(f):
# good if f is large
    curr_line, n = None, 0
    for line in f:
        n += 1
        i = 0 if randrange(n) >= 1 else 1
        curr_line = [curr_line, line][i]
    return curr_line


def main(f, func=iter_file):
    print func(f)
    return None


if __name__ == "__main__":
    seed()
    f_name = argv[1] if len(argv) > 1 else "fortunes.txt"
    with open(f_name) as f:
        main(f)


"""
My first solution is similar to Dave Webb's, but I also wrote up an iterative
function that will use less resources if the fortune file is large.
[sourcecode lang="python"]
#!/usr/bin/env python
from random import choice, randrange, seed
from sys import argv

def whole_file(f):
# easily written, good if f is smallish
    return choice(f.readlines())

def iter_file(f):
# good if f is large
    curr_line, n = None, 0
    for line in f:
        n += 1
        i = 0 if randrange(n) >= 1 else 1
        curr_line = [curr_line, line][i]
    return curr_line

def main(f, func=iter_file):
    print func(f)
    return None

if __name__ == "__main__":
    seed()
    f_name = argv[1] if len(argv) > 1 else "fortunes.txt"
    with open(f_name) as f:
        main(f)
[/sourcecode]
"""
