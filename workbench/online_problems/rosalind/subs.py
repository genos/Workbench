#!/usr/bin/env python
# coding: utf-8


def all_locations_sub(superstring, substring):
    i = -1
    try:
        while True:
            i = superstring.index(substring, i + 1)
            yield i + 1
    except ValueError:
        raise StopIteration


if __name__ == "__main__":
    print(' '.join(map(str, all_locations_sub("GATATATGCATATACTT", "ATAT"))))
    with open("data/rosalind_subs.txt") as f:
        superstring, substring = f.read().strip().split('\n')
        print(' '.join(map(str, all_locations_sub(superstring, substring))))
