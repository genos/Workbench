#!/usr/bin/env python

import random


def card():
    cols = [random.sample(xrange(a, b), 5) for (a, b) in
            zip([1, 16, 31, 46, 61], [16, 31, 46, 61, 76])]
    return [num for col in cols for num in col]


def bingo(c):
    _test = lambda xs: sum(c[x] for x in xs) == 0
    if any(map(_test, [
        xrange(5),              # B
        xrange(5, 10),          # I
        [10, 11, 12, 13],       # N
        xrange(14, 19),         # G
        xrange(19, 24),         # O
        [0, 5, 10, 14, 19],     # 1st row
        [1, 6, 11, 15, 20],     # 2nd row
        [2, 7, 16, 21],         # 3rd row
        [3, 8, 12, 17, 22],     # 4th row
        [4, 9, 13, 18, 23],     # 5th row
        [0, 6, 17, 23],         # nw to se diag
        [4, 8, 15, 19]])):      # ne to sw diag
        return True
    else:
        return False


def find_winner(cards):
    for i in xrange(len(cards)):
        if bingo(cards[i]):
            return i
    return None


def play(n):
    cards = [card() for _ in xrange(n)]
    cage = range(1, 76)
    random.shuffle(cage)
    round, winner = 0, 0
    while True:
        if any(map(bingo, cards)):
            winner = find_winner(cards)
            return round, winner
        else:
            round += 1
            call = cage.pop()
            for c in cards:
                for i in xrange(24):
                    if c[i] == call:
                        c[i] = 0


if __name__ == "__main__":
    print sum(map(lambda _: play(1)[0], xrange(10000))) / 1e4
    print sum(map(lambda _: play(500)[0], xrange(10000))) / 1e4
