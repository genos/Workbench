#!/usr/bin/env python
from random import randrange

def single_game(chutes, ladders):
    pos, path = 0, []
    while pos < 100:
        pos += randrange(1, 7)
        if pos in chutes:
            pos = chutes[pos]
        elif pos in ladders:
            pos = ladders[pos]
        path.append(pos)
    if path[-1] > 100:
        path[-1] = 100
    return path


def mult_games(n, chutes, ladders):
    return [len(single_game(chutes, ladders)) for _ in xrange(n)]


def compete(k, n, chutes, ladders):
    return [min(mult_games(k, chutes, ladders)) for _ in xrange(n)]


def mean(xs):
    return sum(xs) / float(len(xs))


def stats(k, n, chutes, ladders):
    games = compete(k, n, chutes, ladders)
    return min(games), max(games), mean(games)


if __name__ == "__main__":
    chutes = {16: 6, 47: 26, 49: 11, 56: 53, 62: 19, 64: 60, 87: 24, 93: 73,
                95: 75, 98: 78}
    ladders = {1: 38, 4: 14, 9: 31, 21: 42, 28: 84, 36: 44, 51: 67, 71: 91,
                80: 100}
    for k in xrange(1, 7):
        print stats(k, 100000, chutes, ladders)
"""
Not terribly fast, though it gets the job done. To speed this up, I tried
1. Numpy arrays (which doesn't get much of an improvement),
2. PyPy (which sped things up a lot), and
3. Cython to compile the code.
None of these three options come standard with the typical Python installation,
unfortunately. As a further exercise for myself, I went about compiling via
Cython, without changing the original code beyond all recognition; details
available on <a href="http://codepad.org/HgrYW753">codepad</a>.
[sourcecode lang="python"]
#!/usr/bin/env python
from random import randrange

def single_game(chutes, ladders):
    pos, path = 0, []
    while pos < 100:
        pos += randrange(1, 7)
        if pos in chutes:
            pos = chutes[pos]
        elif pos in ladders:
            pos = ladders[pos]
        path.append(pos)
    if path[-1] > 100:
        path[-1] = 100
    return path


def mult_games(n, chutes, ladders):
    return [len(single_game(chutes, ladders)) for _ in xrange(n)]


def compete(k, n, chutes, ladders):
    return [min(mult_games(k, chutes, ladders)) for _ in xrange(n)]


def mean(xs):
    return sum(xs) / float(len(xs))


def stats(k, n, chutes, ladders):
    games = compete(k, n, chutes, ladders)
    return min(games), max(games), mean(games)


if __name__ == "__main__":
    chutes = {16: 6, 47: 26, 49: 11, 56: 53, 62: 19, 64: 60, 87: 24, 93: 73,
                95: 75, 98: 78}
    ladders = {1: 38, 4: 14, 9: 31, 21: 42, 28: 84, 36: 44, 51: 67, 71: 91,
                80: 100}
    for k in xrange(1, 7):
        print stats(k, 100000, chutes, ladders)
[/sourcecode]
"""
