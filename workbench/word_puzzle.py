#!/usr/bin/env python
"""word_puzzle.py

I wrote this (with some help from the internet) to help me solve (read: cheat
at) word puzzles like "Scramble With Friends" and Boggle.

GRE, 26 Jun 2012
"""


class Puzzle(object):
    """Collects word puzzle details, like:
        initialization          (init):
            takes in file_name, sets up puzzle
        solving                 (solve):
            finds all words; uses dict_name (default is /usr/share/dict/words):
        output                  (show_words):
            prints out words, longest to shortest
    """

    def __init__(self, file_name):
        """Set up shop"""
        self.board = list()
        self.letters = set()
        with open(file_name) as f:
            for line in f:
                clean_line = line.strip().upper()
                self.board.append(clean_line)
                self.letters.update(clean_line)
        self.size = len(self.board)
        self.words = set()
        self.dictionary = None  # Caching in case we solve multiple times
        self.prefixes = None
        return None

    def __str__(self):
        """A little bit of info"""
        return "Word puzzle with {0} rows and columns".format(self.size)

    def solve(self, dict_name="/usr/share/dict/words"):
        """If we haven't already built our dictionary, do that (only keeping
        words all of whose letters are in self.letters). Next, walk through
        our board, add all words starting at that location.
        """
        if self.dictionary is None:  # If we haven't already cached this...
            self.dictionary = set()
            with open(dict_name) as d:
                for line in d:
                    word = line.strip().upper()
                    if word.startswith('QU'):  # treat as one
                        word = word[0] + word[2:]
                    if (len(word) > 1 and
                            self.letters.intersection(word) == set(word)):
                        self.dictionary.update([word])

        if self.prefixes is None:
            self.prefixes = set(word[:n] for word in self.dictionary for n in
                    xrange(1, len(word) + 1))

        if len(self.words) == 0:  # If we haven't already solved...
            for x, row in enumerate(self.board):
                for y, letter in enumerate(row):
                    self.find_words(letter, location=(x, y), visited=set())
        return None

    def find_words(self, prefix, location, visited):
        """Find all words we can make from given prefix at position = location;
        keep track of visited, so we don't reuse anything"""
        candidates = set(self.neighbors(location)) - visited
        if len(candidates) != 0:
            if prefix in self.dictionary and len(prefix) > 1:
                self.words.add(prefix)
            if prefix in self.prefixes:
                for (x, y) in candidates:
                    p = prefix + self.board[x][y]
                    v = visited.union([location])
                    self.find_words(p, (x, y), v)
        return None

    def neighbors(self, (x, y)):
        """Neighbors of the point (x, y) in a size x size puzzle"""
        for nx in xrange(max(0, x - 1), min(x + 2, self.size)):
            for ny in xrange(max(0, y - 1), min(y + 2, self.size)):
                if (nx, ny) != (x, y):
                    yield (nx, ny)

    def show_words(self):
        """Output words found"""
        print "I found {0} words:".format(len(self.words))
        for word in sorted(self.words, key=len, reverse=True):
            print word
        return None


if __name__ == "__main__":
    from sys import argv
    try:
        PUZZ = Puzzle(argv[1])
        print PUZZ
        if len(argv) > 2:
            PUZZ.solve(dict_name=argv[2])
        else:
            PUZZ.solve()
        PUZZ.show_words()
    except IndexError:
        print "usage: {0} puzzle_file".format(argv[0])
