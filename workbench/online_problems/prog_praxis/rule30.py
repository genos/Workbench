#!/usr/bin/env python
"""rule30.py

My answer to http://programmingpraxis.com/2011/04/29/rule-30-rng/
GRE, 5/2/11
"""

from bitarray import bitarray


class Automaton(object):
    """
    Generalized class to simulate cellular automaton; basically a
    bitarray-based generator
    """

    def __init__(self, arr, rule=None):
        """
        Build a bitarray to hold state, rule gives transition to next state
        """
        self.arr = bitarray(arr)
        self.rule = rule
        self.size = len(self.arr)
        return None

    def __iter__(self):
        """
        We'd like to use the bitarray for iteration...
        """
        return self.arr

    def __repr__(self):
        """
        ...and representation
        """
        return self.arr.to01()

    def next(self):
        """
        Use rule to transition to next state, return state
        """
        state = bitarray('0' * self.size)
        state[0] = self.rule[self.arr[-1:].to01() + self.arr[:2].to01()]
        state[-1] = self.rule[self.arr[-2:].to01() + self.arr[:1].to01()]
        for i in xrange(1, self.size - 1):
            state[i] = self.rule[self.arr[i - 1: i + 2].to01()]
        self.arr = state
        return state


class Rule30(Automaton):
    """
    Rule 30 specific cellular automaton, with PRNG capabilities
    """

    def __init__(self, seed=1729):
        """
        Extends initialization of Automaton class by specifying rule and seeding
        initial value
        """
        seed_arr = bitarray(bin(seed)[2:])
        seed_arr.extend('0' for _ in xrange(7 * 43 - len(seed_arr)))
        seed_arr = seed_arr[:7 * 43]
        Automaton.__init__(self, arr=seed_arr, rule={'111': False,
            '110': False, '101': False, '100': True, '011': True, '010': True,
            '001': True, '000': False})
        for _ in xrange(1729):
            next(self)
        return None

    def __call__(self):
        """
        Output pseudorandom 1s and 0s
        """
        return self.next().to01()[0]


if __name__ == "__main__":
    import sys
    PRNG = Rule30(seed=int(sys.argv[1] if sys.argv[1:] else 1729))
    for _ in xrange(int(sys.argv[2] if sys.argv[2:] else 10)):
        print PRNG()
