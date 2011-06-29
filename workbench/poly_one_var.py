#!/usr/bin/env python
# TODO
# 1.    Use an ordered dict to speed up leading term/power product calls
# 2.    Parse polynomials from strings; hide the underlying data structure
# 3.    Expose important methods as functions, e.g. lt()

from fractions import Fraction
import operator


def _removeZeros(d):
    """Auxiliary procedure to remove zeros from a dictionary. Created because
    trying to remove from a dictionary inside a loop led to errors."""
    l = list((k, d[k]) for k in d)
    for p in l:
        if p[1] == 0:
            l.remove(p)
    return dict(l)


class Polynomial(object):
    """Polynomials in one variable. The parent ring and variable are user
    specifiable. Takes as input a power-->coefficient dictionary (pc_dict) that
    specifies the coefficients of the polynomial indexed by power.

    Example::
        >>> p = Polynomial(pc_dict={1:2, 3:4, 5:6}, ring='Q', var='x')
        >>> print p
        6x^5 + 4x^3 + 2x^1
        >>> p.lc()
        6
        >>> print p.lt()
        6x^5
        >>> print p + Polynomial(pc_dict={7:5, 5:3, 3:1})
        5x^7 + 9x^5 + 5x^3 + 2x^1
    """

    def __init__(self, pc_dict={}, ring='Q', var='x'):
        self.ring = ring
        self.pc_dict = {}
        if ring == 'R':
            for k in pc_dict:
                self.pc_dict[int(k)] = float(pc_dict[k])
        elif ring == 'Q':
            for k in pc_dict:
                self.pc_dict[int(k)] = Fraction(pc_dict[k])
        elif ring == 'Z':
            for k in pc_dict:
                self.pc_dict[int(k)] = int(pc_dict[k])
        else:
            raise ValueError("Unknown ring")
        self.var = var
        return

    def _checkVar(self, other):
        """Checks whether the two polynomials are in the same variable"""
        if self.var != other.var:
            raise ValueError("One variable polynomials only")
        else:
            return True

    def _checkRing(self, other):
        """Checks whether the two polynomials are from the same ring"""
        if self.ring != other.ring:
            raise ValueError("Polynomials from two different rings")
        else:
            return self.ring

    def __repr__(self):
        """String output of a polynomial"""
        s = ' + '.join(str(self.pc_dict[k]) + '*' + self.var + '^' + str(k)
                       for k in sorted(self.pc_dict.keys(), reverse=True))
        if len(s) == 0:
            return '0'
        else:
            # Replace 'x^0' with constant, 'x^1' with 'x', '+ -' with '- '
            for pair in [('*' + self.var + '^0', ''),
                         (self.var + '^1 ', self.var + ' '),
                         ('+ -', '- ')]:
                s = s.replace(*pair)
            return s

    def _addOrSub(self, other, op):
        """Adds or subtracts the polynomial other to/from the polynomial self,
        depending on what op is. Written to factor out some repeated code.
        """
        self._checkVar(other)
        self._checkRing(other)
        pc_dict = self.pc_dict.copy()
        for k in other.pc_dict:
            try:
                pc_dict[k] = op(pc_dict[k], other.pc_dict[k])
            except (KeyError, TypeError):
                pc_dict[k] = op(0, other.pc_dict[k])
        pc_dict = _removeZeros(pc_dict)
        return Polynomial(pc_dict=pc_dict, ring=self.ring)

    def __add__(self, other):
        return self._addOrSub(other, operator.__add__)

    def __sub__(self, other):
        return self._addOrSub(other, operator.__sub__)

    def __mul__(self, other):
        """Different from addition or subtraction, so I gave * its own code."""
        self._checkVar(other)
        ring = self._checkRing(other)
        pc_dict = {}
        for k1 in self.pc_dict:
            for k2 in other.pc_dict:
                k = k1 + k2
                try:
                    pc_dict[k] += self.pc_dict[k1] * other.pc_dict[k2]
                except KeyError:
                    pc_dict[k] = self.pc_dict[k1] * other.pc_dict[k2]
        return Polynomial(pc_dict=pc_dict, ring=ring)

    def __div__(self, other):
        """Division algorithm"""
        self._checkVar(other)
        self._checkRing(other)
        if self.ring == 'Z':
            pass
        else:
            d = other.deg()
            l = other.lt()
            c = other.lc()
            q = Polynomial()
            r = Polynomial(pc_dict=self.pc_dict, ring=self.ring, var=self.var)
            tmp = 0
            while r != 0 and d <= r.deg():
                tmp = Polynomial(pc_dict={(r.deg() - d): r.lc() / other.lc()})
                q += tmp
                r -= tmp * other
        return (q, r)

    def __nonzero__(self):
        """Determines whether this is the zero polynomial"""
        return self.deg() > -1

    def deg(self):
        """The degree of our polynomial"""
        degrees = sorted(self.pc_dict.keys(), reverse=True)
        if len(degrees) == 0:
            return -1
        else:
            return degrees[0]

    def lt(self):
        """The leading term, as a new polynomial"""
        d = self.deg()
        ltc = self.pc_dict[d]
        return Polynomial(pc_dict={d: ltc})

    def lc(self):
        """The leading coefficient of our polynomial"""
        return self.pc_dict[sorted(self.pc_dict.keys(), reverse=True)[0]]


if __name__ == "__main__":
    f = Polynomial(pc_dict={0: 17, 17: 42}, ring='Q')
    g = Polynomial(pc_dict=dict(zip(range(5), range(1, 6))), ring='Z')
    h = Polynomial(pc_dict=dict(zip(range(5), range(1, 6))), ring='R')
    p = Polynomial(pc_dict=dict(zip(range(5), range(1, 6))), ring='Q')

    def printer(name):
        print "{0} = {1}".format(name, eval(name))

    printer('f')
    printer('g')
    printer('h')
    printer('p')
    print "-" * 17 + "\n"

    #printer('f + g')
    printer('f + p')
    printer('f - p')
    printer('f * p')
    #printer('f / p')
    print "-" * 17 + "\n"

    printer("f.lt()")
    printer("f.lc()")
    print "-" * 17 + "\n"

    q, r = (Polynomial(pc_dict={3: 1, 2: -2, 1: 2, 0: 8}, ring='Q') /
            Polynomial(pc_dict={2: 2, 1: 3, 0: 1}, ring='Q'))
    print q
    print r
