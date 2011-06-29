"""
interval.py

My submission for Programming Praxis's "Interval Arithmetic" exercise; see
http://programmingpraxis.com/2010/12/21/interval-arithmetic/

NOTE: requires the fractions module.

GRE, 12/21/10
"""

from fractions import Fraction

class Interval(object):

    def __init__(self, x, y, centerRep = False):
        self.xy = (x, y)
        self.centerRep = centerRep
        return

    def __repr__(self):
        if not self.centerRep:
            return "Interval [%s, %s]" % (self.xy[0], self.xy[1])
        else:
            return "Interval of width %s centered at %s" % (self.xy[1],
                                                            self.xy[0])
    def toCenter(self):
        newX = Fraction((self.xy[0] + self.xy[1]), 2)
        newY = Fraction((self.xy[1] - self.xy[0]), 2)
        return Interval(newX, newY, centerRep = True)

    def fromCenter(self):
        newX = self.xy[0] - self.xy[1]
        newY = self.xy[0] + self.xy[1]
        return Interval(newX, newY, centerRep = False)

    def __add__(self, other, centerRep = False):
        s = self.fromCenter()
        o = other.fromCenter()
        x = s.xy[0] + o.xy[0]
        y = s.xy[1] + o.xy[1]
        return Interval(x, y, centerRep)

    def __sub__(self, other, centerRep = False):
        s = self.fromCenter()
        o = other.fromCenter()
        x = s.xy[0] - o.xy[0]
        y = s.xy[1] - o.xy[1]
        return Interval(x, y, centerRep)

    def __mul__(self, other, centerRep = False):
        s = self.fromCenter()
        o = other.fromCenter()
        x = s.xy[0] * o.xy[0]
        y = s.xy[1] * o.xy[1]
        return Interval(x, y, centerRep)

    def __div__(self, other, centerRep = False):
        s = self.fromCenter()
        o = other.fromCenter()
        try:
            entries = [Fraction(a, b) for a in self.xy for b in other.xy]
            return Interval(min(entries), max(entries), centerRep)
        except ZeroDivisionError:
            print "Error: divison by zero in second Interval"
            return

if __name__ == "__main__":
    X = Interval(1, 2)
    Y = Interval(3, 4)
    print X + Y
    print X - Y
    print X * Y
    print X / Y
    C = X.toCenter()
    print C
    print C.fromCenter()
