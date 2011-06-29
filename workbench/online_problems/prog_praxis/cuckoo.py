#!/usr/bin/env python

### prime & ilog2 helpers
import itertools


def erat2():
    D = {}
    yield 2
    for q in itertools.islice(itertools.count(3), 0, None, 2):
        p = D.pop(q, None)
        if p is None:
            D[q * q] = q
            yield q
        else:
            x = p + q
            while x in D or not (x & 1):
                x += p
            D[x] = p


# Inefficient, but not the point of the exercise
def next_prime(p):
    e = erat2()
    x = e.next()
    while x <= p:
        x = e.next()
    return x


def ilog2(x):
    assert x >= 0, ValueError("Log domain error, positive numbers only.")
    c = -1
    while x:
        c += 1
        x >>= 1
    return c
###


### current exercise
def string_hash(s, x):
    return reduce(lambda h, c: h * x + ord(c), s, 0)


class CuckooTable(list):
    def __init__(self, max_size=25, x1=31, x2=37):
        self.max_probes = max(2 * ilog2(max_size), 20)
        self.table_size = next_prime(2 * max_size)
        list.__init__(self, [None] * self.table_size)
        self.x1 = x1
        self.x2 = x2
        return None

    def __repr__(self):
        r = "Cuckoo Table:\nx1 = %d\nx2 = %d\n" % (self.x1, self.x2)
        r += list.__repr__(self)
        return r

    def resetMultipliers(self):
        self.x1 = next_prime(self.x1)
        self.x2 = next_prime(self.x2)
        return None

    def keyHash(self, key, x):
        return string_hash(key, x) % self.table_size

    def lookup(self, key):
        for x in (self.x1, self.x2):
            h = self.keyHash(key, x)
            if self[h] and self[h][0] == key:
                return self[h]
        return None

    def insert(self, key, value):
        if self.lookup(key):
            h1, h2 = self.keyHash(key, self.x1), self.keyHash(key, self.x2)
            if self[h1] and self[h1][0] == key:
                self[h1] = (key, value)
            else:
                self[h2] = (key, value)
        else:
            k, v, c = key, value, self.max_probes
            while c > 0:
                h1, h2 = self.keyHash(k, self.x1), self.keyHash(k, self.x2)
                if not self[h1]:
                    self[h1] = (k, v)
                    break
                elif not self[h2]:
                    self[h2] = (k, v)
                    break
                else:
                    t = self[h1]
                    self[h1] = (k, v)
                    k, v, c = t[0], t[1], c - 1
            if c == 0:
                self.rehash()
                self.insert(k, v)
        return None

    def rehash(self):
        self.resetMultipliers()
        _newTable = iter(self[:])
        for i in xrange(len(self)):
            self[i] = None
        for entry in _newTable:
            if entry is not None:
                self.insert(entry[0], entry[1])
        return None

    def delete(self, key):
        for x in (self.x1, self.x2):
            h = self.keyHash(key, x)
            if self[h] and self[h][0] == key:
                self[h] = None
        return None

    def enlist(self):
        return list(self)

words = ["alpha", "bravo", "charlie", "delta", "echo",
        "foxtrot", "golf", "hotel", "india", "juliet", "kilo", "lima",
        "mike", "november", "oscar", "papa", "quebec", "romeo", "sierra",
        "tango", "uniform", "victor", "whiskey", "xray", "yankee", "zulu"]


def cuckoo_test():
    t = CuckooTable()
    print t.lookup("praxis") is None
    for val in xrange(len(words)):
        t.insert(words[val], val + 1)
    print t.lookup("praxis") is None
    print t.lookup("papa")
    t.delete("papa")
    print t.lookup("papa") is None
    return None


if __name__ == "__main__":
    cuckoo_test()


"""I've changed my answer, building on lists instead of dictionaries; the code
is posted on <a href="http://codepad.org/JljWryqa">codepad</a>.
I also modified my <code>ilog2()</code> so it uses only integer operations
instead of just taking the log base 2 and rounding down to an integer.
"""
