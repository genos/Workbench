from pprint import pprint
from operator import mul


def pascal(n):
    tri = [1]
    for _ in xrange(n + 1):
        yield tri
        tri = [1] + map(sum, zip(tri, tri[1:] + [0]))


def binomial(a, b):
    return (reduce(mul, xrange(a - b + 1, a + 1), 1) //
            reduce(mul, xrange(1, b + 1), 1))


def pascal_binom(n):
    for a in xrange(n + 1):
        yield [binomial(a, b) for b in xrange(a + 1)]


if __name__ == "__main__":
    pprint(list(pascal(10)))
    pprint(list(pascal_binom(10)))
