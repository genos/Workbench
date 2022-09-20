#!/usr/bin/env python3

EPS = 1e-11


def cfrac(x, iters=100):
    """
    float, int --> [int, ';', ints]

    Gives the continued fraction representation of x, stopping after iters
    iterations. Due to inexactness of floating point, we use the constant EPS
    for our testing whether x "is" zero.
    """
    c = [int(x), ";"]
    x -= int(x)
    iters -= 1
    while iters > 0 and abs(x) > EPS:
        x = 1 / x
        c.append(int(x))
        x -= int(x)
        iters -= 1
    return c


def num(cf):
    """
    cfrac --> float

    Converts a finite continued fraction back to a float.
    """
    c = cf[:]
    x = 0
    c[-2] += 1
    c.remove(";")
    c.pop()
    while len(c) > 1:
        x = 1.0 / (c.pop() + x)
    x += c[0]
    return x


if __name__ == "__main__":
    c = cfrac(3.245)
    print(c)
    print(num(c))
    from math import e, pi

    p = cfrac(pi)
    print(p)
    print(num(p))
    ef = cfrac(e)
    print(ef)
    print(num(ef))
