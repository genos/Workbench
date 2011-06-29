#!/usr/bin/env python


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def frac(n, d):
    if d == 0:
        raise ZeroDivisionError
    else:
        sign = -1 if d < 0 else 1
        ad = abs(d)
        g = gcd(n, ad)
        return (sign * (n // g), ad // g)


def num(x):
    return x[0]


def denom(x):
    return x[1]


def plus(x, y):
    return frac(num(x) * denom(y) + denom(x) * num(y), denom(x) * denom(y))


def minus(x, y):
    return plus(x, frac(-num(y), denom(y)))


def times(x, y):
    return frac(num(x) * num(y), denom(x) * denom(y))


def divide(x, y):
    return times(x, frac(denom(y), num(y)))


def is_less_than(x, y):
    return num(x) * denom(y) < denom(x) * num(y)


def frac_print(x):
    print str(num(x)) + "/" + str(denom(x))
    return


if __name__ == "__main__":
    frac_print(plus(frac(1, 3), frac(-1, 7)))
    frac_print(minus(frac(1, 3), frac(-1, 7)))
    frac_print(times(frac(1, 3), frac(-1, 7)))
    frac_print(divide(frac(1, 3), frac(-1, 7)))
    print(is_less_than(frac(1, 3), frac(-1, 7)))


"""
Seems reminiscent of SICP :-)
Since I implemented fractions as pairs, I included two procedures to get the
numerator and denominator. I also included pretty printing of fractions. My
Python solution:
[sourcecode lang=python]
#!/usr/bin/env python

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def frac(n, d):
    if d == 0:
        raise ZeroDivisionError
    else:
        sign = -1 if d < 0 else 1
        ad = abs(d)
        g = gcd(n, ad)
        return (sign * (n // g), ad // g)

def num(x):
    return x[0]

def denom(x):
    return x[1]

def plus(x, y):
    return frac(num(x) * denom(y) + denom(x) * num(y), denom(x) * denom(y))

def minus(x, y):
    return plus(x, frac(-num(y), denom(y)))

def times(x, y):
    return frac(num(x) * num(y), denom(x) * denom(y))

def divide(x, y):
    return times(x, frac(denom(y), num(y)))

def is_less_than(x, y):
    return num(x) * denom(y) < denom(x) * num(y)

def frac_print(x):
    print str(num(x)) + "/" + str(denom(x))
    return

if __name__ == "__main__":
    frac_print(plus(frac(1, 3), frac(-1, 7)))
    frac_print(minus(frac(1, 3), frac(-1, 7)))
    frac_print(times(frac(1, 3), frac(-1, 7)))
    frac_print(divide(frac(1, 3), frac(-1, 7)))
    print(is_less_than(frac(1, 3), frac(-1, 7)))
[/sourcecode]
"""
