#!/usr/bin/env python
"""
numerical_integration.py

My submission to Programming Praxis for the Numerical Integration exercise:
http://programmingpraxis.com/2010/02/09/numerical-integration/


GRE, 4/12/11
"""

from __future__ import division
from math import log


def dx(a, b, n):
    """
    Width of subintervals in partitioning of [a, b] in to n equal parts
    """
    return (b - a) / n


def rectangle(f, a, b, n=1):
    """
    Rectangular Riemann sum using midpoints for sample for integral of f(x)
    over interval [a, b]
    """
    x = lambda k: a + (k + 0.5) * dx(a, b, n)
    return dx(a, b, n) * sum(f(x(k)) for k in xrange(n))


def trapezoid(f, a, b, n=1):
    """
    Trapezoid approximation to integral of f(x) over interval [a, b]
    """
    x = lambda k: a + k * dx(a, b, n)
    return dx(a, b, n) * (f(a) + f(b) + sum(0.5 * (f(x(k)) + f(x(k + 1))) for k
        in xrange(1, n)))


def simpson(f, a, b, n=1):
    """
    Approximate integral of f(x) over [a, b] via Simpson's rule; uses n * 2
    since Simpson's rule requires an even number of subintervals
    """
    x = lambda k: a + k * dx(a, b, 2 * n)
    return dx(a, b, 2 * n) / 3 * (f(a) + f(b) + 2 * sum(f(x(k)) for k in
        xrange(2, 2 * n, 2)) + 4 * sum(f(x(k)) for k in xrange(1, 2 * n, 2)))


def adaptive_quad(f, a, b, quad=simpson, eps=1e-7):
    """
    Adaptive quadrature for integral of f(x) over [a, b]
    """
    int_with5 = quad(f, a, b, 5)
    int_with10 = quad(f, a, b, 10)
    m = (a + b) / 2
    if abs(int_with5 - int_with10) < eps:
        return int_with10
    else:
        return (adaptive_quad(f, a, m, quad, eps) +
                adaptive_quad(f, m, b, quad, eps))


def approx_pi(b):
    """
    Use the logarithmic integral li(b) to approximate pi(b), the number of
    primes less than b
    """
    return adaptive_quad(lambda x: 1 / log(x), 2, b)


if __name__ == "__main__":
    cube = lambda x: pow(x, 3)
    print rectangle(cube, 0, 1, 10000)
    print trapezoid(cube, 0, 1, 10000)
    print simpson(cube, 0, 1, 10000)
    print approx_pi(1e21)
