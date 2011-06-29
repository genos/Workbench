#!/usr/bin/env python


def quad(xs):
    a = b = d = 0                   # a & b = indices maximizing diff = d
    for i in xrange(len(xs)):
        for j in xrange(i, len(xs)):
            if xs[j] - xs[i] > d:
                a, b, d = i, j, xs[j] - xs[i]
    return (a, b, d)


def linear(xs):
    a = b = d = im = d2 = 0
# a & b = indices maxing diff = d, im = index of min, d2 = current value - min
    for i in xrange(len(xs)):
        if xs[i] < xs[im]:
            im = i                  # search for index of minimum value
        d2 = xs[i] - xs[im]         # search for max difference
        if d < d2:
            a, b, d = im, i, d2
    return (a, b, d)


def test(method, xs, a, b, d):
    print "Testing %s..." % method.func_name
    print "xs = %s " % xs
    print "Max diff for xs is %s from indices (%s, %s)." % (d, a, b)
    print "Does %s give this answer?\t%s\n" % (method.func_name, "Yes" if
            method(xs) == (a, b, d) else "No")
    return None


if __name__ == "__main__":
    for method in (quad, linear):
        for (xs, a, b, d) in (([4, 3, 9, 1, 8, 3, 6, 7, 5], 3, 4, 7),
                              ([4, 2, 9, 1, 8, 3, 6, 7, 5], 1, 2, 7),
                              (range(10, 0, -1), 0, 0, 0)):
            test(method, xs, a, b, d)

"""
@arturasl Sorry! I read the letter at the end of your name as an "i" instead of
an "l." Might need new glasses...

I have to say I needed help finding the linear solution; brain's a bit slow on
Fridays I suppose. I've posted my solution on
<a href="https://gist.github.com/898261">github</a>.
"""
