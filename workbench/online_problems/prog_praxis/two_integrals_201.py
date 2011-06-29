#!/usr/bin/env python

from __future__ import division
import math

GAMMA = 0.5772156649015328606065


def exp_int(x):
    s = GAMMA + math.log(x)
    term, k, f = x, 1, 1
    while term > 1e-17:
        s += term
        k += 1
        f *= k
        term = pow(x, k) / (k * f)
    return s


def log_int(x):
    return exp_int(math.log(x))


def offset_log_int(x):
    return log_int(x) - 1.04516378011749278


if __name__ == "__main__":
    print "Li_offset(1e6) = {0:d}".format(int(round(offset_log_int(1e6))))
    print "Li_offset(1e21) = {0:d}".format(int(round(offset_log_int(1e21))))
# Output:
# Li_offset(1e6) = 78627
# Li_offset(1e21) = 21127269486616088576


"""
My Python solution. This blog and my free time studies are drawing me more and
more towards Scheme and Haskell, but since there are two great solutions in
those languages already I felt I should offer a solution in a different
language. I've moved towards the newer "format" instead of the older printf
style string formatting.
[sourcecode lang="python"]
#!/usr/bin/env python

from __future__ import division
import math

GAMMA = 0.5772156649015328606065


def exp_int(x):
    s = GAMMA + math.log(x)
    term, k, f = x, 1, 1
    while term > 1e-17:
        s += term
        k += 1
        f *= k
        term = pow(x, k) / (k * f)
    return s


def log_int(x):
    return exp_int(math.log(x))


def offset_log_int(x):
    return log_int(x) - 1.04516378011749278


if __name__ == "__main__":
    print "Li_offset(1e6) = {0:d}".format(int(round(offset_log_int(1e6))))
    print "Li_offset(1e21) = {0:d}".format(int(round(offset_log_int(1e21))))
# Output:
# Li_offset(1e6) = 78627
# Li_offset(1e21) = 21127269486616088576
[/sourcecode]
"""
