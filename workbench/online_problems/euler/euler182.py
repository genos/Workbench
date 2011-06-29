#!/usr/bin/env python2.6

from time import time
from gmpy import gcd   # much faster than homecoded or fractions.gcd()
start = time()

# Section 1: preliminary definitions
p = 1009; pm1 = p - 1
q = 3643; qm1 = q - 1
n = p * q
phi = pm1 * qm1

# Section 2: building our list of usable e values, as an iterator object
es = (e for e in xrange(1, phi, 2) if gcd(e, phi) == 1)

# Section 3: building a dict of # unconcealed messages
n_unc = {}
for e in es:
    u = (1 + gcd(e - 1, pm1)) * (1 + gcd(e - 1, qm1))
    if u in n_unc:
        n_unc[u].append(e)
    else:
        n_unc[u] = [e]

# Section 4: printing sum of es that give min # unc. messages
m = min(n_unc.iterkeys())
print(sum(n_unc[m]))
print(time() - start)
