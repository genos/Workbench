cipher = [(3,8), (2,5), (3,3), (2,5), (3,3), (2,2), (2,5), (3,8), (2,4),
          (2,5), (2,1), (1,1), (2,8)]

from collections import Counter

print Counter(cipher)
print Counter(c[0] for c in cipher)

from string import ascii_lowercase as lower
from itertools import repeat

d = {}
d.update(zip(zip(repeat(2), xrange(1, 10)), lower[:9]))
d.update(zip(zip(repeat(3), xrange(1, 10)), lower[10:19]))
d.update(zip(zip(repeat(1), xrange(1, 10)), lower[19:28]))

print ''.join(d[c] for c in cipher)
