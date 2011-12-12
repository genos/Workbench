#!/usr/bin/env python

from itertools import ifilter, imap, product
print set(xrange(181)) - set(ifilter(lambda s: s < 181,
                                     imap(sum, product(xrange(0, 181, 6),
                                                       xrange(0, 181, 9),
                                                       xrange(0, 181, 20)))))
