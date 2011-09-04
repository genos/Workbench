from math import sqrt

def cfrac(n):
    mda = set()
    m, d, a = 0, 1, int(sqrt(n))
    while (m, d, a) not in mda:
        mda.add((m, d, a))
        m = d * a - m
        d = (n - m**2) // d
        a = int((sqrt(n) + m) / d)

    return len(mda) - 1

if __name__ == "__main__":
   print sum(1 for n in xrange(2, 10001) if int(sqrt(n)) != sqrt(n) and
             cfrac(n) & 1)
