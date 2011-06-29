def ilog(b, n):
    c = 0
    i = b
    while i <= n:
        c += 1
        i *= b
    return c

def odd_divisors(n):
    return [d for d in xrange(1, n + 1) if d % 2 and not n % d]

def politeness(n):
    return len(odd_divisors(n)) - 1

def polite(n, d):
    d2 = d//2
    nd = n//d
    if d2 < nd:
        return range(nd - d2, nd + d2 + 1)
    else:
        return range(abs(nd - d2) + 1, (nd + d2 + 1))

def polites(n):
    return [polite(n, d) for d in odd_divisors(n)[1:]]
