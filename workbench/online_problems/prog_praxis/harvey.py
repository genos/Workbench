from itertools import combinations, ifilter, imap, izip

def sums(n):
    return [n * (n + 1) / 2,
            n * (n + 1) * (2 * n + 1) / 6,
            (n * (n + 1) / 2) ** 2]

def prop(sub):
    return all(0 == a - b for (a, b) in
               izip(sums(16), map(lambda x: 2 * x,
                                   [sum(sub), sum(x**2 for x in sub),
                                   sum(x**3 for x in sub)])))

if __name__ == "__main__":
    from pprint import pprint
    s = set(range(1, 17))
    print next(imap(lambda sub: [sub, tuple(s.difference(sub))],
              ifilter(prop, combinations(range(1, 17), 8))))
