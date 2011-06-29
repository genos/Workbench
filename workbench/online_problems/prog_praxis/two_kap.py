def digits(n):
# Only works on integers > 0
    ds = []
    while n:
        ds.append(n % 10)
        n /= 10
    ds.reverse()
    return ds


def undigits(lst):
    l = len(lst)
    return sum(10 ** (l - i - 1) * lst[i] for i in xrange(l - 1, -1, -1))


def kap_chain(n):
    chain = []
    while n not in (6174, 0):
        chain.append(n)
        ds = digits(n)
        while len(ds) < 4:
            ds.append(0)
        n = undigits(sorted(ds, reverse=True)) - undigits(sorted(ds))
    return chain


def find_longest_kap_chain(lim):
    m, c = 0, 0
    for n in xrange(1, lim):
        l = len(kap_chain(n))
        if l > c:
            m, c = n, l
    return m, c


def split(n):
    ds = digits(n)
    l = len(ds)
    return undigits(ds[:l / 2]), undigits(ds[l / 2:])


def is_kaprekar(n):
    return sum(split(n ** 2)) == n


if __name__ == "__main__":
    print find_longest_kap_chain(10000)
    print [n for n in xrange(1, 1000) if is_kaprekar(n)]

"""
I believe we've done the second part before (exercise 169), but I came up with a
new solution this time.
My solution is available <a href="https://gist.github.com/881134">here</a>.
"""
