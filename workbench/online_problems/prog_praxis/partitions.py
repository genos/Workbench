from functools import update_wrapper

def memoize(func):
    func.memo = {}
    def wrapper(arg):
        try:
            return func.memo[arg]
        except KeyError:
            func.memo[arg] = result = func(arg)
            return result
    return update_wrapper(wrapper, func)


@memoize
def partitions(n):
    parts = set([tuple([n])])
    for k in xrange(1, n):
        for p in partitions(n - k):
            parts.add(tuple(sorted([k] + p)))
    return map(list, parts)


def zs1(n):
    x = [1] * n
    x[0], m, h = n, 0, 0
    yield [x[0]]
    while x[0] != 1:
        if x[h] == 2:
            m, x[h] = m + 1, 1
            h -= 1
        else:
            r = x[h] - 1
            t, x[h] = m - h + 1, r
            while t >= r:
                h += 1
                x[h], t = r, t - r
            if t == 0:
                m = h
            else:
                m = h + 1
                if t > 1:
                    h += 1
                    x[h] = t
        yield x[:m + 1]


def zs2(n):
    x = [1] * (n + 1)
    yield x[1:]
    x[:2] = -1, 2
    h, m = 1, n - 1
    yield x[1: m + 1]
    while x[1] != n:
        if m - h > 1:
            h += 1
            x[h], m = 2, m - 1
        else:
            j = m - 2
            while x[j] == x[m - 1]:
                x[j] = 1
                j -= 1
            h = j + 1
            x[h], r, x[m] = x[m - 1] + 1, x[m] + x[m - 1] * (m - h - 1), 1
            if m - h > 1:
                x[m - 1] = 1
            m = h + r - 1
        yield x[1: m + 1]


if __name__ == "__main__":
    print sorted(partitions(6))
    print list(zs1(6))
    print list(zs2(6))


"""
It strikes me that many recursive solutions will probably be inefficient
(except for Haskell ones, perhaps), since solutions to subproblems are
recomputed every time they're needed, similar to SICP's discussion of Fibonacci
number generation.
First up, a memoizing version (based on a Python version that Programming
Praxis pointed me to on StackOverflow) that stores previous answers.
Next are <code>zs1</code> and <code>zs2</code>, Python versions of two
algorithms found in <code>Fast Algorithms for Generating Integer
Paritions</code> by Zoghbi and Stojmenovic (1998). They're iterative and more
efficient than the first solution, if uglier. I've made them generators instead
of lists, but that's not really relevant to the algorithms.
[sourcecode lang="python"]

[/sourcecode]
"""
