def tetra(n):
    return n * (n + 1) * (n + 2) / 6


def prob18_linear(target):
    n = 1
    while tetra(n) < target:
        n += 1
    return n


def bin_search(target, func):
    lo, hi = 1, 10
    while func(hi) < t:
        hi *= 10
    mid = (lo + hi) / 2
    fmid = func(mid)
    while fmid != target:
        if fmid < target:
            lo, mid = mid, (mid + hi) / 2
        else:
            hi, mid = mid, (lo + mid) / 2
        fmid = func(mid)
    return mid


def prob18_log(t):
    return bin_search(t, tetra)
