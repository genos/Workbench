def searching(arr):
    s = set()
    for a in arr:
        if a in s:
            return a
        else:
            s.add(a)


def sorting(arr):
    brr = sorted(arr)
    for i in xrange(1, len(brr)):
        if brr[i] == brr[i-1]:
            return brr[i]


def gauss(arr):
    # requires that arr contains _all_ natural numbers from one to len(arr)
    n = len(arr)
    return sum(arr) - n * (n - 1) // 2
