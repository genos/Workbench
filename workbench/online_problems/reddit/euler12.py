def factors(n):
    if (n & 1 == 0):
        fs = [2]
        fs.extend(factors(n // 2))
    else:
        f, fs = 3, []
        while n >= pow(f, 2):
            if (n % f == 0):
                fs.append(f)
                n //= f
            else:
                f += 2
        fs.append(n)
    return fs


def num_div(n):
    fs = factors(n)
    prev, fs, f, d = fs[0], fs[1:], 2, 1
    while fs:
        if fs[0] == prev:
            f, fs = f + 1, fs[1:]
        else:
            prev, fs, f, d = fs[0], fs[1:], 2, d * f
    return d * f


def euler_12(n):
    i = tri = 1
    while n >= num_div(tri):
        i, tri = i + 1, tri + i + 1
    return tri


if __name__ == "__main__":
    from time import time
    start = time()
    e12 = euler_12(500)
    stop = time()
    print "The answer is %d." % e12
    print "That answer took me %g seconds" % (stop - start)
