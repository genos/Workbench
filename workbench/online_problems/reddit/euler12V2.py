from itertools import count


def factors(n):
    while (n % 2 == 0):
        yield 2
        n //= 2
    for c in count():
        f = 3 + 2 * c
        if n <= f * f:
            yield n
            raise StopIteration
        elif (n % f == 0):
            while (n % f == 0):
                yield f
                n //= f


def num_div(n):
    fs = factors(n)
    prev, f, d = fs.next(), 2, 1
    try:
        while True:
            nf = fs.next()
            if nf == prev:
                f += 1
            else:
                prev, f, d = nf, 2, d * f
    except StopIteration:
        return d * f


def euler_12(n):
    tri = 1
    for i in count(1):
        if n >= num_div(tri):
            tri += i + 1
        else:
            return tri


if __name__ == "__main__":
    from time import time
    start = time()
    e12 = euler_12(500)
    stop = time()
    print "The answer is %d." % e12
    print "That answer took me %g seconds." % (stop - start)
