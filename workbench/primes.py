#!/usr/bin/env python3

def gcd(a, b):
    while b: a, b = b, a % b
    return abs(a)


def primes(n):
    m = (n - 1) // 2
    b = [True] * m
    i, p, ps = 0, 3, [2]
    while p * p < n:
        if b[i]:
            ps.append(p)
            j = 2 * i * i + 6 * i + 3
            while j < m:
                b[j] = False
                j = j + 2 * i + 3
        i += 1; p += 2
    while i < m:
        if b[i]:
            ps.append(p)
        i += 1; p += 2
    return ps


# Sneaky way to store a value for a function so it's not recomputed
def stored(name, value):
    def decorate(function):
        setattr(function, name, value)
        return function
    return decorate


@stored("ps", set(primes(100)))  # is_prime.ps = {2, 3, ..., 97} forever
def is_prime(n):
    if n in is_prime.ps: return True

    def is_spsp(n, a):
        d, s = n - 1, 0
        while d % 2 == 0:
            d /= 2; s += 1
            if pow(a, d, n) == 1: return True
            else:
                for r in xrange(s):
                    if pow(a, d * pow(2, r), n) == n - 1: return True
                    return False

    for p in is_prime.ps:
        if not is_spsp(n, p): return False
    return True


def factors(n):
    if -1 <= n <= 1: return [n]
    elif n <= 0: return sorted(factors(-n) + [-1])
    else:

        def fact(n=n, c=1, fs=None):
            if fs is None: fs = []  # avoids a devious bug
            f = lambda x: (x * x + c) % n
            if n % 2 == 0:
                return fact(n // 2, c, fs + [2])
            elif n == 1:
                return fs
            elif is_prime(n):
                return fs + [n]
            else:
                t, h, d = 2, 2, 1
                while d == 1:
                    t, h = f(t), f(f(h))
                    d = gcd(t - h, n)
                if d == n:
                    return fact(n, c + 1, fs)
                elif is_prime(d):
                    return fact(n // d, c + 1, fs + [d])
                else:
                    return fact(n, c + 1, fs)

        return sorted(fact())


if __name__ == "__main__":
    print(factors(2 * 3 * 5 * 7 * -11 * 2))
    print(is_prime(pow(2, 89) - 1))
    print(is_prime(pow(2, 47) - 1))
    print(factors((pow(2, 16) + 1) * (pow(2, 16) - 1)))
