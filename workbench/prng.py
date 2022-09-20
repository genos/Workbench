#!/usr/bin/env python3
"""
Pseudorandom Number Generators and Probabilistic Encryption in Python (originally 2.4)

        Graham Enos, Spring 2010
"""


def midsqr(x_0):
    """
    Middle Square Generator; see Mills 2003

    Input:
    x_0         integer in Z_1e5

    Example:
    >>> my_midsqr = midsqr(1010)
    >>> for i in range(3):
    ...     print(my_mid_sqr.next(), end=" ")
    2010 4010 8010
    """

    x = x_0  # Initialize x as x_0

    while True:
        x = (x**2 / 10) % (10**4)  # (x^2/10) (mod 10^4)
        yield x


def fib(m, x_0, x_1):
    """
    Fibonacci Generator; see Mills 2003

    Input:
    m           integer >= 2
    x_0, x_1    seeds, integers in Z_m

    Example:
    >>> my_fib = fib(10,1,1)
    >>> for i in range(10):
    ...     print(my_fib.next(), end=" ")
    ...
    2 3 5 8 3 1 4 5 9 4
    """

    y_0 = x_0
    y_1 = x_1  # Initialize y_0 and y_1 as x_0 and x_1

    while True:
        temp_y = y_1
        y_1 = (y_0 + y_1) % m
        y_0 = temp_y
        yield y_1


def lcg(m, a, b, x_0):
    """
    Linear Congruential Generator, Algorithm 8.1 in Stinson 2006

    Input:
    m           integer >= 2
    a,b         integers in Z_m
    x_0         seed, integer in Z_m

    Output:
    ones and zeros

    Example:
    >>> my_lcg = lcg(31, 3, 5, 17)
    >>> for i in range(10):
    ...     print my_lcg.next(),
    ...
    1 0 0 1 0 1 1 0 1 0
    """

    x = x_0  # Initialize x as x_0

    while True:
        x = (a * x + b) % m  # ax + b (mod m)
        yield x % 2


def rsa(n, b, x_0):
    """
    RSA Generator, Algorithm 8.2 in Stinson 2006

    Input:
    n           composite n = p * q; p & q prime
    b           integer such that gcd(b, phi(n)) = 1
    x_0         seed, any element of Z_n*

    Output:
    ones and zeros

    Example:
    >>> my_rsa = rsa(91261, 1547, 75634)
    >>> for i in range(20):
    ...     print my_rsa.next(),
    ...
    1 0 0 0 0 1 1 1 0 1 1 1 1 0 0 1 1 0 0 0
    """

    x = x_0  # Intialize x as x_0

    while True:
        x = pow(x, b, n)  # x^b (mod n)
        yield x % 2


def dlg(p, a, x_0):
    """
    Discrete Logarithm Generator, Algorithm 8.8 in Stinson 2006

    Input:
    p           a prime
    a           primitive element mod p (has multiplicative order p-1)
    x_0         seed, any nonzero element of Z_p

    Output:
    ones and zeros

    Example:
    >>> my_dlg = dlg(21383, 5, 15886)
    >>> for i in range(50):
    ...     print my_dlg.next(),
    ...
    0 0 1 1 1 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 1 0 0 1 0 0 1 1
    1 0 0 0 1 0 1 0 0 0 0 1
    """

    x = x_0  # Initialize x as x_0
    half_p = p / 2.0

    while True:
        x = pow(a, x, p)  # a^x (mod p)
        if x > half_p:
            yield 1
        else:
            yield 0


def bbs(n, x_0):
    """
    Blum-Blum-Shub Generator, Algorithm 8.5 in Stinson 2006

    Input:
    n           composite n = p*q; p & q are primes = 3 mod 4
    x_0         seed, quadratic residue mod n

    Output:
    ones and zeros

    Example:
    >>> my_bbs = bbs(209, 3)
    >>> for i in range(50):
    ...     print my_bbs.next(),
    ...
    1 1 0 0 0 0 0 1 0 1 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 1
    0 0 0 0 0 1 0 1 1 0 1 1
    """

    x = x_0  # Initialize x as x_0

    while True:
        x = pow(x, 2, n)  # x^2 (mod n)
        yield x % 2


def bg_enc(n, r, x):
    """
    Blum-Goldwasser Encryption

    Requires:
    bbs()       BBS PRNG

    Input:
    n           composite n = p*q; p & q are primes = 3 mod 4
    r           seed, quadratic residue mod n
    x           tuple of plaintext bits

    Output:
    ciphertext, a tuple of ones and zeros

    Example
    >>> bg_enc(272953, 159201, (1, 1, 0, 1, 0))
    ((0, 1, 1, 1, 0), 139680)

    """

    my_bbs = bbs(n, r)
    k = len(x)
    z = ()

    for i in range(k):
        z += (my_bbs.next(),)  # Grow keystream

    s = pow(r, 2 ** (k + 1), n)  # r^(2^(k+1)) mod n
    y = tuple(a ^ b for (a, b) in zip(x, z))  # x XOR z

    return (y, s)


def bg_dec(p, q, y, s):
    """
    Blum-Goldwasser Decryption

    Requires:
    bbs()       BBS PRNG
    euclid      Extended Euclidean Algorithm

    Input:
    p, q        primes = 3 mod 4
    y           tuple of ciphertext bits
    s           hidden seed, quadratic residue mod p*q

    Output:
    plaintext, a tuple of ones and zeros

    Example:
    >>> bg_dec(499, 547, (0, 1, 1, 1, 0), 139680)
    (0, 0, 1, 1, 0)
    """

    n = p * q
    k = len(y)
    (a, b) = euclid(p, q)[:2]  # Integers such that ap + bq = 1

    d1 = pow((p + 1) / 4, k + 1, p - 1)  # ((p+1)/4)^(k+1) mod (p-1)
    d2 = pow((q + 1) / 4, k + 1, q - 1)  # ((q+1)/4)^(k+1) mod (q-1)
    u = pow(s, d1, p)  # s^d1 mod p
    v = pow(s, d2, q)  # s^d2 mod q
    r = (v * a * p + u * b * q) % n  # vap + ubq mod n

    my_bbs = bbs(n, r)
    z = ()

    for i in range(k):
        z += (my_bbs.next(),)  # Grow keystream

    x = tuple(a ^ b for (a, b) in zip(y, z))  # y XOR z

    return x


def euclid(a, b):
    """
    Extended Euclidean Algorithm; finds (x,y,z) such that ax + by = gcd(a,b) = z

    Written with help from
    http://en.literateprograms.org/Extended_Euclidean_algorithm_%28Python%29

    Input:
    (a,b)       two integers

    Output:
    (x, y, z)   integers such that ax + by = gcd(a,b) = z

    Example:
    >>> euclid(12345,54321)
    (3617, -822, 3)
    """

    (x1, x2, x3) = (1, 0, a)
    (y1, y2, y3) = (0, 1, b)

    while y3 != 0:
        quotient = x3 / y3

        tmp1 = x1 - quotient * y1
        tmp2 = x2 - quotient * y2
        tmp3 = x3 - quotient * y3

        (x1, x2, x3) = (y1, y2, y3)
        (y1, y2, y3) = (tmp1, tmp2, tmp3)

    return x1, x2, x3
