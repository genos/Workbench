#!/usr/bin/env python


def blum_micali(p, g, x0):
    """Blum-Micali CSPRNG, from
    http://en.wikipedia.org/wiki/Blum-Micali_algorithm
    p:    odd prime
    g:    primitive root mod p
    x0:   seed in Z_p
    At each iteration, this generator computes x_(i + 1) = g^(x_i) mod p, then
    yields 1 if x_(i+1) < (p - 1) / 2, or 0 otherwise.
    """
    x = x0
    cutoff = p >> 1
    while True:
        x = pow(g, x, p)
        yield 1 if x < cutoff else 0


if __name__ == "__main__":
    # A toy example, to be sure
    p, g, x0 = 17, 10, 7
    bm = blum_micali(p, g, x0)
    print([next(bm) for _ in range(20)])
