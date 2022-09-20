#!/usr/bin/env python3

# from http://en.literateprograms.org/Jacobi_Symbol_(Python)


def jacobi(a, n):
    """Jacobi Symbol (a / n)"""
    if a == 0:
        return 0
    elif a == 1:
        return 1
    elif a == 2:
        if (n % 8) in set([3, 5]):
            return -1
        else:
            return 1
    elif a % 2 == 0:
        return jacobi(2, n) * jacobi(a // 2, n)
    elif a >= n:
        return jacobi(a % n, n)
    elif a % 4 == n % 4 == 3:
        return -jacobi(n, a)
    else:
        return jacobi(n, a)


if __name__ == "__main__":
    for (a, n) in [(127, 703), (11, 91), (2, 7), (5, 7), (14, 7)]:
        print(jacobi(a, n))
