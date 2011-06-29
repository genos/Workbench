#!/usr/bin/env python

def num_digits(k):
    c = 0
    while k > 0:
        c += 1
        k /= 10
    return c

def is_kaprekar(k):
    k2 = pow(k, 2)
    p10ndk = pow(10, num_digits(k))
    if k == (k2 // p10ndk) + (k2 % p10ndk):
        return True
    else:
        return False

if __name__ == '__main__':
    for k in range(1, 1001):
        if is_kaprekar(k): print(k)
