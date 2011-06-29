#!/usr/bin/env python

def check(n):
    num = n
    while num > 0:
        if (num % 10) not in xrange(3): return False
        num //= 10
    return True

def f(n):
    num = n
    while not check(num):
        num += n
    return num

if __name__ == '__main__':
    print sum((f(n)/n for n in xrange(1,10001)))
