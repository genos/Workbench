# project euler, question 55

import math

def reverse(n):
    rev = 0
    while n > 0:
        rev = (10 * rev) + (n % 10)
        n //= 10
    return rev

def is_pal(n):
    if n in range(10):
        return True
    len_n = 1 + int(math.log10(n))
    tplnm1 = pow(10, len_n-1)
    if len_n == 2:
        return (n % 10) == (n // tplnm1)
    else:
        return ((n % 10) == (n // tplnm1) and (is_pal((n  % tplnm1)//10)))

def is_lychrel(n):
    c = 0
    n2 = n
    while c < 50:
        n2 = n2 + reverse(n2)
        if is_pal(n2):
            return False
        else:
            c += 1
            continue
    return True

def maine():
    count = 0
    for i in range(int(1e4)):
        if is_lychrel(i): count += 1
    print(count)
    return

if __name__ == '__main__':
    maine()
