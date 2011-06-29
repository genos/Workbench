def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def divides(k, n):
    if n % k == 0:
        return True
    else:
        return False

def coprime(k, n):
    if gcd(k, n) == 1:
        return True
    else:
        return False

def divisors(n):
    return [k for k in xrange(1, n + 1) if divides(k, n)]

def sum_divs(n):
    return sum(divisors(n))

def num_divs(n):
    return len(divisors(n))

def totatives(n):
    return [k for k in xrange(1, n) if coprime(k, n)]

def totient(n):
    return len(totatives(n))
