def factors(n):
    fs = []
    while (n & 1 == 0):
        fs.append(2)
        n //= 2
    f = 3
    while n >= f * f:
        if (n % f == 0):
            fs.append(f)
            n //= f
        else:
            f += 2
    if n != 1:
        fs.append(n)
    return fs


if __name__ == "__main__":
    print(factors(600851475143)[-1])
