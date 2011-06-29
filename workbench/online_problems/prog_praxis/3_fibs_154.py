def fib_exp(n):
    if n < 2:
        return 1
    else:
        return fib_exp(n-1) + fib_exp(n-2)

def fib_lin(n):
    c = n
    a, b = 1, 1
    while c > 0:
        a, b, c = b, a + b, c - 1
    return a

def matrix_mul(a, b):
    return [[a[0][0]*b[0][0]+a[0][1]*b[1][0], a[0][0]*b[0][1]+a[0][1]*b[1][1]],
            [a[1][0]*b[0][0]+a[1][1]*b[1][0], a[1][0]*b[0][1]+a[1][1]*b[1][1]]]

def matrix_pow(a, n):
    if n == 1:
        return a
    elif (n % 2):
        return matrix_mul(a, matrix_pow(matrix_mul(a, a), n/2))
    else:
        return matrix_pow(matrix_mul(a, a), n/2)

def fib_log(n):
    a = [[1, 1], [1,0]]
    return matrix_pow(a, n)[1][0]
