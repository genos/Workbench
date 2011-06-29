//project euler, quesiton 301
#include <stdio.h>

unsigned int X(unsigned long n1, unsigned long n2, unsigned long n3)
{
    return n1 ^ n2 ^ n3;
}

unsigned int power(unsigned int base, unsigned int exp)
{
    if (exp == 0) {
        return 1;
    }
    else if (exp % 2) {
        return base * power(base, exp - 1);
    }
    else {
        unsigned int tmp = power(base, exp/2);
        return tmp * tmp;
    }
}

int main(int argc, char *argv[])
{
    unsigned long c = 0;
    unsigned long n;
    unsigned long upper_limit = power(2, 30) + 1;
    for (n = 1; n < upper_limit; n++) {
        if (X(n, 2*n, 3*n) == 0) {
            c++;
        }
    }

    printf("%lu\n", c);
    
    return 0;
}
