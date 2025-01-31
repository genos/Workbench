/*
 * sieve.c
 *
 * Prints out primes from 1 to N, one per line.
 *
 * Adaptation/sizeable clean up of the Sieve of Eratosthenes I found online at
 * http://www.cs.ucr.edu/~ciardo/teaching/CS237/source/sieve.c
 *
 * GRE, 1/20/10
 */

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define N 2000000                             /* Limited only by memory size */
#define SQRTN ((long) sqrt(N))

int main(void) {
    bool prime[N + 1];                            /* n is prime <=> prime[n] */
    long n;                                      /* Index of possible primes */
    long s;                                                    /* Step index */
                                                     /* Initialize the sieve */
    prime[0] = false;
    prime[1] = false;
    memset(prime + 2, true, N - 2);
    for (n = 2; n <= SQRTN; n++) {               /* Search all possibilities */
        if (prime[n]) {                                 /* If n is prime,... */
            for (s = 2; s <= (N / n); s++) {
                                           /* ...sn can't be prime for any s */
                prime[s * n] = false;
            }
        }
    }
                                                                   /* Output */
    for (n = 2; n <= N; n++) {
        if (prime[n])  {
            printf("%7ld\n", n);
        }
    }
  return 0;
}
