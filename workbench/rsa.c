#include <stdio.h>
#include <stdlib.h>
#define E 1547
#define M 91261
#define X 75634
#define ulong unsigned long

// Modular exponentiation, via Wikipedia and Schneier's "Applied Cryptography"
inline ulong expt_mod(ulong b, ulong e, ulong m){
    register ulong r = 1;
    while (e) {
        if (e & 1) r = (r * b) % m;
        e >>= 1;
        b = (b * b) % m;
    }
    return r;
}

int main(int argc, char *argv[]) {
    register ulong x = argc > 1 ? atol(argv[1]) : X;
    register int i = argc > 2 ? atoi(argv[2]) : 10;
    while (i--) printf("%lu", (x = expt_mod(x, E, M)) % 2);
    printf("\n");
    return 0;
}
