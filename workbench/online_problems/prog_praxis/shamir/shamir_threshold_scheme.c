/* shamir_threshold_scheme.c
 *
 * My C implementation of Shamir's (k, n) Theshold scheme.
 * GRE, 6/23/11
 */

#include <stdio.h>
#include <stdlib.h>
#define uint unsigned int
#define ulong unsigned long
#define ushort unsigned short

// point struct to hold (x, y) values
typedef struct {
    ulong x, y;
} point;


// Modular exponentiation, via Wikipedia and Schneier's "Applied Cryptography"
ulong expt_mod(ulong b, ulong e, ulong m){
    ulong r = 1;
    while (e) {
        if (e & 1) {
            r = (r * b) % m;
        }
        e >>= 1;
        b = (b * b) % m;
    }
    return r;
}


// Modular inverse; x^{phi(p) - 1} = 1 mod p, and p prime => phi(p) = p - 1
ulong mod_inv(ulong x, ulong p){
    return expt_mod(x, p - 2, p);
}


// Horner's method to evaluate polynomial given by coeffs[] modulo mod
ulong horner_mod(ulong x, ulong coeffs[], ulong k, ulong mod){
    uint i;
    ulong y = 0;

    for (i = k - 1; i > 0; i--) {
        y = (y + coeffs[i]) % mod;
        y = (y * x) % mod;
    }
    y = (y + coeffs[0]) % mod;

    return y;
}


// Shamir (k, n) threshold scheme
void shamir_threshold(ulong s, ulong k, ulong n, ulong p, point xy_pairs[]){
    uint i;
    ushort dup_flag[n];
    ulong x, coeffs[k];

    for (i = 0; i < n; i++) {
        dup_flag[i] = 0;
    }


    coeffs[0] = s;
    for (i = 1; i < k; i++) {
        coeffs[i] = (ulong)(1 + (rand() % (p - 2)));
    }

    for (i = 0; i < n; i++) {
        do {
            x = (ulong)(1 + (rand() % (p - 2)));
        }
        while (dup_flag[x]);

        dup_flag[x] = 1;
        xy_pairs[i].x = x;
        xy_pairs[i].y = horner_mod(x, coeffs, k, p);
    }
}


// Lagrange interpolation to recover the constant term
ulong interp_const(point xy_pairs[], ulong k, ulong p){
    uint i, j;
    ulong c;
    ulong s = 0;

    for (i = 0; i < k; i++) {
        c = 1;
        for (j = 0; j < k; j++) {
            if (xy_pairs[j].x < xy_pairs[i].x) {
                c = (c * xy_pairs[j].x * mod_inv(p - (xy_pairs[i].x -
                                xy_pairs[j].x), p)) % p;
            }
            else if (xy_pairs[j].x > xy_pairs[i].x) {
                c = (c * xy_pairs[j].x * mod_inv(xy_pairs[j].x - xy_pairs[i].x,
                            p)) % p;
            }
            else {
                continue;
            }
        }
        s = (s + xy_pairs[i].y * c) % p;
    }
    return s;
}


// The main event (fingers crossed!)
int main(int argc, char *argv[]){
    uint i;
    ulong s = 17;
    ulong n = 20;
    ulong k = 5;
    ulong p = 23;
    point xy_pairs[n];

    if (argc > 1) {
        srand((uint)atoi(argv[1]));
    }
    
    printf("%lu\n", s);
    shamir_threshold(s, k, n, p, xy_pairs);
    for (i = 0; i < n; i++) {
        printf("%lu\t%lu\n", xy_pairs[i].x, xy_pairs[i].y);
    }
    printf("%lu\n", interp_const(xy_pairs, k, p));

    return 0;
}
