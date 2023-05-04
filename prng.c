#include <stdio.h>
#include <stdlib.h>
#define     M       (11 * 19)
#define     A       1729
#define     C       17
#define     N       ((1 << (1 << 4)) - 1)
#define     G       123456789
#define     L       987654321
#define     LOOP    while(i++ < atoi(argv[3]) &&\
                          putchar('0' + (char)((x = p(x)) & 1)));\
                    return putchar('\n') ^ '\n';

typedef int (*prng)(int x);
int bbs(int x) { return (x * x) % M; }
int lcg(int x) { return (((A * x) % N) + C) % N; }
int lehmer(int x) { return (x * G) % L; }

int main(int argc, char *argv[]) {
    prng ps[] = {bbs, lcg, lehmer};
    prng p = ps[atoi(argv[1])];
    int x = atoi(argv[2]), i;
    LOOP
}
