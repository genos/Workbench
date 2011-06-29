#include <stdio.h>

int main(int argc, char *argv[])
{
    register unsigned short int i;
    for (i = 1; i < 1716; i++) {
        if (i % 13 == 12 && i % 12 == 4 && i % 11 == 10) {
            printf("%u\n", i);
            return 0;
        }
    }
    return 1;
}
