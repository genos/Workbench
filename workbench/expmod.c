#include <stdio.h>
#include <gmp.h>

int main(const int argc, const char **argv) {
    mpz_t x, y, z, o;
    mpz_init_set_str(x, "17", 10);
    mpz_init_set_ui(y, 1729);
    mpz_init_set_ui(z, 7);
    mpz_init(o);

    mpz_powm(o, x, y, z);

    gmp_printf("x = %Zd\ny = %Zd\nz = %Zd\n", x, y, z);
    gmp_printf("x^y (mod z) = %Zd\n", o);

    mpz_clears(x, y, z, o);
    return 0;
}
