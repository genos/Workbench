#include <stdio.h>
#include <stdlib.h>

int main() {
    size_t n = 0;

    printf("What size should our square matrix be?\t");
    scanf("%1zu", &n);

    // Build square matrix M
    int **M;
    M = calloc(n, sizeof(int *));
    if (M == NULL) {
        return EXIT_FAILURE; // calloc fail
    }
    for (size_t i = 0; i < n; ++i) {
        M[i] = (int *) calloc(n * n, sizeof(int));
        if (M[i] == NULL) {
            for(size_t j = 0; j < i; ++j) {
                free(M[j]);
                M[j] = NULL;
            }
            free(M);
            M = NULL;
            return EXIT_FAILURE; //  calloc fail
        }
    }

    // Fill in entries
    printf("Let's add entries to the matrix!\n");
    for (size_t i = 0; i < n; i++) {
        for (size_t j = 0; j < n; j++) {
            printf("\nM[%zu, %zu] =\t", i, j);
            scanf("%1d", &M[i][j]);
        }
    }

    // Output matrix
    printf("Here's your matrix:");
    for (size_t i = 0; i < n; ++i) {
        printf("\n");
        for (size_t j = 0; j < n; ++j) {
            printf("%4d", M[i][j]);
        }
    }
    printf("\n");

    // Free memory
    for (size_t i = 0; i < n; ++i) {
        free(M[i]);
        M[i] = NULL;
    }
    free(M);
    M = NULL;
    return EXIT_SUCCESS;
}
