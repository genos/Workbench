#include <stdio.h>
#include <stdlib.h>

int main(int argc, const char *argv[]) {
    int n, i, j, **M;
    n = 3;

    printf("What size should our matrix be?\t");
    scanf("%d", &n);

    // Build square matrix M
    M = malloc(n * sizeof(int *));
    if (M == NULL)
        return 0; // check for malloc fail
    for (i = 0; i < n; i++) {
        M[i] = (int*)malloc(n * n * sizeof(int));
        if (M[i] == NULL)
            return 0; // check for malloc fail
    }

    // Fill in entries
    printf("Let's add entries to the matrix!\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            printf("\nM[%d, %d] =\t", i, j);
            scanf("%d", &M[i][j]); 
        }
    }

    // Output matrix
    printf("Here's your matrix:");
    for (i = 0; i < n; i++) {
        printf("\n");
        for (j = 0; j < n; j++) {
            printf("%4d", M[i][j]);
        }
    }
    printf("\n");
    
    // Free memory
    for (i = 0; i < n; i++) {
        free(M[i]);
    }
    free(M);
    return 0;
}
