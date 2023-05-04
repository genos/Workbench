%{
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
int yylex(void);
void yyerror(char *);
intmax_t sym[1000];
intmax_t expt(intmax_t, intmax_t);
%}

%token INTEGER VARIABLE
%left '+' '-'
%left '*' '/'
%right '^'
%nonassoc UMINUS

%%

program:
       program statement '\n'  
       |
       ;

statement:
         expr                   { printf("%" PRIdMAX "\n", $1); }
         |  VARIABLE '=' expr   { sym[$1] = $3; }

expr:
    INTEGER                     { $$ = $1; }
    |   VARIABLE                { $$ = sym[$1]; }
    |   expr '+' expr           { $$ = $1 + $3; }
    |   expr '-' expr           { $$ = $1 - $3; }
    |   expr '*' expr           { $$ = $1 * $3; }
    |   expr '/' expr           { $$ = $1 / $3; }
    |   expr '^' expr           { $$ = expt($1, $3);}
    |   '-' expr %prec UMINUS   { $$ = (-1) * $2; }
    |   '(' expr ')'            { $$ = $2; }
    ;

%%

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}

intmax_t expt(intmax_t base, intmax_t power) {
    if (power <= 0) {
        return 1;
    } else if (power == 1) {
        return base;
    } else {
        intmax_t tmp = expt(base, power >> 1);
        tmp *= tmp;
        if (power & 1) {
            tmp *= base;
        }
        return tmp;
    }
}

int main(void) {
    yyparse();
    return 0;
}
