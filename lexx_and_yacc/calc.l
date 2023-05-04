%{
#include "y.tab.h"
#include <stdlib.h>
void yyerror(char *);
%}

%%

    /* exit */
(exit|quit|bye)         { printf("bye!\n"); exit(0); }
    /* variables */
[a-zA-Z]+[a-zA-Z0-9_]*  { yylval = *yytext - 'a'; return VARIABLE; }
    /* integers */
[0-9]+                  { yylval = atoi(yytext); return INTEGER; }
    /* operators */
[-+()=/*^\n]            { return *yytext; }
    /* skip whitespace */
[ \t]                   ;
    /* anything else is an error */
.                       { yyerror("invalid character"); }

%%

int yywrap(void) {
    return 1;
}
