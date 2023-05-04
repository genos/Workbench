%{
size_t count = 0;
%}

%%

^(.*)\n     printf("%4d\t%s", count++, yytext);

%%

int main(int argc, char *argv[]) {
    yyin = fopen(argv[1], "r");
    yylex();
    fclose(yyin);
}
