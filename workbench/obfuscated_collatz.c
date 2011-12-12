main(int c,char **v){int n=atoi(v[1]);while(n-1){n=n&1?3*n+1:n/2;printf("%d\n",n);}}
