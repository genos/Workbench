n,k=map(int,raw_input().split(','))
if k in [0,n]:print 1
else:
 a,b=1,1
 for j in range(1,n-k+1):
  a*=k+j;b*=j
 print a/b
