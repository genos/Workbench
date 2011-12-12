function C = nate2(n)
A = repmat(1:n, n, 1);
C = isprime(A + A') .* max(A, A') + not(isprime(A + A')) .* min(A, A');
