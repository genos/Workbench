function tf = nate1b(A, i)
% returns true or false depending on whether player i is strong in
% tournament matrix A
tf = false;
[n n] = size(A);
opps = A(i,:);
if sum(opps) == n - 1
  tf = true;  % i beat everyone but i
else
  tobeat = find(opps < 1);  % places where i didn't win
  beat = find(opps == 1);  % places where i did win
  wins = 0;
  for t = 1:length(tobeat)
    if tobeat(t) != i  % don't test A(i, i)
      j = tobeat(t);
      for b = 1:length(beat)
        k = beat(b);
        if (A(i, k) == 1) && (A(k, j)) == 1  % or A(i,k)+A(k,j)==2
          wins++;  % i beat k and k beat j
          break
        end
      end
    end
  end
  tf = (wins == t - 1);
end
