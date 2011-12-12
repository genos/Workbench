function tf = nate1a(A)
% Property 1
set = [0 1/2 1];
if not(all(ismember(A, set)))
  tf = false;
% Property 2
elseif norm(diag(A), Inf) != 0
  tf = false;
% Property 3
elseif not(all(A + A' + eye(size(A))))
  tf = false;
else
  tf = true;
end