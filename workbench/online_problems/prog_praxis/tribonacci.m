format long;

function t = tribo(n)
    a = 0; b = 0; t = 1;
    for i = 2:n
        tmpa = a; tmpb = b; tmpt = t;
        a = b; b = t; t = a + b + t;
    endfor
    return;
endfunction

function t = tripow(n)
    T = [1, 1, 0; 1, 0, 1; 1, 0, 0];
    t = (T^n)(1, 1);
    return;
endfunction

function l = lim(n)
    l = tripow(n) / tripow(n - 1);
    return;
endfunction
