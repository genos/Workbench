# ERROR: ch_A not defined
global ch_A::BigInt = 13591409
global ch_B::BigInt = 545140134
global ch_C::BigInt = 640320
global ch_C3::BigInt = ch_C ^ 3
global ch_D::BigInt = 12

function ch_split(a::BigInt, b::BigInt)
    if b - a == 1
        g::BigInt = (6 * b - 5) * (2 * b - 1) * (6 * b - 1)
        g, div(ch_C3 * b ^ 3, 24), (-1^b) * g * (b * ch_B + ch_A)
    else
        mid = div(a + b, 2)
        g1, p1, q1 = ch_split(a, mid)
        g2, p2, q2 = ch_split(mid, b)
        g1 * g2, p1 * p2, q1 * p2 + q2 * p1
    end
end

function pi_again(digits)
    num_terms::BigInt = iround(2::BigInt + digits / 14.181647462)
    sqrt_C::BigInt = isqrt(ch_C * 100 ^ digits)
    g, p, q = ch_split(0, num_terms)
    div(p * ch_C * sqrt_C, ch_D * (q + p * ch_A))
end

printlin(map(pi_again, 0:10))
