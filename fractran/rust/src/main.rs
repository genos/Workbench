/// `<https://raganwald.com/2020/05/03/fractran.html>`
use rug::{
    ops::{MulFrom, Pow},
    Integer, Rational,
};

fn log2(n: &Integer) -> Option<u32> {
    if n.is_power_of_two() {
        n.find_one(0)
    } else {
        None
    }
}

struct Fractran<'a> {
    n: Integer,
    fs: &'a [Rational],
}

impl Iterator for Fractran<'_> {
    type Item = Integer;
    fn next(&mut self) -> Option<Self::Item> {
        for f in self.fs {
            if self.n.is_divisible(f.denom()) {
                self.n.div_exact_mut(f.denom());
                self.n.mul_from(f.numer());
                return Some(self.n.clone());
            }
        }
        None
    }
}

fn fib(i: u32) -> u32 {
    Fractran {
        n: 78 * Integer::from(5).pow(i - 1),
        fs: &[
            Rational::from((17, 65)),
            Rational::from((133, 34)),
            Rational::from((17, 19)),
            Rational::from((23, 17)),
            Rational::from((2233, 69)),
            Rational::from((23, 29)),
            Rational::from((31, 23)),
            Rational::from((74, 341)),
            Rational::from((31, 37)),
            Rational::from((41, 31)),
            Rational::from((129, 287)),
            Rational::from((41, 43)),
            Rational::from((13, 41)),
            Rational::from((1, 13)),
            Rational::from((1, 3)),
        ],
    }
    .last()
    .and_then(|n| log2(&n))
    .unwrap()
}

fn collatz(i: u32) -> Vec<u32> {
    Fractran {
        n: Integer::from(1) << i,
        fs: &[
            Rational::from((165, 14)),
            Rational::from((11, 63)),
            Rational::from((38, 21)),
            Rational::from((13, 7)),
            Rational::from((34, 325)),
            Rational::from((1, 13)),
            Rational::from((184, 95)),
            Rational::from((1, 19)),
            Rational::from((7, 11)),
            Rational::from((13, 17)),
            Rational::from((19, 23)),
            Rational::from((1575, 4)),
        ],
    }
    .filter_map(|n| log2(&n))
    .collect()
}

fn main() {
    for i in 1..=15 {
        println!("Fib({}) = {}", i, fib(i));
    }
    println!("Collatz sequence for 7:");
    for c in collatz(7) {
        println!("{c}");
    }
}
