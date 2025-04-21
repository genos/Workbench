/// `<https://raganwald.com/2020/05/03/fractran.html>`
use num::{BigUint, One, Zero, rational::Ratio};
use std::sync::LazyLock;

fn log2(n: &BigUint) -> Option<u64> {
    if n.is_zero() || n.count_ones() > 1 {
        None
    } else {
        Some(n.bits() - 1)
    }
}

struct FTIter<'a> {
    n: Ratio<BigUint>,
    fs: &'a [Ratio<BigUint>],
}

impl Iterator for FTIter<'_> {
    type Item = BigUint;
    fn next(&mut self) -> Option<Self::Item> {
        for f in self.fs {
            let nf = &self.n * f;
            if nf.is_integer() {
                self.n = nf;
                return Some(self.n.to_integer());
            }
        }
        None
    }
}

fn eval(n: Ratio<BigUint>, fs: &[Ratio<BigUint>]) -> impl Iterator<Item = BigUint> {
    FTIter { n, fs }
}

fn fib(i: u32) -> u64 {
    static FS: LazyLock<[Ratio<BigUint>; 15]> = LazyLock::new(|| {
        [
            "17/65", "133/34", "17/19", "23/17", "2233/69", "23/29", "31/23", "74/341", "31/37",
            "41/31", "129/287", "41/43", "13/41", "1/13", "1/3",
        ]
        .map(|s| s.parse().unwrap())
    });
    eval(Ratio::from(78u32 * BigUint::from(5u32).pow(i - 1)), &*FS)
        .last()
        .and_then(|n| log2(&n))
        .unwrap()
}

fn collatz(i: u32) -> impl Iterator<Item = u64> {
    static FS: LazyLock<[Ratio<BigUint>; 12]> = LazyLock::new(|| {
        [
            "165/14", "11/63", "38/21", "13/7", "34/325", "1/13", "184/95", "1/19", "7/11",
            "13/17", "19/23", "1575/4",
        ]
        .map(|s| s.parse().unwrap())
    });
    eval(Ratio::from(BigUint::one() << i), &*FS).filter_map(|n| log2(&n))
}

fn main() {
    for i in 1..=10 {
        println!("Fib({}) = {}", i, fib(i));
    }
    println!("Collatz sequence for 7:");
    for c in collatz(7) {
        println!("{c}");
    }
}
