/// `<https://raganwald.com/2020/05/03/fractran.html>`
use rug::{ops::Pow, Integer, Rational};
use std::sync::LazyLock;

fn log2(n: &Integer) -> Option<u32> {
    if n.is_power_of_two() {
        n.find_one(0)
    } else {
        None
    }
}

struct FTIter<'a> {
    n: Rational,
    fs: &'a [Rational],
}

impl Iterator for FTIter<'_> {
    type Item = Integer;
    fn next(&mut self) -> Option<Self::Item> {
        for f in self.fs {
            let nf = self.n.clone() * f;
            if nf.is_integer() {
                self.n = nf;
                return Some(self.n.numer().clone());
            }
        }
        None
    }
}

fn eval(n: Rational, fs: &[Rational]) -> impl Iterator<Item = Integer> {
    FTIter { n, fs }
}

fn fib(i: u32) -> u32 {
    static FS: LazyLock<[Rational; 15]> = LazyLock::new(|| {
        [
            "17/65", "133/34", "17/19", "23/17", "2233/69", "23/29", "31/23", "74/341", "31/37",
            "41/31", "129/287", "41/43", "13/41", "1/13", "1/3",
        ]
        .map(|s| s.parse().unwrap())
    });
    eval(Rational::from(78u32 * Integer::from(5u32).pow(i - 1)), &*FS)
        .last()
        .and_then(|n| log2(&n))
        .unwrap()
}

fn collatz(i: u32) -> impl Iterator<Item = u32> {
    static FS: LazyLock<[Rational; 12]> = LazyLock::new(|| {
        [
            "165/14", "11/63", "38/21", "13/7", "34/325", "1/13", "184/95", "1/19", "7/11",
            "13/17", "19/23", "1575/4",
        ]
        .map(|s| s.parse().unwrap())
    });
    eval(Rational::from(Integer::from(1) << i), &*FS).filter_map(|n| log2(&n))
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
