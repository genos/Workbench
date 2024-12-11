#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
use clap::{Parser, ValueEnum};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::fmt;

mod basic;
mod branch;
mod flat;
mod stack;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Method {
    Basic,
    Branch,
    Flat,
    Stack,
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Basic => "basic",
            Self::Branch => "branch",
            Self::Flat => "flat",
            Self::Stack => "stack",
        })
    }
}

impl Method {
    fn run(self, rng: &mut impl Rng, depth: u32, x: f64, y: f64) -> f64 {
        match self {
            Self::Basic => basic::interpret(&basic::build(rng, depth), x, y),
            Self::Branch => branch::interpret(&branch::build(rng, depth), x, y),
            Self::Flat => flat::interpret(&flat::build(rng, depth), x, y),
            Self::Stack => stack::interpret(&stack::build(rng, depth), x, y),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use paste::paste;
    use proptest::prelude::*;

    macro_rules! test_equiv {
        ($($m1:ident <=> $m2:ident),+) => {
            $(
                paste! {
                    proptest! {
                        #[test]
                        fn [<$m1:lower _equiv_ $m2:lower>](
                            seed: u64, depth in 0u32..=20, x in 0.0..=1.0, y in 0.0..=1.0
                        ) {
                            let rng = SmallRng::seed_from_u64(seed);
                            let z1 = crate::Method::$m1.run(&mut rng.clone(), depth, x, y);
                            let z2 = crate::Method::$m2.run(&mut rng.clone(), depth, x, y);
                            prop_assert!((z1 - z2).abs() < f64::EPSILON);
                        }
                    }
                }
            )+
        }
    }

    test_equiv![Stack <=> Flat, Stack <=> Branch, Stack <=> Basic];
}

#[derive(Parser)]
struct Args {
    /// Method to use
    #[arg(short, long, default_value_t = Method::Basic)]
    method: Method,
    /// Seed for PRNG
    #[arg(short, long, default_value_t = 1729)]
    seed: u64,
    /// Max recursion depth for building the picture
    #[arg(short, long, default_value_t = 26)]
    depth: u32,
}

fn main() {
    let args = Args::parse();
    let mut rng = SmallRng::seed_from_u64(args.seed);
    let x = rng.gen();
    let y = rng.gen();
    println!("{}", args.method.run(&mut rng, args.depth, x, y));
}
