/// `<https://arxiv.org/abs/2501.04032>`
use clap::Parser;
use rayon::prelude::*;

fn calculate_stopping_time(n: u128) -> (u128, u128) {
    let (mut k, mut s) = (n, 0);
    while k > 1 {
        if (k & 1) == 1 {
            k = 3 * k + 1;
            let z = u128::from(k.trailing_zeros());
            s += z + 1;
            k >>= z;
        } else {
            let z = u128::from(k.trailing_zeros());
            s += z;
            k >>= z;
        }
    }
    (n, s)
}

/// Playing with Collatz
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Lower bound (inclusive) power of 2 to search
    #[arg(long, short, default_value_t = 1)]
    lower_power_of_2: u128,
    /// Upper bound (inclusive) power of 2 to search
    #[arg(long, short, default_value_t = 10)]
    upper_power_of_2: u128,
}

fn main() {
    let args = Args::parse();
    let lb = 1u128 << args.lower_power_of_2;
    let ub = 1u128 << args.upper_power_of_2;
    if lb > ub {
        println!("Lower bound must be ≤ upper bound.");
        std::process::exit(1);
    }
    let (n, s) = (lb..=ub)
        .into_par_iter()
        .map(calculate_stopping_time)
        .max_by_key(|p| p.1)
        .expect("parsing/powering u128");
    println!("Max stopping time = {s} for n = {n}");
}
