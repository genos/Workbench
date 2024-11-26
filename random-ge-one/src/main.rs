use rand::{rngs::SmallRng, Rng, SeedableRng};
use rayon::prelude::*;

fn single_run(mut rng: SmallRng) -> f64 {
    let mut total = 0.0;
    let mut iters = 0.0;
    while total < 1.0 {
        total += rng.gen::<f64>();
        iters += 1.0;
    }
    iters
}

fn n_runs_average(n: u32) -> f64 {
    (0..n)
        .into_par_iter()
        .map(|n| single_run(SmallRng::seed_from_u64(u64::from(n))))
        .sum::<f64>()
        / f64::from(n)
}

fn main() {
    let n = std::env::args()
        .nth(1)
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1000);
    println!("{}", n_runs_average(n));
}
