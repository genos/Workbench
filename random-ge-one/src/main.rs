use rand::{rngs::SmallRng, Rng, SeedableRng};
use rayon::prelude::*;

struct Welford {
    x: f64,
    n: f64,
}

fn merge(a: &Welford, b: &Welford) -> Welford {
    let n = a.n + b.n;
    Welford {
        x: a.x + (b.x - a.x) * b.n / n,
        n,
    }
}

fn single_run(mut rng: SmallRng) -> Welford {
    let mut total = 0.0;
    let mut iters = 0.0;
    while total < 1.0 {
        total += rng.gen::<f64>();
        iters += 1.0;
    }
    Welford { x: iters, n: 1.0 }
}

fn n_runs_average(n: u32) -> f64 {
    (0..n)
        .into_par_iter()
        .map(|n| single_run(SmallRng::seed_from_u64(u64::from(n))))
        .reduce_with(|x, y| merge(&x, &y))
        .unwrap_or(Welford { x: 0.0, n: 0.0 })
        .x
}

fn main() {
    let n = std::env::args()
        .nth(1)
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1000);
    println!("{}", n_runs_average(n));
}
