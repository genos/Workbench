use clap::Parser;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use rayon::prelude::*;

/// Playing with Vishwanath's Constant.
#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// # Of runs to perform
    #[arg(long, short, default_value_t = 1<<20)]
    num_runs: usize,
    /// Chunk size for processing
    #[arg(long, short, default_value_t = 128)]
    chunk_size: usize,
    /// Length of a single run
    #[arg(long, short, default_value_t = 1024)]
    length: u16,
    /// PRNG Seed
    #[arg(long, short, default_value_t = 1729)]
    seed: u64,
}

fn single_run(length: u16, seed: u64) -> f64 {
    let mut rng = SmallRng::seed_from_u64(seed);
    let (mut a, mut b) = (1f64, 1f64);
    for _ in 0..length {
        let t = a + (if rng.gen() { 1.0 } else { -1.0 }) * b;
        a = b;
        b = t;
    }
    a.abs().powf(f64::from(length).recip())
}

fn many_runs(args: &Args) -> (f64, f64) {
    let mut seeds = vec![0u64; args.num_runs];
    SmallRng::seed_from_u64(args.seed).fill(&mut seeds[..]);
    let (n, mu, m2) = seeds
        .into_par_iter()
        .fold_chunks(
            args.chunk_size,
            || (0.0, 0.0, 0.0),
            |(n_old, mu_old, m2_old), seed| {
                let x = single_run(args.length, seed);
                let n = n_old + 1.0;
                let d = x - mu_old;
                let mu = mu_old + d / n;
                let m2 = m2_old + d * (x - mu);
                (n, mu, m2)
            },
        )
        .reduce(
            || (0.0, 0.0, 0.0),
            |(n_a, mu_a, m2_a), (n_b, mu_b, m2_b)| {
                let n = n_a + n_b;
                let d = mu_b - mu_a;
                let mu = mu_a + d * n_b / n;
                let m2 = m2_a + m2_b + d * d * n_a * n_b / n;
                (n, mu, m2)
            },
        );
    (mu, m2 / (n - 1.0))
}

fn main() {
    println!("(mean, var) = {:?}", many_runs(&Args::parse()));
}
