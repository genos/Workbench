#![feature(portable_simd)]
use rayon::prelude::*;
use std::simd::{LaneCount, Simd, SupportedLaneCount};

#[must_use]
pub fn seq(xs: &[f32]) -> f32 {
    let (mut n, mut mu) = (0.0, 0.0);
    for x in xs {
        n += 1.0;
        mu += (x - mu) / n;
    }
    mu
}

#[must_use]
pub fn par(xs: &[f32]) -> f32 {
    xs.into_par_iter()
        .fold(
            || (0.0, 0.0),
            |(n_old, mu_old), x| {
                let n = n_old + 1.0;
                let mu = mu_old + (x - mu_old) / n;
                (n, mu)
            },
        )
        .reduce(
            || (0.0, 0.0),
            |(n_a, mu_a), (n_b, mu_b)| {
                let n = n_a + n_b;
                let mu = mu_a + (mu_b - mu_a) * n_b / n;
                (n, mu)
            },
        )
        .1
}

#[must_use]
pub fn seq_simd<const N: usize>(xs: &[f32]) -> f32
where
    LaneCount<N>: SupportedLaneCount,
{
    let chunks = xs.len() / N;
    let left_over = xs.len() - (xs.len() % N);
    let (mut ns, mut mus) = (Simd::splat(0.0), Simd::splat(0.0));
    for i in 0..chunks {
        ns += Simd::splat(1.0);
        let ys = Simd::from_slice(&xs[i * N..(i + 1) * N]);
        mus += (ys - mus) / ns;
    }
    let (mut n, mut mu) = (0.0, 0.0);
    for i in 0..N {
        n += ns[i];
        mu += (mus[i] - mu) * ns[i] / n;
    }
    for x in xs.iter().skip(left_over) {
        n += 1.0;
        mu += (x - mu) / n;
    }
    mu
}

#[must_use]
pub fn par_simd<const N: usize>(xs: &[f32]) -> f32
where
    LaneCount<N>: SupportedLaneCount,
{
    let chunks = xs.len() / N;
    let left_over = xs.len() - (xs.len() % N);
    let (ns, mus) = (0..chunks)
        .into_par_iter()
        .fold(
            || (Simd::splat(0.0), Simd::splat(0.0)),
            |(ns_old, mus_old), i| {
                let ns = ns_old + Simd::splat(1.0);
                let ys = Simd::from_slice(&xs[i * N..(i + 1) * N]);
                let mu = mus_old + (ys - mus_old) / ns;
                (ns, mu)
            },
        )
        .reduce(
            || (Simd::splat(0.0), Simd::splat(0.0)),
            |(ns_a, mus_a), (ns_b, mus_b)| {
                let ns = ns_a + ns_b;
                let mus = mus_a + (mus_b - mus_a) * ns_b / ns;
                (ns, mus)
            },
        );
    let (mut n, mut mu) = (0.0, 0.0);
    for i in 0..N {
        n += ns[i];
        mu += (mus[i] - mu) * ns[i] / n;
    }
    for x in xs.iter().skip(left_over) {
        n += 1.0;
        mu += (x - mu) / n;
    }
    mu
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    #[test]
    fn its_1_point_5() {
        for mean in [
            seq,
            par,
            seq_simd::<1>,
            seq_simd::<2>,
            par_simd::<1>,
            par_simd::<2>,
        ] {
            assert!((mean(&[1.0, 2.0]) - 1.5).abs() < 1e-16);
        }
    }
    #[test]
    fn its_3() {
        for mean in [
            seq,
            par,
            seq_simd::<1>,
            seq_simd::<2>,
            seq_simd::<3>,
            seq_simd::<4>,
            par_simd::<1>,
            par_simd::<2>,
            par_simd::<3>,
            par_simd::<4>,
        ] {
            assert!((mean(&[1.0, 2.0, 3.0, 4.0, 5.0]) - 3.0).abs() < 1e-16);
        }
    }
    proptest! {
        #[test]
        fn equiv(xs in prop::collection::vec(-100f32..100f32, 100..1_000)) {
            prop_assert!((seq(&xs) - par(&xs)).abs() < 1e-5);
            prop_assert!((par(&xs) - seq_simd::<16>(&xs)).abs() < 1e-5);
            prop_assert!((seq_simd::<16>(&xs) - par_simd::<16>(&xs)).abs() < 1e-5);
        }
    }
}
