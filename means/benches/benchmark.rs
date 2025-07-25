#![allow(clippy::type_complexity)]
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use rand::{
    SeedableRng,
    distr::{Distribution, StandardUniform},
    rngs::StdRng,
};
use std::hint::black_box;

fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("Means");
    let mut rng = StdRng::seed_from_u64(8_675_309);
    let names_functions: Vec<(&str, fn(&[f32]) -> f32)> = vec![
        ("Seq", means::seq),
        ("Par", means::par),
        ("SeqSimd8", means::seq_simd::<8>),
        ("SeqSimd16", means::seq_simd::<16>),
        ("SeqSimd32", means::seq_simd::<32>),
        ("SeqSimd64", means::seq_simd::<64>),
        ("ParSimd8", means::par_simd::<8>),
        ("ParSimd16", means::par_simd::<16>),
        ("ParSimd32", means::par_simd::<32>),
        ("ParSimd64", means::par_simd::<64>),
    ];
    for e in 1..10 {
        let i = 10usize.pow(e);
        let xs = StandardUniform
            .sample_iter(&mut rng)
            .take(i)
            .collect::<Vec<f32>>();
        for (name, function) in &names_functions {
            group.bench_with_input(BenchmarkId::new(*name, i), &i, |b, _i| {
                b.iter(|| function(black_box(&xs)));
            });
        }
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
