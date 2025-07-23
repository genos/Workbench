use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use rand::{
    SeedableRng,
    distr::{Distribution, StandardUniform},
    rngs::StdRng,
};
use std::hint::black_box;

type Mean = fn(&[f32]) -> f32;

fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("Means");
    let mut rng = StdRng::seed_from_u64(8_675_309);
    let names_means: Vec<(&str, Mean)> = vec![
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
    for e in 4..=8 {
        let i = 10usize.pow(e);
        let xs = StandardUniform
            .sample_iter(&mut rng)
            .take(i)
            .collect::<Vec<f32>>();
        for (name, mean) in &names_means {
            group.bench_with_input(BenchmarkId::new(*name, i), &i, |b, _i| {
                b.iter(|| mean(black_box(&xs)));
            });
        }
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
