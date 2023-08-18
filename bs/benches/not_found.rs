use bs::binary_search;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use rand::Rng;

fn setup(size: usize) -> (u32, Vec<u32>) {
    let mut rng = rand::thread_rng();
    let mut xs = (0..size).map(|_| rng.gen()).collect::<Vec<_>>();
    xs.sort();
    loop {
        let n = rng.gen::<u32>();
        if !xs.contains(&n) {
            return (n, xs);
        }
    }
}

fn not_found(c: &mut Criterion) {
    let mut group = c.benchmark_group("not found");
    for size in [1, 2, 4, 8, 16, 32, 64, 128] {
        let (n, xs) = setup(size);
        let ys = xs.clone();
        let zs = ys.clone();
        group.bench_with_input(
            BenchmarkId::new("our binary", size),
            &(n, xs),
            |b, (n, xs)| b.iter(|| black_box(binary_search(n, xs))),
        );
        group.bench_with_input(
            BenchmarkId::new("lib linear", size),
            &(n, ys),
            |b, (n, ys)| b.iter(|| black_box(ys.iter().position(|y| y == n))),
        );
        group.bench_with_input(
            BenchmarkId::new("lib binary", size),
            &(n, zs),
            |b, (n, zs)| b.iter(|| black_box(zs.binary_search(n))),
        );
    }
    group.finish();
}

criterion_group!(benches, not_found);
criterion_main!(benches);
