use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

fn criterion_benchmark(c: &mut Criterion) {
    let source = include_str!("benchfile.dil");
    let mut group = c.benchmark_group("parse");

    let alloc = Bump::new();
    // SAFETY: dropped at the end of function
    let mut rt_alloc = unsafe { dilaria::RtAlloc::new() };

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("benchfile.dil", |b| {
        b.iter(|| parse(black_box(source), &alloc, &mut rt_alloc));
    });

    group.finish();
}

fn parse(source: &str, alloc: &Bump, rt_alloc: &mut dilaria::RtAlloc) {
    let p = dilaria::_parse(source, alloc, rt_alloc);
    black_box(&p);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
