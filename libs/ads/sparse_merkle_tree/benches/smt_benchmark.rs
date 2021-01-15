#[macro_use]
extern crate criterion;

use criterion::{BenchmarkId, Criterion};
//use criterion::black_box;

use cryptohash::sha256;
use sha256::DIGESTBYTES;

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use sparse_merkle_tree::*;

fn prepare_inserts(
    num_entries: usize,
    rng: &mut StdRng,
) -> (Vec<[u8; DIGESTBYTES]>, Vec<Vec<u8>>) {
    let mut keys = Vec::with_capacity(num_entries);
    let mut data = Vec::with_capacity(num_entries);
    for _ in 0..num_entries {
        let mut key_value = [0u8; DIGESTBYTES];
        rng.fill(&mut key_value);
        keys.push(key_value);

        let data_value = (0..DIGESTBYTES).map(|_| rng.gen()).collect();
        data.push(data_value);
    }

    keys.sort();

    (keys, data)
}

const NUM_KV_PAIRS: [usize; 6] = [1, 10, 100, 200, 500, 100];

/** Benchmarks [1, 10, 100, 200, 500, 1000] inserts to an smt */
fn smt_set_benchmark(c: &mut Criterion) {
    let seed = [0xBBu8; DIGESTBYTES];
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    let mut group = c.benchmark_group("Empty Tree");
    for size in NUM_KV_PAIRS.iter() {
        let parameter_string = format!("{} items", *size);
        let id = BenchmarkId::new("Insert", parameter_string);

        let inserts = prepare_inserts(*size, &mut rng); // 1000
        group.bench_with_input(id, &inserts, |b, (keys, values)| {
            let mut smt = SmtMap256::new();
            b.iter(|| {
                for i in 0..*size {
                    let res = smt.set(&keys[i], Some(&values[i]));
                    criterion::black_box(res);
                }
            });
        });
    }
    group.finish();
}

/** Benchmarks [1, 10, 100, 200, 500, 1000] retrievals from a tree */
fn smt_get_benchmark(c: &mut Criterion) {
    let seed = [0xBBu8; DIGESTBYTES];
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    let mut group = c.benchmark_group("Empty Tree");
    for size in NUM_KV_PAIRS.iter() {
        let parameter_string = format!("{} items", *size);
        let id = BenchmarkId::new("Insert", parameter_string);

        let inserts = prepare_inserts(*size, &mut rng); // 1000
        let mut smt = SmtMap256::new();
        for i in 0..*size {
            smt.set(&inserts.0[i], Some(&inserts.1[i]));
        }

        group.bench_with_input(id, &inserts, |b, (keys, values)| {
            b.iter(|| {
                for i in 0..*size {
                    let res = smt.get(&keys[i]);
                    assert_eq!(res, Some(&values[i]).as_ref());
                    criterion::black_box(res);
                }
            });
        });
    }
    group.finish();
}

/** Benchmarks [1, 10, 100, 200, 500, 1000] retrievals from a tree with proof */
fn smt_get_and_prove_benchmark(c: &mut Criterion) {
    let seed = [0xBBu8; DIGESTBYTES];
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    let mut group = c.benchmark_group("Empty Tree");
    for size in NUM_KV_PAIRS.iter() {
        let parameter_string = format!("{} items", *size);
        let id = BenchmarkId::new("Insert", parameter_string);

        let inserts = prepare_inserts(*size, &mut rng);
        let mut smt = SmtMap256::new();
        for i in 0..*size {
            smt.set(&inserts.0[i], Some(&inserts.1[i]));
        }

        group.bench_with_input(id, &inserts, |b, (keys, values)| {
            b.iter(|| {
                for i in 0..*size {
                    let (value, proof) = smt.get_with_proof(&keys[i]);
                    assert!(smt.check_merkle_proof(&keys[i], value, &proof));
                }
            });
        });
    }
    group.finish();
}

/** Benchmarks removal of item from smt */
fn smt_delete_benchmark(c: &mut Criterion) {
    let seed = [0xBBu8; DIGESTBYTES];
    let mut rng: StdRng = SeedableRng::from_seed(seed);
    let mut group = c.benchmark_group("Empty Tree");
    for size in NUM_KV_PAIRS.iter() {
        let parameter_string = format!("{} items", *size);
        let id = BenchmarkId::new("Insert", parameter_string);

        let inserts = prepare_inserts(*size, &mut rng);
        let mut smt = SmtMap256::new();
        for i in 0..*size {
            smt.set(&inserts.0[i], Some(&inserts.1[i]));
        }

        group.bench_with_input(id, &inserts, |b, (keys, values)| {
            b.iter(|| {
                for i in 0..*size {
                    smt.set(&keys[i], None);
                }
            });
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    smt_set_benchmark,
    smt_get_benchmark,
    smt_get_and_prove_benchmark,
    smt_delete_benchmark
);

criterion_main!(benches);
