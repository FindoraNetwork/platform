use criterion::{criterion_group, criterion_main, Criterion};
use lazy_static::lazy_static;
use ledger::{data_model::FinalizedBlock, store::block_cache};

const NUM: u64 = 1;

lazy_static! {
    static ref DB: block_cache::Sled = {
        let db = block_cache::Sled::new();
        (0..NUM).for_each(|i| {
            db.push(gen_sample(i));
        });
        db
    };
}

fn gen_sample(merkle_id: u64) -> FinalizedBlock {
    FinalizedBlock {
        txns: vec![],
        merkle_id,
    }
}

fn write() {
    (0..NUM).for_each(|i| {
        DB.push(gen_sample(i));
    });
}

fn read() {
    DB.iter().enumerate().for_each(|(i, b)| {
        assert_eq!(i, b.merkle_id as usize);
    });
}

pub fn sled_write(c: &mut Criterion) {
    // make lazy_static to do its work
    assert_eq!(DB.len(), NUM as usize);
    c.bench_function("sled_write", |b| b.iter(|| write()));
}

pub fn sled_read(c: &mut Criterion) {
    c.bench_function("sled_read", |b| b.iter(|| read()));
}

criterion_group!(benches, sled_write, sled_read);
criterion_main!(benches);
