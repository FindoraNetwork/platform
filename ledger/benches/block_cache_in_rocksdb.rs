use criterion::{criterion_group, criterion_main, Criterion};
use lazy_static::lazy_static;
use ledger::{data_model::FinalizedBlock, store::block_cache};

const NUM: u64 = 1;

lazy_static! {
    static ref DB: block_cache::Rocks = {
        let db = block_cache::Rocks::new();
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

pub fn rocks_write(c: &mut Criterion) {
    // make lazy_static to do its work
    assert_eq!(DB.len(), NUM as usize);
    c.bench_function("rocks_write", |b| b.iter(|| write()));
}

pub fn rocks_read(c: &mut Criterion) {
    c.bench_function("rocks_read", |b| b.iter(|| read()));
}

criterion_group!(benches, rocks_write, rocks_read);
criterion_main!(benches);
