use evm_precompile_anemoi::Anemoi;
use noah_algebra::bls12_381::BLSScalar;
use noah_algebra::prelude::Scalar;
use std::time::Instant;

fn main() {
    let mut prng = noah_algebra::rand_helper::test_rng();
    let mut elems = Vec::with_capacity(32);
    for _ in 0..32 {
        elems.push(BLSScalar::random(&mut prng));
    }

    let data = elems
        .iter()
        .map(|x| x.to_bytes())
        .flatten()
        .collect::<Vec<u8>>();

    println!("Benchmarking 2 field elements for 1000 times");
    let start = Instant::now();
    for _ in 0..1000 {
        _ = Anemoi::execute_with_input_and_gas(&data[0..64], None);
    }
    let time = start.elapsed().as_nanos() / 1000;
    let gas = Anemoi::GAS_PER_PERM;
    println!("Result = {} ns every time", time);
    println!("Cost = {} every time", gas);
    println!("NS per gas = {}", (time as f64) / (gas as f64));

    println!("Benchmarking 32 field elements for 1000 times");
    let start = Instant::now();
    for _ in 0..1000 {
        _ = Anemoi::execute_with_input_and_gas(&data[0..1024], None);
    }
    let time = start.elapsed().as_nanos() / 1000;
    let gas = Anemoi::GAS_PER_PERM * 11;
    println!("Result = {} ns every time", time);
    println!("Cost = {} every time", gas);
    println!("NS per gas = {}", (time as f64) / (gas as f64));
}
