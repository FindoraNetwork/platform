use evm_precompile_basic::Sha256;
use module_evm::precompile::LinearCostPrecompile;
use std::time::Instant;

fn main() {
    let data = vec![1u8; 16384];

    println!("Benchmarking SHA256 for 1KB data for 10000 times");
    let start = Instant::now();
    for _ in 0..10000 {
        _ = Sha256::execute(&data[0..1024], 0);
    }
    let time = start.elapsed().as_nanos() / 10000;
    let gas = Sha256::BASE + Sha256::WORD * 1024 / 32;
    println!("Result = {} ns every time", time);
    println!("Cost = {} every time", gas);
    println!("NS per gas = {}", (time as f64) / (gas as f64));

    println!("Benchmarking SHA256 for 4KB data for 10000 times");
    let start = Instant::now();
    for _ in 0..10000 {
        _ = Sha256::execute(&data[0..4096], 0);
    }
    let time = start.elapsed().as_nanos() / 10000;
    let gas = Sha256::BASE + Sha256::WORD * 4096 / 32;
    println!("Result = {} ns every time", time);
    println!("Cost = {} every time", gas);
    println!("NS per gas = {}", (time as f64) / (gas as f64));

    println!("Benchmarking SHA256 for 16KB data for 10000 times");
    let start = Instant::now();
    for _ in 0..10000 {
        _ = Sha256::execute(&data, 0);
    }
    let time = start.elapsed().as_nanos() / 10000;
    let gas = Sha256::BASE + Sha256::WORD * 16384 / 32;
    println!("Result = {} ns every time", time);
    println!("Cost = {} every time", gas);
    println!("NS per gas = {}", (time as f64) / (gas as f64));
}
