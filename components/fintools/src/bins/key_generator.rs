use fintools::fns::gen_key_and_print;
use ruc::*;
use std::env::args;

fn main() {
    let n = pnk!(
        args()
            .nth(1)
            .unwrap_or_else(|| "1".to_owned())
            .parse::<u64>()
    );
    (0..n).for_each(|_| gen_key_and_print());
}
