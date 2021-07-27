use ruc::*;
use std::env::args;

fn main() {
    let n = pnk!(
        args()
            .nth(1)
            .unwrap_or_else(|| "1".to_owned())
            .parse::<u64>()
    );
    (0..n).for_each(|_| {
        let mnemonic = pnk!(wallet::generate_mnemonic_custom(24, "en"));
        let key = wallet::restore_keypair_from_mnemonic_default(&mnemonic)
            .c(d!())
            .and_then(|kp| serde_json::to_string_pretty(&kp).c(d!()));
        println!(
            "\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mKey:\x1b[00m {}\n",
            mnemonic,
            pnk!(key)
        );
    });
}
