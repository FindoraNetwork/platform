use ledger::staking::init;
use ruc::*;
use std::fs;

fn main() {
    pnk!(gen());
}

fn gen() -> Result<()> {
    let cfg_path = init::get_cfg_path().c(d!())?;
    let secret_info_path = format!("{}.keys", &cfg_path);
    let mut cfg_template = init::get_cfg_data().c(d!())?;

    let mnemonics = (0..cfg_template.valiators.len())
        .map(|_| {
            wallet::generate_mnemonic_custom(24, "en")
                .c(d!())
                .and_then(|m| {
                    wallet::restore_keypair_from_mnemonic_default(&m)
                        .c(d!())
                        .map(|kp| wallet::public_key_to_base64(&kp.get_pk()))
                        .map(|pk| (m, pk))
                })
        })
        .collect::<Result<Vec<_>>>()?;

    cfg_template
        .valiators
        .iter_mut()
        .zip(mnemonics.iter())
        .for_each(|(v, m)| {
            v.id = m.1.clone();
        });

    let cfg = cfg_template;
    serde_json::to_vec_pretty(&cfg)
        .c(d!())
        .and_then(|cfg| fs::write(cfg_path, cfg).c(d!()))
        .and_then(|_| serde_json::to_vec_pretty(&mnemonics).c(d!()))
        .and_then(|m| fs::write(&secret_info_path, m).c(d!()))
        .map(|_| {
            println!(
                "\x1b[01mSecret info has been writed to \x1b[31m{}\x1b[00m",
                secret_info_path
            );
        })
}
