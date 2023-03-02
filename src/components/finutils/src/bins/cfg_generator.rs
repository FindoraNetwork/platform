use {
    globutils::wallet,
    ledger::staking::init,
    ruc::*,
    std::{env, fs},
};

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
                        .map(|k| (m, wallet::public_key_to_bech32(k.get_pk_ref()), k))
                })
        })
        .collect::<Result<Vec<_>>>()?;

    println!("Public Key List:");
    mnemonics
        .iter()
        .map(|kp| serde_json::to_string(kp.2.get_pk_ref()).unwrap())
        .for_each(|pk| {
            println!("{}", pk.trim_matches(|c| c == '"'));
        });

    println!("Private Key List:");
    mnemonics
        .iter()
        .map(|kp| serde_json::to_string(kp.2.get_sk_ref()).unwrap())
        .for_each(|pk| {
            println!("{}", pk.trim_matches(|c| c == '"'));
        });

    let id_list = if let Ok(file) = env::var("MAINNET_0_2_X_VALIDATOR_ID_LIST") {
        fs::read_to_string(file).c(d!()).and_then(|list| {
            list.lines()
                .map(|id| {
                    wallet::public_key_from_base64(id)
                        .c(d!())
                        .or_else(|e| wallet::public_key_from_bech32(id).c(d!(e)))
                        .map(|pk| wallet::public_key_to_base64(&pk))
                })
                .collect::<Result<Vec<_>>>()
        })?
    } else {
        mnemonics
            .iter()
            .map(|m| wallet::public_key_to_base64(&m.2.get_pk()))
            .collect()
    };

    cfg_template
        .valiators
        .iter_mut()
        .zip(id_list.into_iter())
        .for_each(|(v, id)| {
            v.id = id;
        });

    let cfg = cfg_template;
    serde_json::to_vec_pretty(&cfg)
        .c(d!())
        .and_then(|cfg| fs::write(cfg_path, cfg).c(d!()))
        .and_then(|_| serde_json::to_vec_pretty(&mnemonics).c(d!()))
        .and_then(|m| fs::write(&secret_info_path, m).c(d!()))
        .map(|_| {
            println!(
                "\x1b[01mSecret info has been writed to \x1b[31m{secret_info_path}\x1b[00m",
            );
        })
}
