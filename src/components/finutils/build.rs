use {
    ruc::*,
    vergen::{generate_cargo_keys, ConstantsFlags},
};

fn main() {
    let mut flags = ConstantsFlags::all();
    // Tell vergen to use the semver from cargo and not `git describe`
    flags.set(ConstantsFlags::SEMVER, false);
    flags.set(ConstantsFlags::SEMVER_FROM_CARGO_PKG, true);

    // Generate the 'cargo:' key output
    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");

    pnk!(store_meta());
}

#[cfg(feature = "genstx")]
fn store_meta() -> Result<()> {
    genstx::store_meta().c(d!())
}

#[cfg(not(feature = "genstx"))]
fn store_meta() -> Result<()> {
    Ok(())
}

#[cfg(feature = "genstx")]
mod genstx {
    use {
        globutils::{wallet, HashOf, SignatureOf},
        ledger::data_model::{StateCommitmentData, TxoSID, Utxo},
        ruc::*,
        std::{collections::BTreeMap, fs, sync::mpsc::channel, thread},
        zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo},
    };

    type UtxoMap = BTreeMap<XfrPublicKey, BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>>;

    pub fn store_meta() -> Result<()> {
        get_seq_id()
            .c(d!())
            .and_then(|seq_id| {
                fs::write("src/bins/genstx/seq_id.str", seq_id.to_string()).c(d!())
            })
            .and_then(|_| get_utxo_map().c(d!()))
            .and_then(|um| serde_json::to_vec(&um).c(d!()))
            .and_then(|um| fs::write("src/bins/genstx/utxo.map", um).c(d!()))
    }

    fn get_seq_id() -> Result<u64> {
        type Resp = (
            HashOf<Option<StateCommitmentData>>,
            u64,
            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
        );

        attohttpc::get("https://prod-mainnet.prod.findora.org:8668/global_state")
            .send()
            .c(d!())?
            .error_for_status()
            .c(d!())?
            .bytes()
            .c(d!())
            .and_then(|b| serde_json::from_slice::<Resp>(&b).c(d!()))
            .map(|resp| resp.1)
    }

    fn get_utxo_map() -> Result<UtxoMap> {
        let mut res = map! {B};

        let (s, r) = channel();
        let mut i = 0;
        for l in fs::read_to_string("../../../tools/mainnet_0_2_x_validator_id.list")
            .c(d!())?
            .lines()
        {
            i += 1;
            let pk = wallet::public_key_from_base64(l)
                .c(d!())
                .or_else(|e| wallet::public_key_from_bech32(l).c(d!(e)))?;
            let pk_str = wallet::public_key_to_base64(&pk);
            let url = format!(
                "https://prod-mainnet.prod.findora.org:8668/owned_utxos/{}",
                pk_str
            );

            let ss = s.clone();
            thread::spawn(move || {
                let get = || {
                    attohttpc::get(&url)
                        .send()
                        .c(d!())?
                        .error_for_status()
                        .c(d!())?
                        .bytes()
                        .c(d!())
                        .and_then(|b| {
                            serde_json::from_slice::<
                                BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>,
                            >(&b)
                            .c(d!())
                        })
                        .and_then(|utxos| ss.send((pk, utxos)).c(d!()))
                };
                pnk!(get());
            });
        }

        for _ in 0..i {
            let (pk, utxos) = r.recv().c(d!())?;
            res.insert(pk, utxos);
        }

        Ok(res)
    }
}
