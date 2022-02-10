//!
//! # Findora Network CLI tool
//!
//! FN, a command line tool in findora network.
//!
//! ## Usage
//!
//! ```shell
//! fn [SUBCOMMAND]
//!
//! - stake
//!     - "--amount=[Amout]"
//!     - "--validator-pubkey=[Tendermint PubKey]"
//!     - "--validator-memo=[StakingMemo, default to empty]"
//! - claim
//!     - "--amount=[Amout <Optional, default to 'all'>]"
//! - unstake
//! - show, query real-time state of your staking
//! - setup
//!     - "--serv-addr=[URL/IP]"
//!     - "--owner-mnemonic-path=[File Path]"
//!         - the `id` of your validator will be drived from this
//! ```
//!

#![deny(warnings)]

use {
    clap::{crate_authors, load_yaml, App},
    crypto::basics::hybrid_encryption::{XPublicKey, XSecretKey},
    finutils::common::{self, evm::*},
    fp_utils::ecdsa::SecpPair,
    globutils::wallet,
    ledger::{
        data_model::{ATxoSID, AssetTypeCode, FRA_DECIMALS},
        staking::StakerMemo,
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::fs::File,
    std::{fmt, fs},
    zei::anon_xfr::keys::AXfrKeyPair,
    zei::anon_xfr::structs::{
        AnonBlindAssetRecord, OpenAnonBlindAssetRecord, OpenAnonBlindAssetRecordBuilder,
    },
};

fn main() {
    if let Err(e) = run() {
        tip_fail(e);
    } else {
        tip_success();
    }
}

fn run() -> Result<()> {
    let yaml = load_yaml!("fn.yml");
    let matches = App::from_yaml(yaml)
        .version(common::version())
        .author(crate_authors!())
        .get_matches();

    if matches.is_present("version") {
        println!("{}", env!("VERGEN_SHA"));
    } else if matches.is_present("genkey") {
        common::gen_key_and_print();
    } else if let Some(m) = matches.subcommand_matches("wallet") {
        if m.is_present("create") {
            common::gen_key_and_print();
        } else if m.is_present("show") {
            let seckey = match m.value_of("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            // FRA asset is the default case
            let asset = if let Some(code) = m.value_of("asset") {
                match code.to_lowercase().as_str() {
                    "fra" => None,
                    _ => Some(code),
                }
            } else {
                None
            };
            common::show_account(seckey.as_deref(), asset).c(d!())?;
        } else {
            println!("{}", m.usage());
        }
    } else if let Some(m) = matches.subcommand_matches("delegate") {
        let seckey = match m.value_of("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let amount = m.value_of("amount");
        let validator = m.value_of("validator");
        let show_info = m.is_present("info");

        if amount.is_some() && validator.is_some() {
            common::delegate(
                seckey.as_deref(),
                amount.unwrap().parse::<u64>().c(d!())?,
                validator.unwrap(),
            )
            .c(d!())?;
        } else if show_info {
            common::show_delegations(seckey.as_deref()).c(d!())?;
        } else {
            println!("{}", m.usage());
        }
    } else if let Some(m) = matches.subcommand_matches("undelegate") {
        let seckey = match m.value_of("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let amount = m.value_of("amount");
        let validator = m.value_of("validator");
        if (amount.is_none() && validator.is_some())
            || (amount.is_some() && validator.is_none())
        {
            println!("{}", m.usage());
            return Ok(());
        }
        let param = if amount.is_some() {
            Some((amount.unwrap().parse::<u64>().c(d!())?, validator.unwrap()))
        } else {
            None
        };
        common::undelegate(seckey.as_deref(), param).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("asset") {
        if m.is_present("create") {
            let seckey = match m.value_of("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let memo = m.value_of("memo");
            if memo.is_none() {
                println!("{}", m.usage());
                return Ok(());
            }
            let transferable = m.is_present("transferable");
            let decimal = if let Some(num) = m.value_of("decimal") {
                num.parse::<u8>()
                    .c(d!("decimal should be an 8-bits unsinged integer"))?
            } else {
                FRA_DECIMALS
            };
            let max_units = if let Some(max) = m.value_of("maximum") {
                Some(
                    max.parse::<u64>()
                        .c(d!("maximum should be an unsigned integer"))?,
                )
            } else {
                None
            };
            let token_code = m.value_of("code");
            common::create_asset(
                seckey.as_deref(),
                memo.unwrap(),
                decimal,
                max_units,
                transferable,
                token_code,
            )
            .c(d!())?;
        } else if m.is_present("show") {
            let addr = m.value_of("addr");
            if addr.is_none() {
                println!("{}", m.usage());
                return Ok(());
            } else {
                common::show_asset(addr.unwrap()).c(d!())?;
            }
        } else if m.is_present("issue") {
            let seckey = match m.value_of("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let code = m.value_of("code");
            let amount = m.value_of("amount");
            if code.is_none() || amount.is_none() {
                println!("{}", m.usage());
                return Ok(());
            }
            let amount = amount
                .unwrap()
                .parse::<u64>()
                .c(d!("amount should be a 64-bits unsigned integer"))?;
            let hidden = m.is_present("hidden");

            common::issue_asset(seckey.as_deref(), code.unwrap(), amount, hidden)
                .c(d!())?;
        } else {
            let help = "fn asset [--create | --issue | --show]";
            println!("{}", help);
        }
    } else if let Some(m) = matches.subcommand_matches("staker-update") {
        let vm = if let Some(memo) = m.value_of("validator-memo") {
            Some(serde_json::from_str(memo).c(d!())?)
        } else {
            match (
                m.value_of("validator-memo-name"),
                m.value_of("validator-memo-desc"),
                m.value_of("validator-memo-website"),
                m.value_of("validator-memo-logo"),
            ) {
                (None, None, None, None) => None,
                (name, desc, website, logo) => {
                    let mut memo = StakerMemo::default();
                    if let Some(n) = name {
                        memo.name = n.to_owned();
                    }
                    if let Some(d) = desc {
                        memo.desc = d.to_owned();
                    }
                    if let Some(w) = website {
                        memo.website = w.to_owned();
                    }
                    if let Some(l) = logo {
                        memo.logo = l.to_owned();
                    }
                    Some(memo)
                }
            }
        };

        let cr = m.value_of("commission-rate");
        if vm.is_none() && cr.is_none() {
            println!("{}", m.usage());
            println!(
                "Tips: to update the information of your node, please specify commission-rate or memo"
            );
        } else {
            common::staker_update(cr, vm).c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("stake") {
        let am = m.value_of("amount");
        if m.is_present("append") {
            let staker = match m.value_of("staker-priv-key") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let td_addr = m.value_of("validator-td-addr");
            if am.is_none() {
                println!("{}", m.usage());
            } else {
                common::stake_append(am.unwrap(), staker.as_deref(), td_addr).c(d!())?;
            }
        } else {
            let cr = m.value_of("commission-rate");
            let vm = m.value_of("validator-memo");
            let force = m.is_present("force");
            if am.is_none() || cr.is_none() {
                println!("{}", m.usage());
                println!(
                    "Tips: if you want to raise the power of your node, please use `fn stake --append [OPTIONS]`"
                );
            } else {
                common::stake(am.unwrap(), cr.unwrap(), vm, force).c(d!())?;
            }
        }
    } else if let Some(m) = matches.subcommand_matches("unstake") {
        let am = m.value_of("amount");
        let staker = match m.value_of("staker-priv-key") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let td_addr = m.value_of("validator-td-addr");
        common::unstake(am, staker.as_deref(), td_addr).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("claim") {
        let am = m.value_of("amount");
        let seckey = match m.value_of("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        common::claim(am, seckey.as_deref()).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("show") {
        let basic = m.is_present("basic");
        common::show(basic).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("setup") {
        let sa = m.value_of("serv-addr");
        let om = m.value_of("owner-mnemonic-path");
        let tp = m.value_of("validator-key");
        if sa.is_none() && om.is_none() && tp.is_none() {
            println!("{}", m.usage());
        } else {
            common::setup(sa, om, tp).c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("transfer") {
        let f = match m.value_of("from-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let asset = m.value_of("asset").unwrap_or("FRA");
        let t = m
            .value_of("to-pubkey")
            .c(d!())
            .and_then(|pk| wallet::public_key_from_base64(pk).c(d!()))
            .or_else(|_| {
                m.value_of("to-wallet-address").c(d!()).and_then(|addr| {
                    wallet::public_key_from_bech32(addr).c(d!("invalid wallet address"))
                })
            })?;
        let am = m.value_of("amount");

        if am.is_none() {
            println!("{}", m.usage());
        } else {
            let token_code = if asset.to_uppercase() != "FRA" {
                Some(AssetTypeCode::new_from_base64(asset).c(d!())?)
            } else {
                None
            };
            common::transfer_asset(
                f.as_deref(),
                t,
                token_code,
                am.unwrap(),
                m.is_present("confidential-amount"),
                m.is_present("confidential-type"),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("transfer-batch") {
        let f = match m.value_of("from-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let t = m
            .value_of("to-pubkey-file")
            .c(d!())
            .and_then(|f| {
                fs::read_to_string(f).c(d!()).and_then(|pks| {
                    pks.lines()
                        .map(|pk| wallet::public_key_from_base64(pk.trim()))
                        .collect::<Result<Vec<_>>>()
                        .c(d!("invalid file"))
                })
            })
            .or_else(|_| {
                m.value_of("to-wallet-address-file").c(d!()).and_then(|f| {
                    fs::read_to_string(f).c(d!()).and_then(|addrs| {
                        addrs
                            .lines()
                            .map(|addr| wallet::public_key_from_bech32(addr.trim()))
                            .collect::<Result<Vec<_>>>()
                            .c(d!("invalid file"))
                    })
                })
            })?;
        let am = m.value_of("amount");

        if am.is_none() || t.is_empty() {
            println!("{}", m.usage());
        } else {
            common::transfer_asset_batch(
                f.as_deref(),
                &t,
                None,
                am.unwrap(),
                m.is_present("confidential-amount"),
                m.is_present("confidential-type"),
            )
            .c(d!())?;
        }
    } else if matches.is_present("gen-eth-key") {
        let (pair, phrase, _) = SecpPair::generate_with_phrase(None);
        let kp = hex::encode(pair.seed());
        println!(
            "\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mPrivateKey:\x1b[00m {}\n\x1b[31;01mAddress:\x1b[00m {}\n",
            phrase,
            kp,
            eth_checksum::checksum(&format!("{:?}", pair.address()))
        );
    } else if let Some(m) = matches.subcommand_matches("account") {
        let address = m.value_of("addr");
        let (account, info) = contract_account_info(address)?;
        println!("AccountId: {}\n{:#?}\n", account, info);
    } else if let Some(m) = matches.subcommand_matches("contract-deposit") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        transfer_to_account(amount.parse::<u64>().c(d!())?, address)?
    } else if let Some(m) = matches.subcommand_matches("contract-withdraw") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        let eth_key = m.value_of("eth-key");
        transfer_from_account(amount.parse::<u64>().c(d!())?, address, eth_key)?
    } else if let Some(m) = matches.subcommand_matches("convert-bar-to-abar") {
        let f = match m.value_of("from-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };

        let anon_keys = match m.value_of("anon-keys") {
            Some(path) => {
                let f =
                    fs::read_to_string(path).c(d!("Failed to read anon-keys file"))?;
                let keys = serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?;
                keys
            }
            None => return Err(eg!("path for anon-keys file not found")),
        };

        let owner_sk = f.as_ref();
        let target_addr = anon_keys.axfr_public_key;
        let owner_enc_key = anon_keys.enc_key;
        let txo_sid = m.value_of("txo-sid");

        if txo_sid.is_none() {
            println!("{}", m.usage());
        } else {
            let r = common::convert_bar2abar(
                owner_sk,
                target_addr,
                owner_enc_key,
                txo_sid.unwrap(),
            )
            .c(d!())?;

            println!(
                "\x1b[31;01m Randomizer: {}\x1b[00m",
                wallet::randomizer_to_base58(&r)
            );
            let mut file = fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open("randomizers")
                .expect("cannot open randomizers file");
            std::io::Write::write_all(
                &mut file,
                ("\n".to_owned() + &wallet::randomizer_to_base58(&r)).as_bytes(),
            )
            .expect("randomizer write failed");
        }
    } else if let Some(m) = matches.subcommand_matches("convert-abar-to-bar") {
        let anon_keys = match m.value_of("anon-keys") {
            Some(path) => {
                let f =
                    fs::read_to_string(path).c(d!("Failed to read anon-keys file"))?;
                let keys = serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?;
                keys
            }
            None => return Err(eg!("path for anon-keys file not found")),
        };
        let axfr_secret_key = anon_keys.axfr_secret_key;
        let randomizer = m.value_of("randomizer");
        let dec_key = anon_keys.dec_key;
        let to_xfr_public_key = m.value_of("to-xfr-public-key");
        let fee_xfr_seckey = match m.value_of("fee-xfr-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };

        if randomizer.is_none() || to_xfr_public_key.is_none() {
            println!("{}", m.usage());
        } else {
            common::convert_abar2bar(
                axfr_secret_key,
                randomizer.unwrap(),
                dec_key,
                to_xfr_public_key.unwrap(),
                fee_xfr_seckey.as_deref(),
                m.is_present("confidential-amount"),
                m.is_present("confidential-type"),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("gen-anon-keys") {
        let mut prng = ChaChaRng::from_entropy();
        let keypair = AXfrKeyPair::generate(&mut prng);
        let secret_key = XSecretKey::new(&mut prng);
        let public_key = XPublicKey::from(&secret_key);

        let keys = AnonKeys {
            axfr_secret_key: wallet::anon_secret_key_to_base64(&keypair),
            axfr_public_key: wallet::anon_public_key_to_base64(&keypair.pub_key()),
            enc_key: wallet::x_public_key_to_base64(&public_key),
            dec_key: wallet::x_secret_key_to_base64(&secret_key),
        };

        if let Some(path) = m.value_of("file-path") {
            serde_json::to_writer_pretty(&File::create(path).c(d!())?, &keys).c(d!())?;
            println!("Keys saved to file: {}", path);
        }

        // print keys to terminal
        println!("Keys :\n {}", serde_json::to_string_pretty(&keys).unwrap());
    } else if let Some(m) = matches.subcommand_matches("owned-abars") {
        let randomizer_str = m.value_of("randomizer");
        let axfr_public_key_str = m.value_of("axfr-public-key");

        // create derived public key
        let randomizer = wallet::randomizer_from_base58(randomizer_str.unwrap())?;
        let axfr_public_key =
            wallet::anon_public_key_from_base64(axfr_public_key_str.unwrap())?;
        let derived_public_key = axfr_public_key.randomize(&randomizer);

        println!(
            "Derived Public Key:   {}",
            wallet::anon_public_key_to_base64(&derived_public_key)
        );

        // get results from query server and print
        let list = common::get_owned_abars(&derived_public_key).c(d!())?;
        println!(
            "(AtxoSID, ABAR)   :  {}",
            serde_json::to_string(&list).c(d!())?
        );
    } else if let Some(m) = matches.subcommand_matches("owned-open-abars") {
        let anon_keys = match m.value_of("anon-keys") {
            Some(path) => {
                let f =
                    fs::read_to_string(path).c(d!("Failed to read anon-keys file"))?;
                let keys = serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?;
                keys
            }
            None => return Err(eg!("path for anon-keys file not found")),
        };
        let randomizer_str = m.value_of("randomizer");

        // create derived public key
        let randomizer = wallet::randomizer_from_base58(randomizer_str.unwrap())?;
        let axfr_public_key =
            wallet::anon_public_key_from_base64(anon_keys.axfr_public_key.as_str())?;
        let axfr_secret_key =
            wallet::anon_secret_key_from_base64(anon_keys.axfr_secret_key.as_str())
                .c(d!())?;
        let dec_key = wallet::x_secret_key_from_base64(anon_keys.dec_key.as_str())?;
        let derived_public_key = axfr_public_key.randomize(&randomizer);

        println!(
            "Derived Public Key:   {}",
            wallet::anon_public_key_to_base64(&derived_public_key)
        );

        // get results from query server and print
        let list = common::get_owned_abars(&derived_public_key).c(d!())?;

        let list = list
            .iter()
            .map(|(uid, abar)| {
                let memo = common::get_abar_memo(uid).unwrap().unwrap();
                let oabar = OpenAnonBlindAssetRecordBuilder::from_abar(
                    abar,
                    memo,
                    &axfr_secret_key,
                    &dec_key,
                )
                .unwrap()
                .build()
                .unwrap();
                (uid, abar, oabar)
            })
            .collect::<Vec<(&ATxoSID, &AnonBlindAssetRecord, OpenAnonBlindAssetRecord)>>(
            );

        println!(
            "(AtxoSID, ABAR, OABAR)   :  {}",
            serde_json::to_string(&list).c(d!())?
        );
    } else if let Some(_m) = matches.subcommand_matches("owned-utxos") {
        let list = common::get_owned_utxos()?;
        println!("{:?}", list);
    } else if let Some(m) = matches.subcommand_matches("anon-transfer") {
        let anon_keys = match m.value_of("anon-keys") {
            Some(path) => {
                let f =
                    fs::read_to_string(path).c(d!("Failed to read anon-keys file"))?;
                let keys = serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?;
                keys
            }
            None => return Err(eg!("path for anon-keys file not found")),
        };
        let axfr_secret_key = anon_keys.axfr_secret_key;
        let randomizer = m.value_of("randomizer");
        let dec_key = anon_keys.dec_key;
        let to_axfr_public_key = m.value_of("to-axfr-public-key");
        let to_enc_key = m.value_of("to-enc-key");
        let amount = m.value_of("amount");

        if randomizer.is_none()
            || to_axfr_public_key.is_none()
            || to_enc_key.is_none()
            || amount.is_none()
        {
            println!("{}", m.usage());
        } else {
            common::gen_oabar_add_op(
                axfr_secret_key,
                randomizer.unwrap(),
                dec_key,
                amount.unwrap(),
                to_axfr_public_key.unwrap(),
                to_enc_key.unwrap(),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("anon-transfer-batch") {
        let axfr_secret_keys =
            m.value_of("axfr-secretkey-file").c(d!()).and_then(|f| {
                fs::read_to_string(f).c(d!()).and_then(|sks| {
                    sks.lines()
                        .map(|sk| wallet::anon_secret_key_from_base64(sk.trim()))
                        .collect::<Result<Vec<_>>>()
                        .c(d!("invalid file"))
                })
            })?;
        let dec_keys = m.value_of("decryption-key-file").c(d!()).and_then(|f| {
            fs::read_to_string(f).c(d!()).and_then(|dks| {
                dks.lines()
                    .map(|dk| wallet::x_secret_key_from_base64(dk.trim()))
                    .collect::<Result<Vec<_>>>()
                    .c(d!("invalid file"))
            })
        })?;
        let to_axfr_public_keys = m
            .value_of("to-axfr-public-key-file")
            .c(d!())
            .and_then(|f| {
                fs::read_to_string(f).c(d!()).and_then(|pks| {
                    pks.lines()
                        .map(|pk| wallet::anon_public_key_from_base64(pk.trim()))
                        .collect::<Result<Vec<_>>>()
                        .c(d!("invalid file"))
                })
            })?;
        let to_enc_keys = m.value_of("to-enc-key-file").c(d!()).and_then(|f| {
            fs::read_to_string(f).c(d!()).and_then(|eks| {
                eks.lines()
                    .map(|ek| wallet::x_public_key_from_base64(ek.trim()))
                    .collect::<Result<Vec<_>>>()
                    .c(d!("invalid file"))
            })
        })?;
        let randomizers = m.value_of("randomizer-file").c(d!()).and_then(|f| {
            fs::read_to_string(f)
                .c(d!())
                .map(|rms| rms.lines().map(String::from).collect::<Vec<String>>())
        })?;
        let amounts = m.value_of("amount-file").c(d!()).and_then(|f| {
            fs::read_to_string(f)
                .c(d!())
                .map(|ams| ams.lines().map(String::from).collect::<Vec<String>>())
        })?;

        if axfr_secret_keys.is_empty()
            || dec_keys.is_empty()
            || to_axfr_public_keys.is_empty()
            || to_enc_keys.is_empty()
            || randomizers.is_empty()
            || amounts.is_empty()
        {
            println!("{}", m.usage());
        } else {
            common::gen_oabar_add_op_x(
                axfr_secret_keys,
                dec_keys,
                to_axfr_public_keys,
                to_enc_keys,
                randomizers,
                amounts,
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("anon-fetch-merkle-proof") {
        let atxo_sid = m.value_of("atxo-sid");

        if atxo_sid.is_none() {
            println!("{}", m.usage());
        } else {
            let mt_leaf_info = common::get_mtleaf_info(atxo_sid.unwrap()).c(d!())?;
            println!("{:?}", serde_json::to_string_pretty(&mt_leaf_info));
        }
    } else {
        println!("{}", matches.usage());
    }

    Ok(())
}

fn tip_fail(e: impl fmt::Display) {
    eprintln!("\n\x1b[31;01mFAIL !!!\x1b[00m");
    eprintln!(
        "\x1b[35;01mTips\x1b[01m:\n\tPlease send your error messages to us,\n\tif you can't understand their meanings ~^!^~\x1b[00m"
    );
    eprintln!("\n{}", e);
}

fn tip_success() {
    println!(
        "\x1b[35;01mNote\x1b[01m:\n\tYour operations has been executed without local error,\n\tbut the final result may need an asynchronous query.\x1b[00m"
    );
}

#[derive(Clone, Deserialize, Serialize)]
pub struct AnonKeys {
    pub axfr_secret_key: String,
    pub axfr_public_key: String,
    pub enc_key: String,
    pub dec_key: String,
}
