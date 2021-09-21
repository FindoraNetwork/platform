//!
//! # Findora Network CLI tool
//!
//! FN, a command line tool in findora network.
//!
//! ## Usage
//!
//! ```
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

use clap::{crate_authors, load_yaml, App};
use crypto::basics::hybrid_encryption::{XPublicKey, XSecretKey};
use finutils::common;
use finutils::common::evm::*;
use fp_utils::ecdsa::SecpPair;
use globutils::wallet;
use ledger::data_model::{AssetTypeCode, FRA_DECIMALS};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use std::{fmt, fs};
use zei::anon_xfr::keys::AXfrKeyPair;
use zei::serialization::ZeiFromToBytes;

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
        let cr = m.value_of("commission-rate");
        let vm = m.value_of("validator-memo");
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
    } else if matches.is_present("set-initial-validators") {
        common::set_initial_validators().c(d!())?;
    } else if matches.is_present("gen-eth-key") {
        let (pair, phrase, _) = SecpPair::generate_with_phrase(None);
        let kp = hex::encode(pair.seed());
        println!(
            "\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mPrivateKey:\x1b[00m {}\n\x1b[31;01mAddress:\x1b[00m {:?}\n",
            phrase,
            kp,
            pair.address()
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
        let owner_sk = m.value_of("from-seckey");
        let target_addr = m.value_of("to-pubkey");
        let owner_enc_key = m.value_of("enc-key");
        let txo_sid = m.value_of("txo-sid");

        if target_addr.is_none() || owner_enc_key.is_none() || txo_sid.is_none() {
            println!("{}", m.usage());
        } else {
            common::convert_bar2abar(
                owner_sk,
                target_addr.unwrap(),
                owner_enc_key.unwrap(),
                txo_sid.unwrap(),
            )
            .c(d!())?;
        }
    } else if let Some(_m) = matches.subcommand_matches("generate-anon-keys") {
        let mut prng = ChaChaRng::from_entropy();
        let keypair = AXfrKeyPair::generate(&mut prng);

        println!(
            "AXfrSecretKey: {}",
            base64::encode(keypair.zei_to_bytes().as_slice())
        );
        println!(
            "AXfrPublicKey: {}",
            base64::encode(keypair.pub_key().zei_to_bytes().as_slice())
        );

        let secret_key = XSecretKey::new(&mut prng);
        let public_key = XPublicKey::from(&secret_key);
        println!(
            "Decryption Key: {}",
            base64::encode(secret_key.zei_to_bytes().as_slice())
        );
        println!(
            "Encryption Key: {}",
            base64::encode(public_key.zei_to_bytes().as_slice())
        );
    } else if let Some(m) = matches.subcommand_matches("anon-transfer") {
        let axfr_secret_key  = m.value_of("axfr-secret-key");
        let dec_key = m.value_of("decryption-key");
        let amount = m.value_of("n");   
    
        if axfr_secret_key.is_none() || dec_key.is_none() || amount.is_none() {
            println!("{}", m.usage());
        } else {
            common::gen_oabar_add_op(
                axfr_secret_key.unwrap(),
                dec_key.unwrap(),
                amount.unwrap(),
            )
            .c(d!())?;
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
