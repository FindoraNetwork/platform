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

use std::str::FromStr;

use fp_types::H160;

use {
    clap::{crate_authors, load_yaml, App},
    finutils::common::{self, evm::*, get_keypair, utils},
    fp_utils::ecdsa::SecpPair,
    globutils::wallet,
    ledger::{
        data_model::{AssetTypeCode, ASSET_TYPE_FRA, FRA_DECIMALS},
        staking::{StakerMemo, VALIDATORS_MIN},
    },
    ruc::*,
    std::{fmt, fs},
    zei::{noah_api::anon_xfr::structs::OpenAnonAssetRecordBuilder, XfrSecretKey},
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
    } else if let Some(m) = matches.subcommand_matches("genkey") {
        let gen_eth_address = m.is_present("gen-eth-address");
        common::gen_key_and_print(gen_eth_address);
    } else if let Some(m) = matches.subcommand_matches("wallet") {
        if m.is_present("create") {
            let is_address_eth = m.is_present("gen-eth-address");
            common::gen_key_and_print(is_address_eth);
        } else if m.is_present("show") {
            let seckey = match m.value_of("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let is_address_eth = m.is_present("use-default-eth-address");

            // FRA asset is the default case
            let asset = if let Some(code) = m.value_of("asset") {
                match code.to_lowercase().as_str() {
                    "fra" => None,
                    _ => Some(code),
                }
            } else {
                None
            };
            common::show_account(seckey.as_deref(), asset, is_address_eth).c(d!())?;
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
        let is_address_eth = m.is_present("use-default-eth-address");

        if amount.is_some() && validator.is_some() {
            common::delegate(
                seckey.as_deref(),
                amount.unwrap().parse::<u64>().c(d!())?,
                validator.unwrap(),
                is_address_eth,
            )
            .c(d!())?;
        } else if show_info {
            common::show_delegations(seckey.as_deref(), is_address_eth).c(d!())?;
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
        let is_address_eth = m.is_present("use-default-eth-address");
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
        common::undelegate(seckey.as_deref(), param, is_address_eth).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("asset") {
        if m.is_present("create") {
            let seckey = read_file_path(m.value_of("seckey")).c(d!())?;
            let memo = m.value_of("memo");
            if memo.is_none() {
                println!("{}", m.usage());
                return Ok(());
            }
            let transferable = m.is_present("transferable");
            let is_address_eth = m.is_present("use-default-eth-address");

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
                is_address_eth,
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
            let is_address_eth = m.is_present("use-default-eth-address");

            common::issue_asset(
                seckey.as_deref(),
                code.unwrap(),
                amount,
                hidden,
                is_address_eth,
            )
            .c(d!())?;
        } else {
            let help = "fn asset [--create | --issue | --show]";
            println!("{help}",);
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
        let is_address_eth = m.is_present("use-eth-address");

        let cr = m.value_of("commission-rate");
        if vm.is_none() && cr.is_none() {
            println!("{}", m.usage());
            println!(
                "Tips: to update the information of your node, please specify commission-rate or memo"
            );
        } else {
            common::staker_update(cr, vm, is_address_eth).c(d!())?;
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
            let is_address_eth = m.is_present("use-default-eth-address");
            if am.is_none() {
                println!("{}", m.usage());
            } else {
                common::stake_append(
                    am.unwrap(),
                    staker.as_deref(),
                    td_addr,
                    is_address_eth,
                )
                .c(d!())?;
            }
        } else {
            let cr = m.value_of("commission-rate");
            let vm = m.value_of("validator-memo");
            let force = m.is_present("force");
            let is_address_eth = m.is_present("use-default-eth-address");
            if am.is_none() || cr.is_none() {
                println!("{}", m.usage());
                println!(
                    "Tips: if you want to raise the power of your node, please use `fn stake --append [OPTIONS]`"
                );
            } else {
                common::stake(am.unwrap(), cr.unwrap(), vm, force, is_address_eth)
                    .c(d!())?;
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
        let is_address_eth = m.is_present("use-default-eth-address");
        common::unstake(am, staker.as_deref(), td_addr, is_address_eth).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("claim") {
        let am = m.value_of("amount");
        let is_address_eth = m.is_present("use-default-eth-address");
        let seckey = match m.value_of("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let td_addr = match m.value_of("validator-td-addr") {
            Some(v) => v,
            None => {
                println!("{}", m.usage());
                return Ok(());
            }
        };
        common::claim(td_addr, am, seckey.as_deref(), is_address_eth).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("show") {
        let basic = m.is_present("basic");
        let is_address_eth = m.is_present("eth-address");
        common::show(basic, is_address_eth).c(d!())?;
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
        let f = read_file_path(m.value_of("from-seckey")).c(d!())?;
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
        let is_address_eth = m.is_present("use-default-eth-address");

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
                is_address_eth,
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
        let is_address_eth = m.is_present("use-default-eth-address");

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
                is_address_eth,
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
        let sec_key = m.value_of("sec-key");
        let is_address_eth = m.is_present("use-default-eth-address");

        // FRA asset is the default case
        let asset = if let Some(code) = m.value_of("asset") {
            match code.to_lowercase().as_str() {
                "fra" => None,
                _ => Some(code),
            }
        } else {
            None
        };
        if sec_key.is_some() {
            // Asset defaults to fra
            common::show_account(sec_key, asset, is_address_eth).c(d!())?;
        }
        if address.is_some() {
            let (account, info) = contract_account_info(address, is_address_eth)?;
            println!("AccountId: {account}\n{info:#?}\n");
        }
    } else if let Some(m) = matches.subcommand_matches("contract-deposit") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        let asset = m.value_of("asset");
        let lowlevel_data = m.value_of("lowlevel-data");
        let is_address_eth = m.is_present("eth-address");
        transfer_to_account(
            amount.parse::<u64>().c(d!())?,
            address,
            asset,
            lowlevel_data,
            is_address_eth,
        )?
    } else if let Some(m) = matches.subcommand_matches("contract-withdraw") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        let eth_key = m.value_of("eth-key");
        let is_address_eth = m.is_present("eth-address");
        transfer_from_account(
            amount.parse::<u64>().c(d!())?,
            address,
            eth_key,
            is_address_eth,
        )?
    } else if let Some(m) = matches.subcommand_matches("convert-bar-to-abar") {
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;

        // the receiver Xfr address
        let target_addr = m.value_of("to-address").c(d!())?;

        // The TxoSID to be spent for conversion to ABAR (Anon Blind Asset Record)
        let txo_sid = m.value_of("txo-sid");
        let is_address_eth = m.is_present("use-default-eth-address");

        if txo_sid.is_none() {
            println!("{}", m.usage());
        } else {
            // call the convert function to build and send transaction
            // it takes owner Xfr secret key, Axfr address and TxoSID
            let r = common::convert_bar2abar(
                owner_sk.as_ref(),
                target_addr,
                txo_sid.unwrap(),
                is_address_eth,
            )
            .c(d!())?;

            // Print commitment to terminal
            println!(
                "\x1b[31;01m Commitment: {}\x1b[00m",
                wallet::commitment_to_base58(&r)
            );
            // write the commitment base64 form to the owned_commitments file
            let mut file = fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open("owned_commitments")
                .expect("cannot open commitments file");
            std::io::Write::write_all(
                &mut file,
                ("\n".to_owned() + &wallet::commitment_to_base58(&r)).as_bytes(),
            )
            .expect("commitment write failed");
        }
    } else if let Some(m) = matches.subcommand_matches("convert-abar-to-bar") {
        let is_address_eth = m.is_present("use-default-eth-address");
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;

        // get the BAR receiver address
        let to = m
            .value_of("to-pubkey")
            .c(d!())
            .and_then(wallet::public_key_from_base64)
            .or_else(|_| {
                m.value_of("to-wallet-address").c(d!()).and_then(|addr| {
                    wallet::public_key_from_bech32(addr).c(d!("invalid wallet address"))
                })
            })?;

        // get the commitments for abar conversion and anon_fee
        let commitment = m.value_of("commitment");

        if commitment.is_none() {
            println!("{}", m.usage());
        } else {
            // Build transaction and submit to network
            common::convert_abar2bar(
                owner_sk,
                commitment.unwrap(),
                &to,
                m.is_present("confidential-amount"),
                m.is_present("confidential-type"),
                is_address_eth,
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("owned-abars") {
        let is_address_eth = m.is_present("use-default-eth-address");
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;
        // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
        let from = match owner_sk {
            Some(str) => {
                ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                    .c(d!())?
                    .into_keypair()
            }
            None => get_keypair(is_address_eth).c(d!())?,
        };

        let commitments_list = m
            .value_of("commitments")
            .unwrap_or_else(|| panic!("Commitment list missing \n {}", m.usage()));

        common::get_owned_abars(from, commitments_list)?;
    } else if let Some(m) = matches.subcommand_matches("anon-balance") {
        let is_address_eth = m.is_present("use-default-eth-address");
        // Generates a list of owned Abars (both spent and unspent)
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;
        // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
        let from = match owner_sk {
            Some(str) => {
                ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                    .c(d!(str))?
                    .into_keypair()
            }
            None => get_keypair(is_address_eth).c(d!())?,
        };
        let asset = m.value_of("asset");

        let commitments_list = m
            .value_of("commitments")
            .unwrap_or_else(|| panic!("Commitment list missing \n {}", m.usage()));

        common::anon_balance(from, commitments_list, asset)?;
    } else if let Some(m) = matches.subcommand_matches("owned-open-abars") {
        let is_address_eth = m.is_present("use-default-eth-address");
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;
        // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
        let from = match owner_sk {
            Some(str) => {
                ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                    .c(d!())?
                    .into_keypair()
            }
            None => get_keypair(is_address_eth).c(d!())?,
        };
        let commitment_str = m.value_of("commitment");

        // create derived public key
        let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;

        // get results from query server and print
        let (uid, abar) = utils::get_owned_abar(&commitment).c(d!())?;
        let memo = utils::get_abar_memo(&uid).unwrap().unwrap();
        let oabar =
            OpenAnonAssetRecordBuilder::from_abar(&abar, memo, &from.into_noah())
                .unwrap()
                .build()
                .unwrap();

        println!(
            "(AtxoSID, ABAR, OABAR)   :  {}",
            serde_json::to_string(&(uid, abar, oabar)).c(d!())?
        );
    } else if let Some(m) = matches.subcommand_matches("owned-utxos") {
        // All assets are shown in the default case
        let asset = m.value_of("asset");
        let is_address_eth = m.is_present("eth-address");

        // fetch filtered list by asset
        let list = common::get_owned_utxos(asset, is_address_eth)?;
        let pk = wallet::public_key_to_base64(
            get_keypair(is_address_eth).unwrap().get_pk_ref(),
        );

        // Print UTXO table
        println!("Owned utxos for {pk:?}",);
        println!("{:-^1$}", "", 100);
        println!(
            "{0: <8} | {1: <18} | {2: <45} ",
            "ATxoSID", "Amount", "AssetType"
        );
        for (a, b, c) in list.iter() {
            let amt = b
                .get_amount()
                .map_or_else(|| "Confidential".to_string(), |a| a.to_string());
            let at = c.get_asset_type().map_or_else(
                || "Confidential".to_string(),
                |at| AssetTypeCode { val: at }.to_base64(),
            );

            println!("{0: <8} | {1: <18} | {2: <45} ", a.0, amt, at);
        }
    } else if let Some(m) = matches.subcommand_matches("anon-transfer") {
        let is_eth_address = m.is_present("use-default-eth-address");
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;

        // get commitments
        let commitment = m.value_of("commitment");
        let fee_commitment = m.value_of("fra-commitment");

        // get receiver keys and amount
        let to_address = m.value_of("to-address");
        let amount = m.value_of("amount");

        if commitment.is_none() || to_address.is_none() || amount.is_none() {
            println!("{}", m.usage());
        } else {
            // build transaction and submit
            common::gen_anon_transfer_op(
                owner_sk,
                commitment.unwrap(),
                fee_commitment,
                amount.unwrap(),
                to_address.unwrap(),
                is_eth_address,
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("anon-transfer-batch") {
        let is_eth_address = m.is_present("use-default-eth-address");

        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;

        let to_axfr_public_keys =
            m.value_of("to-address-file").c(d!()).and_then(|f| {
                fs::read_to_string(f).c(d!()).and_then(|pks| {
                    pks.lines()
                        .map(|pk| wallet::public_key_from_bech32(pk.trim()))
                        .collect::<Result<Vec<_>>>()
                        .c(d!("invalid file"))
                })
            })?;
        let mut commitments = m.value_of("commitment-file").c(d!()).and_then(|f| {
            fs::read_to_string(f)
                .c(d!())
                .map(|rms| rms.lines().map(String::from).collect::<Vec<String>>())
        })?;
        commitments.sort();
        commitments.dedup();
        let amounts = m.value_of("amount-file").c(d!()).and_then(|f| {
            fs::read_to_string(f)
                .c(d!())
                .map(|ams| ams.lines().map(String::from).collect::<Vec<String>>())
        })?;
        let assets = m.value_of("asset-file").c(d!()).and_then(|f| {
            let token_code = |asset: &str| {
                if asset.to_uppercase() == "FRA" {
                    AssetTypeCode {
                        val: ASSET_TYPE_FRA,
                    }
                } else {
                    AssetTypeCode::new_from_base64(asset).unwrap_or(AssetTypeCode {
                        val: ASSET_TYPE_FRA,
                    })
                }
            };
            fs::read_to_string(f)
                .c(d!())
                .map(|ams| ams.lines().map(token_code).collect::<Vec<AssetTypeCode>>())
        })?;

        if to_axfr_public_keys.is_empty()
            || commitments.is_empty()
            || amounts.is_empty()
            || assets.is_empty()
        {
            println!("{}", m.usage());
        } else {
            common::gen_oabar_add_op_x(
                owner_sk,
                to_axfr_public_keys,
                commitments,
                amounts,
                assets,
                is_eth_address,
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
    } else if let Some(m) = matches.subcommand_matches("check-abar-status") {
        let is_address_eth = m.is_present("use-default-eth-address");
        // sender Xfr secret key
        let owner_sk = read_file_path(m.value_of("from-seckey")).c(d!())?;
        // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
        let from = match owner_sk {
            Some(str) => {
                ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                    .c(d!())?
                    .into_keypair()
            }
            None => get_keypair(is_address_eth).c(d!())?,
        };

        let commitment_str = m.value_of("commitment");
        let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;

        let abar = utils::get_owned_abar(&commitment).c(d!())?;
        common::check_abar_status(from, abar).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("replace_staker") {
        let target = m
            .value_of("target")
            .c(d!())
            .and_then(|val| H160::from_str(val).c(d!()))?;
        let td_addr = match m.value_of("validator-td-addr") {
            Some(v) => v,
            None => {
                println!("{}", m.usage());
                return Ok(());
            }
        };
        let is_address_eth = m.is_present("eth-address");
        common::replace_staker(target, td_addr, is_address_eth)?;
    } else if let Some(m) = matches.subcommand_matches("dev") {
        #[cfg(not(target_arch = "wasm32"))]
        {
            use finutils::common::dev;

            let mut envcfg = dev::EnvCfg::default();

            let ops = if let Some(sm) = m.subcommand_matches("create") {
                let mut block_itv_secs = 3.0;
                let mut initial_validator_num = VALIDATORS_MIN as u8;
                let mut host_ip = None;
                let mut abcid_bin = None;
                let mut tendermint_bin = None;
                let mut abcid_extra_flags = None;
                let mut tendermint_extra_flags = None;
                let mut force_create = false;
                let mut evm_chain_id = 2152;
                let mut checkpoint_file = None;

                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                if let Some(id) = sm.value_of("evm_chain_id") {
                    evm_chain_id = id.parse::<u64>().c(d!())?;
                }
                if let Some(itv) = sm.value_of("block_itv_secs") {
                    block_itv_secs = itv
                        .parse::<u8>()
                        .map(|i| i as f32)
                        .c(d!())
                        .or_else(|_| itv.parse::<f32>().c(d!()))?;
                }
                if let Some(num) = sm.value_of("validator_num") {
                    initial_validator_num = num.parse::<u8>().c(d!())?;
                    if (VALIDATORS_MIN as u8) > initial_validator_num {
                        return Err(eg!(
                            "The number of initial validators should not less than {}!",
                            VALIDATORS_MIN
                        ));
                    }
                    if 64 < initial_validator_num {
                        return Err(eg!(
                            "The number of initial validators should not exceed 64!"
                        ));
                    }
                }
                if let Some(file) = sm.value_of("checkpoint_file") {
                    checkpoint_file = Some(file.to_owned());
                }
                if let Some(ip) = sm.value_of("host_ip") {
                    host_ip = Some(ip.to_owned());
                }
                if let Some(app_bin) = sm.value_of("abcid_bin_path") {
                    abcid_bin = Some(app_bin.to_owned());
                }
                if let Some(tm_bin) = sm.value_of("tendermint_bin_path") {
                    tendermint_bin = Some(tm_bin.to_owned());
                }
                if let Some(flags) = sm.value_of("abcid_extra_flags") {
                    abcid_extra_flags = Some(flags.to_owned());
                }
                if let Some(flags) = sm.value_of("tendermint_extra_flags") {
                    tendermint_extra_flags = Some(flags.to_owned());
                }
                if sm.is_present("force") {
                    force_create = true;
                }
                dev::Ops::Create {
                    block_itv_secs,
                    initial_validator_num,
                    host_ip,
                    abcid_bin,
                    tendermint_bin,
                    abcid_extra_flags,
                    tendermint_extra_flags,
                    force_create,
                    evm_chain_id,
                    checkpoint_file,
                }
            } else if let Some(sm) = m.subcommand_matches("destroy") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::Destroy
            } else if m.subcommand_matches("destroy-all").is_some() {
                dev::Ops::DestroyAll
            } else if let Some(sm) = m.subcommand_matches("start") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::Start
            } else if m.subcommand_matches("start-all").is_some() {
                dev::Ops::StartAll
            } else if let Some(sm) = m.subcommand_matches("stop") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::Stop
            } else if m.subcommand_matches("stop-all").is_some() {
                dev::Ops::StopAll
            } else if let Some(sm) = m.subcommand_matches("push-node") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::PushNode
            } else if let Some(sm) = m.subcommand_matches("pop-node") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::PopNode
            } else if let Some(sm) = m.subcommand_matches("show") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::Show
            } else if m.subcommand_matches("show-all").is_some() {
                dev::Ops::ShowAll
            } else if m.subcommand_matches("list").is_some() {
                dev::Ops::List
            } else if let Some(sm) = m.subcommand_matches("init") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::Init
            } else if m.subcommand_matches("init-all").is_some() {
                dev::Ops::InitAll
            } else {
                if let Some(name) = m.value_of("env_name") {
                    envcfg.name = name.into();
                }
                dev::Ops::default()
            };

            envcfg.ops = ops;
            envcfg.exec().map_err(|e| eg!(e))?;
        }
        let _ = m;
    } else if let Some(m) = matches.subcommand_matches("ddev") {
        #[cfg(not(target_arch = "wasm32"))]
        {
            use finutils::common::ddev;

            let mut envcfg = ddev::EnvCfg::default();

            let ops = if let Some(sm) = m.subcommand_matches("create") {
                let mut block_itv_secs = 3.0;
                let mut initial_validator_num = VALIDATORS_MIN as u8;
                let mut hosts = None;
                let mut abcid_bin = None;
                let mut tendermint_bin = None;
                let mut abcid_extra_flags = None;
                let mut tendermint_extra_flags = None;
                let mut force_create = false;
                let mut evm_chain_id = 2152;
                let mut checkpoint_file = None;

                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                if let Some(id) = sm.value_of("evm_chain_id") {
                    evm_chain_id = id.parse::<u64>().c(d!())?;
                }
                if let Some(itv) = sm.value_of("block_itv_secs") {
                    block_itv_secs = itv
                        .parse::<u8>()
                        .map(|i| i as f32)
                        .c(d!())
                        .or_else(|_| itv.parse::<f32>().c(d!()))?;
                }
                if let Some(num) = sm.value_of("validator_num") {
                    initial_validator_num = num.parse::<u8>().c(d!())?;
                    if (VALIDATORS_MIN as u8) > initial_validator_num {
                        return Err(eg!(
                            "The number of initial validators should not less than {}!",
                            VALIDATORS_MIN
                        ));
                    }
                    if 64 < initial_validator_num {
                        return Err(eg!(
                            "The number of initial validators should not exceed 64!"
                        ));
                    }
                }
                if let Some(file) = sm.value_of("checkpoint_file") {
                    checkpoint_file = Some(file.to_owned());
                }
                if let Some(hs) = sm.value_of("hosts") {
                    hosts = Some(hs.to_owned());
                }
                if let Some(app_bin) = sm.value_of("abcid_bin_path") {
                    abcid_bin = Some(app_bin.to_owned());
                }
                if let Some(tm_bin) = sm.value_of("tendermint_bin_path") {
                    tendermint_bin = Some(tm_bin.to_owned());
                }
                if let Some(flags) = sm.value_of("abcid_extra_flags") {
                    abcid_extra_flags = Some(flags.to_owned());
                }
                if let Some(flags) = sm.value_of("tendermint_extra_flags") {
                    tendermint_extra_flags = Some(flags.to_owned());
                }
                if sm.is_present("force") {
                    force_create = true;
                }
                ddev::Ops::Create {
                    block_itv_secs,
                    initial_validator_num,
                    hosts,
                    abcid_bin,
                    tendermint_bin,
                    abcid_extra_flags,
                    tendermint_extra_flags,
                    force_create,
                    evm_chain_id,
                    checkpoint_file,
                }
            } else if let Some(sm) = m.subcommand_matches("destroy") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::Destroy
            } else if m.subcommand_matches("destroy-all").is_some() {
                ddev::Ops::DestroyAll
            } else if let Some(sm) = m.subcommand_matches("start") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::Start
            } else if m.subcommand_matches("start-all").is_some() {
                ddev::Ops::StartAll
            } else if let Some(sm) = m.subcommand_matches("stop") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::Stop
            } else if m.subcommand_matches("stop-all").is_some() {
                ddev::Ops::StopAll
            } else if let Some(sm) = m.subcommand_matches("push-node") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::PushNode
            } else if let Some(sm) = m.subcommand_matches("pop-node") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::PopNode
            } else if let Some(sm) = m.subcommand_matches("show") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::Show
            } else if m.subcommand_matches("show-all").is_some() {
                ddev::Ops::ShowAll
            } else if m.subcommand_matches("list").is_some() {
                ddev::Ops::List
            } else if let Some(sm) = m.subcommand_matches("init") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::Init
            } else if m.subcommand_matches("init-all").is_some() {
                ddev::Ops::InitAll
            } else if let Some(sm) = m.subcommand_matches("host-put-file") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                let local_path = sm.value_of("local_path").c(d!())?.to_owned();
                let mut remote_path = None;
                if let Some(rp) = sm.value_of("remote_path") {
                    remote_path = Some(rp.to_owned());
                }
                let mut hosts = None;
                if let Some(hs) = sm.value_of("hosts") {
                    hosts = Some(hs.to_owned());
                }
                ddev::Ops::HostPutFile {
                    local_path,
                    remote_path,
                    hosts,
                }
            } else if let Some(sm) = m.subcommand_matches("host-get-file") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                let remote_path = sm.value_of("remote_path").c(d!())?.to_owned();
                let mut local_base_dir = None;
                if let Some(l) = sm.value_of("local_base_dir") {
                    local_base_dir = Some(l.to_owned());
                }
                let mut hosts = None;
                if let Some(hs) = sm.value_of("hosts") {
                    hosts = Some(hs.to_owned());
                }
                ddev::Ops::HostGetFile {
                    remote_path,
                    local_base_dir,
                    hosts,
                }
            } else if let Some(sm) = m.subcommand_matches("host-exec") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                let mut cmd = None;
                if let Some(c) = sm.value_of("cmd") {
                    cmd = Some(c.to_owned());
                }
                let mut script_path = None;
                if let Some(sp) = sm.value_of("script_path") {
                    script_path = Some(sp.to_owned());
                }
                let mut hosts = None;
                if let Some(hs) = sm.value_of("hosts") {
                    hosts = Some(hs.to_owned());
                }
                ddev::Ops::HostExec {
                    cmd,
                    script_path,
                    hosts,
                }
            } else if let Some(sm) = m.subcommand_matches("node-collect-logs") {
                if let Some(name) = sm.value_of("env_name") {
                    envcfg.name = name.into();
                }
                let mut local_base_dir = None;
                if let Some(l) = sm.value_of("local_base_dir") {
                    local_base_dir = Some(l.to_owned());
                }
                ddev::Ops::NodeCollectLogs { local_base_dir }
            } else {
                if let Some(name) = m.value_of("env_name") {
                    envcfg.name = name.into();
                }
                ddev::Ops::default()
            };

            envcfg.ops = ops;
            envcfg.exec().map_err(|e| eg!(e))?;
        }
        let _ = m;
    } else {
        println!("{}", matches.usage());
    }

    Ok(())
}

fn read_file_path(path: Option<&str>) -> Result<Option<String>> {
    Ok(match path {
        Some(path) => Some(
            fs::read_to_string(path)
                .c(d!("Failed to read seckey file"))?
                .trim()
                .to_string(),
        ),
        None => None,
    })
}

fn tip_fail(e: impl fmt::Display) {
    eprintln!("\n\x1b[31;01mFAIL !!!\x1b[00m");
    eprintln!(
        "\x1b[35;01mTips\x1b[01m:\n\tPlease send your error messages to us,\n\tif you can't understand their meanings ~^!^~\x1b[00m"
    );
    eprintln!("\n{e}");
}

fn tip_success() {
    eprintln!(
        "\x1b[35;01mNote\x1b[01m:\n\tYour operations has been executed without local error,\n\tbut the final result may need an asynchronous query.\x1b[00m"
    );
}
