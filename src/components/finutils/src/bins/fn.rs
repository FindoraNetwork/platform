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
    finutils::common::{
        self,
        dev::{EnvCfg, Ops},
        evm::*,
        get_keypair, utils,
    },
    fp_utils::ecdsa::SecpPair,
    globutils::wallet,
    ledger::{
        data_model::{AssetTypeCode, ASSET_TYPE_FRA, FRA_DECIMALS},
        staking::StakerMemo,
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::fs::File,
    std::{borrow::Borrow, fmt, fs},
    noah::anon_xfr::keys::AXfrKeyPair,
    noah::anon_xfr::structs::OpenAnonAssetRecordBuilder,
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
        let is_address_fra = m.is_present("fra-address");
        common::gen_key_and_print(is_address_fra);
    } else if let Some(m) = matches.subcommand_matches("wallet") {
        if m.is_present("create") {
            let is_address_fra = m.is_present("fra-address");
            common::gen_key_and_print(is_address_fra);
        } else if m.is_present("show") {
            let seckey = match m.value_of("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let is_address_fra = m.is_present("fra-address");

            // FRA asset is the default case
            let asset = if let Some(code) = m.value_of("asset") {
                match code.to_lowercase().as_str() {
                    "fra" => None,
                    _ => Some(code),
                }
            } else {
                None
            };
            common::show_account(seckey.as_deref(), asset, is_address_fra).c(d!())?;
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
        let is_address_fra = m.is_present("fra-address");

        if amount.is_some() && validator.is_some() {
            common::delegate(
                seckey.as_deref(),
                amount.unwrap().parse::<u64>().c(d!())?,
                validator.unwrap(),
                is_address_fra,
            )
            .c(d!())?;
        } else if show_info {
            common::show_delegations(seckey.as_deref(), is_address_fra).c(d!())?;
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
        let is_address_fra = m.is_present("fra-address");
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
        common::undelegate(seckey.as_deref(), param, is_address_fra).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("asset") {
        if m.is_present("create") {
            let memo = m.value_of("memo");
            if memo.is_none() {
                println!("{}", m.usage());
                return Ok(());
            }
            let transferable = m.is_present("transferable");
            let is_address_fra = m.is_present("fra-address");

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
                memo.unwrap(),
                decimal,
                max_units,
                transferable,
                token_code,
                is_address_fra,
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
            let is_address_fra = m.is_present("fra-address");

            common::issue_asset(
                seckey.as_deref(),
                code.unwrap(),
                amount,
                hidden,
                is_address_fra,
            )
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
        let is_address_fra = m.is_present("fra-address");

        let cr = m.value_of("commission-rate");
        if vm.is_none() && cr.is_none() {
            println!("{}", m.usage());
            println!(
                "Tips: to update the information of your node, please specify commission-rate or memo"
            );
        } else {
            common::staker_update(cr, vm, is_address_fra).c(d!())?;
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
            let is_address_fra = m.is_present("fra-address");
            if am.is_none() {
                println!("{}", m.usage());
            } else {
                common::stake_append(
                    am.unwrap(),
                    staker.as_deref(),
                    td_addr,
                    is_address_fra,
                )
                .c(d!())?;
            }
        } else {
            let cr = m.value_of("commission-rate");
            let vm = m.value_of("validator-memo");
            let force = m.is_present("force");
            let is_address_fra = m.is_present("fra-address");
            if am.is_none() || cr.is_none() {
                println!("{}", m.usage());
                println!(
                    "Tips: if you want to raise the power of your node, please use `fn stake --append [OPTIONS]`"
                );
            } else {
                common::stake(am.unwrap(), cr.unwrap(), vm, force, is_address_fra)
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
        let is_address_fra = m.is_present("fra-address");
        common::unstake(am, staker.as_deref(), td_addr, is_address_fra).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("claim") {
        let am = m.value_of("amount");
        let is_address_fra = m.is_present("fra-address");
        let seckey = match m.value_of("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        common::claim(am, seckey.as_deref(), is_address_fra).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("show") {
        let basic = m.is_present("basic");
        let is_address_fra = m.is_present("fra-address");
        common::show(basic, is_address_fra).c(d!())?;
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
        let is_address_fra = m.is_present("fra-address");

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
                is_address_fra,
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
        let is_address_fra = m.is_present("fra-address");

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
                is_address_fra,
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
        let is_address_fra = m.is_present("fra-address");

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
            //Asset defaults to fra
            common::show_account(sec_key, asset, is_address_fra).c(d!())?;
        }
        if address.is_some() {
            let (account, info) = contract_account_info(address, is_address_fra)?;
            println!("AccountId: {}\n{:#?}\n", account, info);
        }
    } else if let Some(m) = matches.subcommand_matches("contract-deposit") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        let asset = m.value_of("asset");
        let lowlevel_data = m.value_of("lowlevel-data");
        let is_address_fra = m.is_present("fra-address");
        transfer_to_account(
            amount.parse::<u64>().c(d!())?,
            asset,
            address,
            lowlevel_data,
            is_address_fra,
        )?
    } else if let Some(m) = matches.subcommand_matches("contract-withdraw") {
        let amount = m.value_of("amount").c(d!())?;
        let address = m.value_of("addr");
        let eth_key = m.value_of("eth-key");
        let is_address_fra = m.is_present("fra-address");
        transfer_from_account(
            amount.parse::<u64>().c(d!())?,
            address,
            eth_key,
            is_address_fra,
        )?
    } else if let Some(m) = matches.subcommand_matches("convert-bar-to-abar") {
        // sender Xfr secret key
        let f = read_file_path(m.value_of("from-seckey")).c(d!())?;
        let owner_sk = f.as_ref();

        // receiver AXfr secret key parsed & the receiver Axfr address
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let target_addr = anon_keys.pub_key;

        // The TxoSID to be spent for conversion to ABAR(Anon Blind Asset Record)
        let txo_sid = m.value_of("txo-sid");
        let is_address_fra = m.is_present("fra-address");

        if txo_sid.is_none() {
            println!("{}", m.usage());
        } else {
            // call the convert function to build and send transaction
            // it takes owner Xfr secret key, Axfr address and TxoSID
            let r = common::convert_bar2abar(
                owner_sk,
                target_addr,
                txo_sid.unwrap(),
                is_address_fra,
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
        // get anon keys for conversion
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let spend_key = anon_keys.spend_key;
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
                spend_key,
                commitment.unwrap(),
                &to,
                m.is_present("confidential-amount"),
                m.is_present("confidential-type"),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("gen-anon-keys") {
        let mut prng = ChaChaRng::from_entropy();
        let keypair = AXfrKeyPair::generate(&mut prng);

        let keys = AnonKeys {
            spend_key: wallet::anon_secret_key_to_base64(&keypair),
            pub_key: wallet::anon_public_key_to_base64(&keypair.get_public_key()),
        };

        if let Some(path) = m.value_of("file-path") {
            serde_json::to_writer_pretty(&File::create(path).c(d!())?, &keys).c(d!())?;
            println!("Keys saved to file: {}", path);
        }

        // print keys to terminal
        println!("Keys :\n {}", serde_json::to_string_pretty(&keys).unwrap());
    } else if let Some(m) = matches.subcommand_matches("owned-abars") {
        // Generates a list of owned Abars (both spent and unspent)
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let spend_key =
            wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str()).c(d!())?;

        let commitments_list = m
            .value_of("commitments")
            .unwrap_or_else(|| panic!("Commitment list missing \n {}", m.usage()));

        common::get_owned_abars(spend_key, commitments_list)?;
    } else if let Some(m) = matches.subcommand_matches("anon-balance") {
        // Generates a list of owned Abars (both spent and unspent)
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let spend_key =
            wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str()).c(d!())?;
        let asset = m.value_of("asset");

        let commitments_list = m
            .value_of("commitments")
            .unwrap_or_else(|| panic!("Commitment list missing \n {}", m.usage()));

        common::anon_balance(spend_key, commitments_list, asset)?;
    } else if let Some(m) = matches.subcommand_matches("owned-open-abars") {
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let commitment_str = m.value_of("commitment");

        // create derived public key
        let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;
        let spend_key =
            wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str()).c(d!())?;

        // get results from query server and print
        let (uid, abar) = utils::get_owned_abar(&commitment).c(d!())?;
        let memo = utils::get_abar_memo(&uid).unwrap().unwrap();
        let oabar = OpenAnonAssetRecordBuilder::from_abar(&abar, memo, &spend_key)
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
        let is_address_fra = m.is_present("fra-address");

        // fetch filtered list by asset
        let list = common::get_owned_utxos(asset, is_address_fra)?;
        let pk = wallet::public_key_to_base64(
            get_keypair(is_address_fra).unwrap().pub_key.borrow(),
        );

        // Print UTXO table
        println!("Owned utxos for {:?}", pk);
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
        // get anon keys of sender
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let secret_key = anon_keys.spend_key;

        // get commitments
        let commitment = m.value_of("commitment");
        let fee_commitment = m.value_of("fra-commitment");

        // get receiver keys and amount
        let to_axfr_public_key = m.value_of("to-axfr-public-key");
        let amount = m.value_of("amount");

        if commitment.is_none() || to_axfr_public_key.is_none() || amount.is_none() {
            println!("{}", m.usage());
        } else {
            // build transaction and submit
            common::gen_anon_transfer_op(
                secret_key,
                commitment.unwrap(),
                fee_commitment,
                amount.unwrap(),
                to_axfr_public_key.unwrap(),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("anon-transfer-batch") {
        // get anon keys of sender
        let anon_keys = parse_anon_key_from_path(m.value_of("anon-keys"))?;
        let secret_key =
            wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str()).c(d!())?;

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
        let commitments = m.value_of("commitment-file").c(d!()).and_then(|f| {
            fs::read_to_string(f)
                .c(d!())
                .map(|rms| rms.lines().map(String::from).collect::<Vec<String>>())
        })?;
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
                secret_key,
                to_axfr_public_keys,
                commitments,
                amounts,
                assets,
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
        let anon_keys = match m.value_of("anon-keys") {
            Some(path) => {
                let f =
                    fs::read_to_string(path).c(d!("Failed to read anon-keys file"))?;
                serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?
            }
            None => return Err(eg!("path for anon-keys file not found")),
        };
        let commitment_str = m.value_of("commitment");
        let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;

        let spend_key =
            wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str()).c(d!())?;

        let abar = utils::get_owned_abar(&commitment).c(d!())?;
        common::check_abar_status(spend_key, abar).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("replace_staker") {
        let target = m
            .value_of("target")
            .c(d!())
            .and_then(wallet::public_key_from_base64)?;

        let is_address_fra = m.is_present("fra-address");
        // let new_td_addr_pk = if let Some(new_td_address_str) = m.value_of("td_address") {
        //     let new_td_address = hex::decode(new_td_address_str)
        //         .c(d!("`td_address` is invalid hex. "))?;

        //     if new_td_address.len() != 20 {
        //         return Err(eg!("Invalid tendermint address."));
        //     }

        //     if let Some(new_td_pk) = m.value_of("td_pubkey") {
        //         let pk_bytes =
        //             base64::decode(new_td_pk).c(d!("`td_pubkey` is invalid base64."))?;

        //         let _ = tendermint::PublicKey::from_raw_ed25519(&pk_bytes)
        //             .c(d!("Invalid tendermint public key."))?;

        //         Some((new_td_address, pk_bytes))
        //     } else {
        //         return Err(eg!("missing `td_pubkey`"));
        //     }
        // } else {
        //     None
        // };
        common::replace_staker(target, None, is_address_fra)?;
    } else if let Some(m) = matches.subcommand_matches("dev") {
        let mut envcfg = EnvCfg::default();

        let ops = if let Some(sm) = m.subcommand_matches("create") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            if let Some(id) = sm.value_of("evm_chain_id") {
                envcfg.evm_chain_id = id.parse::<u64>().c(d!())?;
            }
            if let Some(itv) = sm.value_of("block_itv_secs") {
                envcfg.block_itv_secs = itv.parse::<u8>().c(d!())?;
            }
            if let Some(num) = sm.value_of("validator_num") {
                envcfg.initial_validator_num = num.parse::<u8>().c(d!())?;
                if 64 < envcfg.initial_validator_num {
                    return Err(eg!(
                        "The number of initial validators should not exceed 64!"
                    ));
                }
            }
            if let Some(file) = sm.value_of("checkpoint_file") {
                envcfg.checkpoint_file = Some(file.to_owned());
            }
            if let Some(ip) = sm.value_of("host_ip") {
                envcfg.host_ip = Some(ip.to_owned());
            }
            if let Some(abcid_bin) = sm.value_of("abcid_bin_path") {
                envcfg.abcid_bin = Some(abcid_bin.to_owned());
            }
            if let Some(tm_bin) = sm.value_of("tendermint_bin_path") {
                envcfg.tendermint_bin = Some(tm_bin.to_owned());
            }
            if let Some(flags) = sm.value_of("abcid_extra_flags") {
                envcfg.abcid_extra_flags = Some(flags.to_owned());
            }
            if let Some(flags) = sm.value_of("tendermint_extra_flags") {
                envcfg.tendermint_extra_flags = Some(flags.to_owned());
            }
            if sm.is_present("force") {
                envcfg.force_create = true;
            }
            Ops::Create
        } else if let Some(sm) = m.subcommand_matches("destroy") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Destroy
        } else if m.subcommand_matches("destroy-all").is_some() {
            Ops::DestroyAll
        } else if let Some(sm) = m.subcommand_matches("start") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Start
        } else if m.subcommand_matches("start-all").is_some() {
            Ops::StartAll
        } else if let Some(sm) = m.subcommand_matches("stop") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Stop
        } else if m.subcommand_matches("stop-all").is_some() {
            Ops::StopAll
        } else if let Some(sm) = m.subcommand_matches("push-node") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::PushNode
        } else if let Some(sm) = m.subcommand_matches("pop-node") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::PopNode
        } else if let Some(sm) = m.subcommand_matches("show") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Show
        } else if m.subcommand_matches("show-all").is_some() {
            Ops::ShowAll
        } else if m.subcommand_matches("list").is_some() {
            Ops::List
        } else if let Some(sm) = m.subcommand_matches("init") {
            if let Some(name) = sm.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Init
        } else if m.subcommand_matches("init-all").is_some() {
            Ops::InitAll
        } else {
            if let Some(name) = m.value_of("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::default()
        };

        envcfg.ops = ops;
        envcfg.exec().c(d!())?;
    } else {
        println!("{}", matches.usage());
    }

    Ok(())
}

fn read_file_path(path: Option<&str>) -> Result<Option<String>> {
    Ok(match path {
        Some(path) => {
            Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
        }
        None => None,
    })
}

fn parse_anon_key_from_path(path: Option<&str>) -> Result<AnonKeys> {
    let f = read_file_path(path).c(d!())?;
    if f.is_none() {
        return Err(eg!("Anon keypair path not found"));
    }

    serde_json::from_str::<AnonKeys>(f.unwrap().as_str()).c(d!())
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
    pub spend_key: String,
    pub pub_key: String,
}
