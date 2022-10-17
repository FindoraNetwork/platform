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
    clap::{arg, ArgGroup, Command},
    finutils::common::{
        self,
        dev::{EnvCfg, Ops},
        evm::*,
    },
    fp_utils::ecdsa::SecpPair,
    globutils::wallet,
    ledger::{
        data_model::{AssetTypeCode, FRA_DECIMALS},
        staking::StakerMemo,
    },
    ruc::*,
    std::{fmt, fs},
};

fn main() {
    if let Err(e) = run() {
        tip_fail(e);
    } else {
        tip_success();
    }
}

fn run() -> Result<()> {
    let subcmd_genkey = Command::new("genkey")
        .about("Generate a random Findora public key/private key Pair");
    let mut subcmd_show = Command::new("show")
        .about("View Validator status and accumulated rewardsr")
        .args([arg!(-b --basic "show basic account info")]);
    let mut subcmd_setup =
        Command::new("setup")
        .about("Setup environment variables for staking transactions")
        .args([
            arg!(-S --"serv-addr" <"URL/IP"> "a node address of Findora Network"),
            arg!(-O --"owner-mnemonic-path" <"Path"> "storage path of your mnemonic words"),
            arg!(-K --"validator-key" <"Path"> "path to the tendermint keys of your validator node"),
        ]);
    let mut subcmd_stake = Command::new("stake")
            .about("Stake tokens (i.e. bond tokens) from a Findora account to a Validator")
            .args([
                arg!(-n --"amount" <"Amount"> "how much `FRA unit`s you want to stake").required(true),
                arg!(-R --"commission-rate" <"Rate"> "the commission rate of your node, a float number from 0.0 to 1.0"),
                arg!(-M --"validator-memo" <"Memo"> "the description of your node, optional"),
                arg!(-a --"append" <"Amount"> "stake more FRAs to your node"),
                arg!(-S --"staker-priv-key" <"SecretKey"> "the file which contains private key (in base64 format) of proposer"),
                arg!(-A --"validator-td-addr" <"TendermintAddr"> "stake FRAs to a custom validator"),
                arg!(--"force" "ignore warning and stake FRAs to your target node") ,
            ])
            .group(ArgGroup::new("staking-flags").multiple(true).conflicts_with("amount"))
            .next_help_heading("STAKING-FLAGS")
            .args([
                arg!(-R --"commission-rate" <"Rate"> "the commission rate of your node, a float number from 0.0 to 1.0"),
                arg!(-M --"validator-memo" <"Memo"> "the description of your node, optional"),
            ]);
    let mut subcmd_staker_update = Command::new("staker-update")
        .about("Update information of a validator")
        .args([
            arg!(-R --"commission-rate" <"Rate"> "the commission rate of your node, a float number from 0.0 to 1.0"),
            arg!(-M --"validator-memo" <"Memo"> "the description of your node, optional"),
            arg!(--"validator-memo-name" <"Name">),
            arg!(--"validator-memo-desc" <"Desc">),
            arg!(--"validator-memo-website" <"Website">),
            arg!(--"validator-memo-logo" <"Logo">),
        ]);
    let subcmd_unstake = Command::new("unstake")
        .about("Update information of a validator")
        .args([
            arg!(-S --"staker-priv-key" <SecretKey> "the file which contains private key (in base64 format) of proposer"),
            arg!(-A --"validator-td-addr" <TendermintAddr> "unstake FRAs to a custom validator"),
            arg!(-n --"amount" <Amount> "how much FRA to unstake, needed for partial undelegation"),
        ]);
    let subcmd_claim = Command::new("claim")
        .about("Claim accumulated FRA rewards")
        .args([
            arg!(-n --"amount" <Amount> "how much `FRA unit`s to claim"),
            arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
        ]);
    let mut subcmd_delegate = Command::new("delegate")
        .about("Delegating operations")
        .args([
            arg!(-n --"amount" <Amount> "how much FRA units to be delegated"),
            arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
            arg!(--"validator" <"VALIDATOR ADDRESS"> "the address of a validator"),
            arg!(--"info" "show delegation info").conflicts_with("amount").conflicts_with("validator"),
        ]);
    let mut subcmd_undelegate = Command::new("undelegate")
        .about("Undelegating operations")
        .args([
            arg!(-n --"amount" <Amount> "how much FRA units to be delegated"),
            arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
            arg!(--"validator" <"VALIDATOR ADDRESS"> "the address of a validator"),
        ]);
    let mut subcmd_transfer = Command::new("transfer")
        .about("Transfer tokens from one address to another")
        .args([
            arg!(--"asset" <ASSET> "asset code which you want to tansfer"),
            arg!(-f --"from-seckey" <SecKey> "the file which contains base64-formated `XfrPrivateKey` of the receiver"),
            arg!(-t --"to-pubkey" <PubKey> "base64-formated `XfrPublicKey` of the receiver"),
            arg!(-T --"to-wallet-address" <"Wallet Address"> "fra prefixed address of FindoraNetwork").conflicts_with("to-pubkey"),
            arg!(-n --"amount" <Amount> "how much units to transfer").required(true),
            arg!(--"confidential-amount""mask the amount sent on the transaction log"),
            arg!(--"confidential-type" "mask the asset type sent on the transaction log"),
         ]);
    let mut subcmd_transfer_batch = Command::new("transfer-batch")
        .about("Transfer tokens from one address to many others")
        .args([
            arg!(-f --"from-seckey" <SecKey> "the file which contains base64-formated `XfrPrivateKey` of the receiver"),
            arg!(-t --"to-pubkey-file" <"File Path">),
            arg!(-T --"to-wallet-address-file" <"File Path">).conflicts_with("to-pubkey-file"),
            arg!(-n --"amount" <Amount> "how much units to transfer").required(true),
            arg!(--"confidential-amount""mask the amount sent on the transaction log"),
            arg!(--"confidential-type" "mask the asset type sent on the transaction log"),
         ]);
    let mut subcmd_wallet = Command::new("wallet")
        .about("manipulates a findora wallet")
        .args([
            arg!(--create "create a new findora wallet"),
            arg!(--show "show information of a findora wallet").conflicts_with("create"),
            arg!(--asset "code of asset, such as `fra`").conflicts_with("create"),
            arg!(--seckey <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet").conflicts_with("create"),
        ]);
    let mut subcmd_asset = Command::new("asset")
        .about("manipulate custom asset")
        .group(ArgGroup::new("flags"))
        .next_help_heading("FLAGS")
        .args([
            arg!(--create "create a new asset")
                .conflicts_with("issue")
                .conflicts_with("show")
                .conflicts_with("amount")
                .conflicts_with("hidden")
                .conflicts_with("addr")
                .conflicts_with("maximum"),
            arg!(--issue "issue a asset on ledger")
                .conflicts_with("create")
                .conflicts_with("show")
                .conflicts_with("decimal")
                .conflicts_with("memo")
                .conflicts_with("transferable")
                .conflicts_with("addr"),
            arg!(--show "show list of assets")
                .conflicts_with("create")
                .conflicts_with("issue")
                .conflicts_with("seckey")
                .conflicts_with("decimal")
                .conflicts_with("transferable")
                .conflicts_with("maximum")
                .conflicts_with("memo")
                .conflicts_with("amount")
                .conflicts_with("hidden")
                .conflicts_with("code"),
        ])
        .args([
            arg!(--create "create a new asset")
                .conflicts_with("issue")
                .conflicts_with("show"),
            arg!(--issue "issue a asset on ledger")
                .conflicts_with("create")
                .conflicts_with("show"),
            arg!(--show "show list of assets")
                .conflicts_with("create")
                .conflicts_with("issue"),
            arg!(-u --user <InterUserval> ""),
            arg!(--create "create a new asset"),
            arg!(--issue "issue a asset on ledger"),
            arg!(--show "show list of assets"),
            arg!(--code <"ASSET CODE"> "Custom asset type code"),
            arg!(--addr <"WALLET ADDRESS"> "Findora wallet address"),
            arg!(--seckey <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of findora account"),
            arg!(--decimal <DECIMAL>"asset decimals of a new asset"),
            arg!(--transferable "transferability type of a new asset"),
            arg!(--maximum <"MAXIMUM AMOUNT"> "maximum amount of a new asset"),
            arg!(--memo <MEMO> "asset memo of a new asset"),
            arg!(--amount <AMOUNT> "amount when issuing a asset"),
            arg!(--hidden "hidden asset amount when issuing asset on ledger"),
        ]);
    let subcmd_account = Command::new("account")
        .about("Return user contract account information")
        .args([arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true)]);
    let subcmd_contract_deposit = Command::new("contract-deposit")
        .about("Transfer FRA from a Findora account to the specified Ethereum address")
        .args([
            arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true),
            arg!(-n --amount <AMOUNT> "amount when issuing a asset").required(true),
        ]);
    let subcmd_contract_withdraw = Command::new("contract-withdraw")
        .about("Transfer FRA from an Ethereum address to the specified Findora account")
        .args([
            arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true),
            arg!(-n --amount <AMOUNT> "amount when issuing a asset").required(true),
            arg!(-e --"eth-key" <MNEMONIC> "ethereum account mnemonic phrase sign withdraw tx").required(true)
        ]);
    let subcmd_gen_eth_key =
        Command::new("gen-eth-key").about("Generate an Ethereum address");
    let subcmd_replace_staker = Command::new("replace_staker")
        .about("Replace the staker of the validator with target address")
        .args([
            arg!(-t --target <"TARGET PUBLIC KEY"> "the public key of new staker, you must be the staker of the validator, you could use `fn setup` to configure your secret key and public key").required(true),
            arg!(--"td_address" <"TENDERMINT ADDRESS"> "the tendermint address that you may want to replace.").required(false),
            arg!(--"td_pubkey" <"TENDERMINT PUBKEY"> "the tendermint public key that you may want to replace.").required(false),
        ]);
    let subcmd_dev = Command::new("dev")
        .about("Manage development clusters on your localhost")
        .args([
            arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                .required(false),
        ])
        .subcommand(
            Command::new("create").about("Create a new env").args([
                arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                    .required(false),
                arg!(-i --"block-itv-secs" <"BLOCK INTERVAL"> "Block interval in seconds")
                    .required(false),
                arg!(-N --"validator-num" <"VALIDATOR NUMBER"> "How many initial validators should be created")
                    .required(false),
                arg!(-I --"evm-chain-id" <"EVM CHAIN ID"> "The chain id in the scope of evm logic")
                    .required(false),
                arg!(-c --"checkpoint-file" <"CHECKPOINT FILE"> "The file path of the checkpoint file")
                    .required(false),
                arg!(-H --"host-ip" <"HOST IP"> "The IP of your local host, default to 127.0.0.1")
                    .required(false),
                arg!(-d --"abcid-bin-path" <"ABCID BIN PATH"> "The path of your custom abcid binary")
                    .required(false),
                arg!(-D --"tendermint-bin-path" <"TENDERMINT BIN PATH"> "The path of your custom tendermint binary")
                    .required(false),
                arg!(-x --"abcid-extra-flags" <"ABCID EXTRA FLAGS"> "A pair of quotes should be used when specifying extra flags")
                    .required(false),
                arg!(-X --"tendermint-extra-flags" <"TENDERMINT EXTRA FLAGS"> "A pair of quotes should be used when specifying extra flags")
                    .required(false),
                arg!(-f --"force" <"ENV NAME"> "destroy the target ENV and create a new one")
                    .required(false),
            ]),
        )
        .subcommand(
            Command::new("destroy")
                .about("Destroy an existing env")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(Command::new("destroy-all").about("Destroy all existing ENVs"))
        .subcommand(
            Command::new("start").about("Start an existing env").args([
                arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                    .required(false),
            ]),
        )
        .subcommand(Command::new("start-all").about("Start all existing ENVs"))
        .subcommand(
            Command::new("stop").about("Stop an existing env").args([
                arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                    .required(false),
            ]),
        )
        .subcommand(
            Command::new("stop-all")
                .about("Stop all existing ENVs")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(
            Command::new("push-node")
                .about("Attach a new node to an existing env")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(
            Command::new("pop-node")
                .about("Pop a node from an existing env")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(
            Command::new("show")
                .about("Default operation, show the information of an existing env"),
        )
        .subcommand(
            Command::new("show-all").about("Show the details of all existing ENVs"),
        )
        .subcommand(
            Command::new("list")
                .about("List the names of all existing ENVs")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(
            Command::new("init")
                .about("Config the initial settings(POS,FRA issuance...)")
                .args([
                    arg!(-e --"env-name" <"ENV NAME"> "The name of the target env")
                        .required(false),
                ]),
        )
        .subcommand(
            Command::new("init-all")
                .about("Apply the `init` operation to all existing ENVs"),
        );

    let mut cmd = Command::new("fn")
        .version(env!("VERGEN_SHA"))
        .author("develop@FindoraNetwork.org")
        .about("A command line tool of the Findora Network")
        .subcommand(subcmd_genkey)
        .subcommand(subcmd_show.clone())
        .subcommand(subcmd_setup.clone())
        .subcommand(subcmd_stake.clone())
        .subcommand(subcmd_staker_update.clone())
        .subcommand(subcmd_unstake)
        .subcommand(subcmd_claim)
        .subcommand(subcmd_delegate.clone())
        .subcommand(subcmd_undelegate.clone())
        .subcommand(subcmd_transfer.clone())
        .subcommand(subcmd_transfer_batch.clone())
        .subcommand(subcmd_wallet.clone())
        .subcommand(subcmd_asset.clone())
        .subcommand(subcmd_account)
        .subcommand(subcmd_contract_deposit)
        .subcommand(subcmd_contract_withdraw)
        .subcommand(subcmd_gen_eth_key)
        .subcommand(subcmd_replace_staker)
        .subcommand(subcmd_dev);

    let matches = cmd.clone().get_matches();

    if matches.subcommand_matches("genkey").is_some() {
        common::gen_key_and_print();
    } else if let Some(m) = matches.subcommand_matches("wallet") {
        if m.get_flag("create") {
            common::gen_key_and_print();
        } else if m.get_flag("show") {
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            // FRA asset is the default case
            let asset = if let Some(code) = m.get_one::<String>("asset") {
                match code.to_lowercase().as_str() {
                    "fra" => None,
                    _ => Some(code.as_str()),
                }
            } else {
                None
            };
            common::show_account(seckey.as_deref(), asset).c(d!())?;
        } else {
            subcmd_wallet.print_help().unwrap();
        }
    } else if let Some(m) = matches.subcommand_matches("delegate") {
        let seckey = match m.get_one::<String>("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let amount = m.get_one::<String>("amount");
        let validator = m.get_one::<String>("validator");
        let show_info = m.get_flag("info");

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
            subcmd_delegate.print_help().unwrap();
        }
    } else if let Some(m) = matches.subcommand_matches("undelegate") {
        let seckey = match m.get_one::<String>("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let amount = m.get_one::<String>("amount");
        let validator = m.get_one::<String>("validator");
        if (amount.is_none() && validator.is_some())
            || (amount.is_some() && validator.is_none())
        {
            subcmd_undelegate.print_help().unwrap();
            return Ok(());
        }
        let param = if amount.is_some() {
            Some((
                amount.unwrap().parse::<u64>().c(d!())?,
                validator.unwrap().as_str(),
            ))
        } else {
            None
        };
        common::undelegate(seckey.as_deref(), param).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("asset") {
        if m.get_flag("create") {
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let memo = m.get_one::<String>("memo");
            if memo.is_none() {
                subcmd_asset.print_help().unwrap();
                return Ok(());
            }
            let transferable = m.get_flag("transferable");
            let decimal = if let Some(num) = m.get_one::<String>("decimal") {
                num.parse::<u8>()
                    .c(d!("decimal should be an 8-bits unsinged integer"))?
            } else {
                FRA_DECIMALS
            };
            let max_units = if let Some(max) = m.get_one::<String>("maximum") {
                Some(
                    max.parse::<u64>()
                        .c(d!("maximum should be an unsigned integer"))?,
                )
            } else {
                None
            };
            let token_code = m.get_one::<String>("code").map(|val| val.as_str());
            common::create_asset(
                seckey.as_deref(),
                memo.unwrap(),
                decimal,
                max_units,
                transferable,
                token_code,
            )
            .c(d!())?;
        } else if m.get_flag("show") {
            let addr = m.get_one::<String>("addr");
            if addr.is_none() {
                subcmd_show.print_help().unwrap();
                return Ok(());
            } else {
                common::show_asset(addr.unwrap()).c(d!())?;
            }
        } else if m.get_flag("issue") {
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let code = m.get_one::<String>("code");
            let amount = m.get_one::<String>("amount");
            if code.is_none() || amount.is_none() {
                subcmd_asset.print_help().unwrap();
                return Ok(());
            }
            let amount = amount
                .unwrap()
                .parse::<u64>()
                .c(d!("amount should be a 64-bits unsigned integer"))?;
            let hidden = m.get_flag("hidden");

            common::issue_asset(seckey.as_deref(), code.unwrap(), amount, hidden)
                .c(d!())?;
        } else {
            let help = "fn asset [--create | --issue | --show]";
            println!("{}", help);
        }
    } else if let Some(m) = matches.subcommand_matches("staker-update") {
        let vm = if let Some(memo) = m.get_one::<String>("validator-memo") {
            Some(serde_json::from_str(memo).c(d!())?)
        } else {
            match (
                m.get_one::<String>("validator-memo-name"),
                m.get_one::<String>("validator-memo-desc"),
                m.get_one::<String>("validator-memo-website"),
                m.get_one::<String>("validator-memo-logo"),
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

        let cr = m
            .get_one::<String>("commission-rate")
            .map(|val| val.as_str());
        if vm.is_none() && cr.is_none() {
            subcmd_staker_update.print_help().unwrap();
            println!(
                "Tips: to update the information of your node, please specify commission-rate or memo"
            );
        } else {
            common::staker_update(cr, vm).c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("stake") {
        let am = m.get_one::<String>("amount");
        if m.get_flag("append") {
            let staker = match m.get_one::<String>("staker-priv-key") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let td_addr = m
                .get_one::<String>("validator-td-addr")
                .map(|val| val.as_str());
            if am.is_none() {
                subcmd_stake.print_help().unwrap();
            } else {
                common::stake_append(am.unwrap(), staker.as_deref(), td_addr).c(d!())?;
            }
        } else {
            let cr = m
                .get_one::<String>("commission-rate")
                .map(|val| val.as_str());
            let vm = m
                .get_one::<String>("validator-memo")
                .map(|val| val.as_str());
            let force = m.get_flag("force");
            if am.is_none() || cr.is_none() {
                subcmd_stake.print_help().unwrap();
                println!(
                    "Tips: if you want to raise the power of your node, please use `fn stake --append [OPTIONS]`"
                );
            } else {
                common::stake(am.unwrap(), cr.unwrap(), vm, force).c(d!())?;
            }
        }
    } else if let Some(m) = matches.subcommand_matches("unstake") {
        let am = m.get_one::<String>("amount").map(|val| val.as_str());
        let staker = match m.get_one::<String>("staker-priv-key") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let td_addr = m
            .get_one::<String>("validator-td-addr")
            .map(|val| val.as_str());
        common::unstake(am, staker.as_deref(), td_addr).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("claim") {
        let am = m.get_one::<String>("amount").map(|val| val.as_str());
        let seckey = match m.get_one::<String>("seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        common::claim(am, seckey.as_deref()).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("show") {
        let basic = m.get_flag("basic");
        common::show(basic).c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("setup") {
        let sa = m.get_one::<String>("serv-addr").map(|val| val.as_str());
        let om = m
            .get_one::<String>("owner-mnemonic-path")
            .map(|val| val.as_str());
        let tp = m.get_one::<String>("validator-key").map(|val| val.as_str());
        if sa.is_none() && om.is_none() && tp.is_none() {
            subcmd_setup.print_help().unwrap();
        } else {
            common::setup(sa, om, tp).c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("transfer") {
        let f = match m.get_one::<String>("from-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let asset = m
            .get_one::<String>("asset")
            .map(|val| val.as_str())
            .unwrap_or("FRA");
        let t = m
            .get_one::<String>("to-pubkey")
            .c(d!())
            .and_then(|pk| wallet::public_key_from_base64(pk).c(d!()))
            .or_else(|_| {
                m.get_one::<String>("to-wallet-address")
                    .c(d!())
                    .and_then(|addr| {
                        wallet::public_key_from_bech32(addr)
                            .c(d!("invalid wallet address"))
                    })
            })?;
        let am = m.get_one::<String>("amount");

        if am.is_none() {
            subcmd_transfer.print_help().unwrap();
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
                m.get_flag("confidential-amount"),
                m.get_flag("confidential-type"),
            )
            .c(d!())?;
        }
    } else if let Some(m) = matches.subcommand_matches("transfer-batch") {
        let f = match m.get_one::<String>("from-seckey") {
            Some(path) => {
                Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
            }
            None => None,
        };
        let t = m
            .get_one::<String>("to-pubkey-file")
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
                m.get_one::<String>("to-wallet-address-file")
                    .c(d!())
                    .and_then(|f| {
                        fs::read_to_string(f).c(d!()).and_then(|addrs| {
                            addrs
                                .lines()
                                .map(|addr| wallet::public_key_from_bech32(addr.trim()))
                                .collect::<Result<Vec<_>>>()
                                .c(d!("invalid file"))
                        })
                    })
            })?;
        let am = m.get_one::<String>("amount");

        if am.is_none() || t.is_empty() {
            subcmd_transfer_batch.print_help().unwrap();
        } else {
            common::transfer_asset_batch(
                f.as_deref(),
                &t,
                None,
                am.unwrap(),
                m.get_flag("confidential-amount"),
                m.get_flag("confidential-type"),
            )
            .c(d!())?;
        }
    } else if matches.subcommand_matches("gen-eth-key").is_some() {
        let (pair, phrase, _) = SecpPair::generate_with_phrase(None);
        let kp = hex::encode(pair.seed());
        println!(
            "\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mPrivateKey:\x1b[00m {}\n\x1b[31;01mAddress:\x1b[00m {}\n",
            phrase,
            kp,
            eth_checksum::checksum(&format!("{:?}", pair.address()))
        );
    } else if let Some(m) = matches.subcommand_matches("account") {
        let address = m.get_one::<String>("addr").map(|val| val.as_str());
        let (account, info) = contract_account_info(address)?;
        println!("AccountId: {}\n{:#?}\n", account, info);
    } else if let Some(m) = matches.subcommand_matches("contract-deposit") {
        let amount = m
            .get_one::<String>("amount")
            .map(|val| val.as_str())
            .c(d!())?;
        let address = m.get_one::<String>("addr").map(|val| val.as_str());
        transfer_to_account(amount.parse::<u64>().c(d!())?, address)?
    } else if let Some(m) = matches.subcommand_matches("contract-withdraw") {
        let amount = m.get_one::<String>("amount").c(d!())?;
        let address = m.get_one::<String>("addr").map(|val| val.as_str());
        let eth_key = m.get_one::<String>("eth-key").map(|val| val.as_str());
        transfer_from_account(amount.parse::<u64>().c(d!())?, address, eth_key)?
    } else if let Some(m) = matches.subcommand_matches("replace_staker") {
        let target = m
            .get_one::<String>("target")
            .map(|val| val.as_str())
            .c(d!())
            .and_then(wallet::public_key_from_base64)?;
        // let new_td_addr_pk = if let Some(new_td_address_str) = m.get_one::<String>("td_address") {
        //     let new_td_address = hex::decode(new_td_address_str)
        //         .c(d!("`td_address` is invalid hex. "))?;

        //     if new_td_address.len() != 20 {
        //         return Err(eg!("Invalid tendermint address."));
        //     }

        //     if let Some(new_td_pk) = m.get_one::<String>("td_pubkey") {
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
        common::replace_staker(target, None)?;
    } else if let Some(m) = matches.subcommand_matches("dev") {
        let mut envcfg = EnvCfg::default();

        let ops = if let Some(sm) = m.subcommand_matches("create") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            if let Some(id) = sm.get_one::<String>("evm_chain_id") {
                envcfg.evm_chain_id = id.parse::<u64>().c(d!())?;
            }
            if let Some(itv) = sm.get_one::<String>("block_itv_secs") {
                envcfg.block_itv_secs = itv.parse::<u8>().c(d!())?;
            }
            if let Some(num) = sm.get_one::<String>("validator_num") {
                envcfg.initial_validator_num = num.parse::<u8>().c(d!())?;
                if 64 < envcfg.initial_validator_num {
                    return Err(eg!(
                        "The number of initial validators should not exceed 64!"
                    ));
                }
            }
            if let Some(file) = sm.get_one::<String>("checkpoint_file") {
                envcfg.checkpoint_file = Some(file.to_owned());
            }
            if let Some(ip) = sm.get_one::<String>("host_ip") {
                envcfg.host_ip = Some(ip.to_owned());
            }
            if let Some(abcid_bin) = sm.get_one::<String>("abcid_bin_path") {
                envcfg.abcid_bin = Some(abcid_bin.to_owned());
            }
            if let Some(tm_bin) = sm.get_one::<String>("tendermint_bin_path") {
                envcfg.tendermint_bin = Some(tm_bin.to_owned());
            }
            if let Some(flags) = sm.get_one::<String>("abcid_extra_flags") {
                envcfg.abcid_extra_flags = Some(flags.to_owned());
            }
            if let Some(flags) = sm.get_one::<String>("tendermint_extra_flags") {
                envcfg.tendermint_extra_flags = Some(flags.to_owned());
            }
            if sm.get_flag("force") {
                envcfg.force_create = true;
            }
            Ops::Create
        } else if let Some(sm) = m.subcommand_matches("destroy") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Destroy
        } else if m.subcommand_matches("destroy-all").is_some() {
            Ops::DestroyAll
        } else if let Some(sm) = m.subcommand_matches("start") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Start
        } else if m.subcommand_matches("start-all").is_some() {
            Ops::StartAll
        } else if let Some(sm) = m.subcommand_matches("stop") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Stop
        } else if m.subcommand_matches("stop-all").is_some() {
            Ops::StopAll
        } else if let Some(sm) = m.subcommand_matches("push-node") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::PushNode
        } else if let Some(sm) = m.subcommand_matches("pop-node") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::PopNode
        } else if let Some(sm) = m.subcommand_matches("show") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Show
        } else if m.subcommand_matches("show-all").is_some() {
            Ops::ShowAll
        } else if m.subcommand_matches("list").is_some() {
            Ops::List
        } else if let Some(sm) = m.subcommand_matches("init") {
            if let Some(name) = sm.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::Init
        } else if m.subcommand_matches("init-all").is_some() {
            Ops::InitAll
        } else {
            if let Some(name) = m.get_one::<String>("env_name") {
                envcfg.name = name.to_owned();
            }
            Ops::default()
        };

        envcfg.ops = ops;
        envcfg.exec().c(d!())?;
    } else {
        cmd.print_help().unwrap();
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
