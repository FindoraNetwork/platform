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
    clap::{arg, crate_authors, ArgGroup, Command},
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
    noah::anon_xfr::keys::AXfrKeyPair,
    noah::anon_xfr::structs::OpenAnonAssetRecordBuilder,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    serde::{Deserialize, Serialize},
    std::fs::File,
    std::{borrow::Borrow, fmt, fs},
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
        .about("Generate a random Findora public key/private key Pair")
        .args([arg!(--"fra-address" "if the fra address version of wallet is used")]);
    let mut subcmd_show = Command::new("show")
        .about("View Validator status and accumulated rewardsr")
        .args([
            arg!(-b --basic "show basic account info"),
            arg!(--"fra-address" "if the fra address version of wallet is used"),
        ]);
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
            arg!(--"fra-address" "if the fra address version of wallet is used"),
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
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_unstake = Command::new("unstake")
    .about("Unstake tokens (i.e. unbond tokens) from a Validator")
    .args([
        arg!(-S --"staker-priv-key" <SecretKey> "the file which contains private key (in base64 format) of proposer"),
        arg!(-A --"validator-td-addr" <TendermintAddr> "unstake FRAs to a custom validator"),
        arg!(-n --"amount" <Amount> "how much FRA to unstake, needed for partial undelegation"),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_claim = Command::new("claim")
    .about("Claim accumulated FRA rewards")
    .args([
        arg!(-n --"amount" <Amount> "how much `FRA unit`s to claim"),
        arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let mut subcmd_delegate = Command::new("delegate")
    .about("Delegating operations")
    .args([
        arg!(-n --"amount" <Amount> "how much FRA units to be delegated"),
        arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
        arg!(--"validator" <"VALIDATOR ADDRESS"> "the address of a validator"),
        arg!(--"info" "show delegation info").conflicts_with("amount").conflicts_with("validator"),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let mut subcmd_undelegate = Command::new("undelegate")
    .about("Undelegating operations")
    .args([
        arg!(-n --"amount" <Amount> "how much FRA units to be delegated"),
        arg!(--"seckey" <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet"),
        arg!(--"validator" <"VALIDATOR ADDRESS"> "the address of a validator"),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
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
        arg!(--"fra-address" "if the fra address version of wallet is used"),
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
        arg!(--"fra-address" "if the fra address version of wallet is used"),
     ]);
    let mut subcmd_wallet = Command::new("wallet")
    .about("manipulates a findora wallet")
    .args([
        arg!(--create "create a new findora wallet"),
        arg!(--show "show information of a findora wallet").conflicts_with("create"),
        arg!(--asset "code of asset, such as `fra`").conflicts_with("create"),
        arg!(--seckey <"SECRET KEY"> "the file which contains base64-formated `XfrPrivateKey` of an existing wallet").conflicts_with("create"),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
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
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_account = Command::new("account")
    .about("Return user contract account information")
    .args([
        arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true),
        arg!(-s --"sec-key" <"SECRET KEY"> "base64-formatted `XfrPrivateKey`"),
        arg!(--asset "code of asset, such as `fra`").allow_hyphen_values(true),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_contract_deposit = Command::new("contract-deposit")
    .about("Transfer FRA from a Findora account to the specified Ethereum address")
    .args([
        arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true),
        arg!(-n --amount <AMOUNT> "amount when issuing a asset").required(true),
        arg!(-s --asset <ASSET> "deposit asset type.").required(false),
        arg!(-l --"lowlevel-data" <LOWLEVEL> "deposit with evm lowlevel call.").required(false),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_contract_withdraw = Command::new("contract-withdraw")
    .about("Transfer FRA from an Ethereum address to the specified Findora account")
    .args([
        arg!(-a --addr <"WALLET ADDRESS"> "findora account(eg:fra1rkv...) or Ethereum address(g:0xd3Bf...)").required(true),
        arg!(-n --amount <AMOUNT> "amount when issuing a asset").required(true),
        arg!(-e --"eth-key" <MNEMONIC> "ethereum account mnemonic phrase sign withdraw tx").required(true),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let subcmd_gen_eth_key =
        Command::new("gen-eth-key").about("Generate an Ethereum address");
    let  subcmd_gen_anon_keys = Command::new("gen-anon-keys")
        .about("Generate Anon keys")
        .args([arg!(-f --"file-path" <"File Path"> "Path of file to save the anon keys json")]);
    let mut subcmd_owned_abars = Command::new("owned-abars")
        .about("Get Anon UTXOs for a keypair use commitment")
        .args([
            arg!(-c --commitments <COMMITMENT> "Commitment of the ABAR").required(true).allow_hyphen_values(true),
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender").required(true),
            arg!(--asset <ASSET> "code of asset, such as `fra`").allow_hyphen_values(true),
        ]);
    let mut subcmd_anon_balance = Command::new("anon-balance")
        .about("List Anon balance and spending status for a public key and a list of commitments")
        .args([
            arg!(-c --commitments <COMMITMENT> "Commitment of the ABAR").required(true).allow_hyphen_values(true),
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender").required(true),
            arg!(--asset <"ASSET"> "code of asset, such as `fra`").allow_hyphen_values(true),
        ]);
    let subcmd_owned_open_abars = Command::new("owned-open-abars")
        .about("Get Open Anon UTXOs for a keypair use commitment")
        .args([
            arg!(-c --commitments <COMMITMENT> "Commitment of the ABAR").required(true).allow_hyphen_values(true),
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender").required(true),
        ]);
    let subcmd_owned_utxos = Command::new("owned-utxos")
        .about("List owned UTXOs for a public key")
        .args([
            arg!(--asset <ASSET> "asset code which you want to tansfer")
                .allow_hyphen_values(true),
            arg!(--"fra-address" "if the fra address version of wallet is used"),
        ]);
    let mut subcmd_convert_bar_to_abar = Command::new("convert-bar-to-abar")
        .about("Convert a BAR to Anon BAR for yourself")
        .args([
            arg!(-s --"xfr-secretkey" <"SECRET KEY PATH"> "Xfr secret key file path of converter"),
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of converter").required(true),
            arg!(-t --"txo-sid" <"TXO SID"> "Txo Sid of input to convert").required(true),
            arg!(--"fra-address" "if the fra address version of wallet is used"),
        ]);
    let mut subcmd_convert_abar_to_bar = Command::new("convert-abar-to-bar")
        .about("Convert a ABAR to BAR")
        .args([
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender").required(true),
            arg!(-c --commitments <COMMITMENT> "Commitment for the input Anon BAR").required(true).allow_hyphen_values(true),
            arg!(-t --"to-pubkey" <PubKey> "base64-formated `XfrPublicKey` of the receiver").allow_hyphen_values(true),
            arg!(-T --"to-wallet-address" <"XFR WALLET ADDRESS"> "Xfr public key of the receiver").conflicts_with("to-pubkey"),
            arg!(--"confidential-amount" "mask the amount sent on the transaction log"),
            arg!(--"confidential-type" "mask the asset type sent on the transaction log"),
        ]);
    let mut subcmd_anon_transfer = Command::new("anon-transfer")
        .about("Perform Anonymous Transfer")
        .args([
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender").required(true),
            arg!(-c --commitments <COMMITMENT> "Commitment for the input Anon BAR").required(true).allow_hyphen_values(true),
            arg!(--"fra-commitment" <"FRA COMMITMENT"> "Commitment for the input FRA Anon BAR").allow_hyphen_values(true),
            arg!(--"to-axfr-public-key" <"PUBLIC KEY"> "Axfr public key of the receiver").required(true).allow_hyphen_values(true),
            arg!(-n --amount <Amount> "how much units to transfer").required(true),
        ]);
    let mut subcmd_anon_transfer_batch = Command::new("anon-transfer-batch")
        .about("Perform Anonymous Transfer")
        .args([
            arg!(-s --"anon-keys" <"SECRET KEY"> "Anon keys file path of the sender").required(true),
            arg!(-c --"commitment-file" <COMMITMENT> "Commitments for the input Anon BARs").required(true),
            arg!(--"to-axfr-public-key-file" <"PUBLIC KEY"> "Axfr public keys of the receivers").required(true),
            arg!(-n --"amount-file" <Amount> "how much units to transfer for each receiver").required(true),
            arg!(-a --"asset-file" <Asset> "Relative asset code type.").required(true),
        ]);
    let mut subcmd_anon_fetch_merkle_proof = Command::new("anon-fetch-merkle-proof")
        .about("Perform Anonymous Transfer")
        .args([
            arg!(-a --"txo-sid" <"ATXO SID"> "ATXO SID of the Anonymous Blind Asset Record")
                .required(true),
        ]);
    let subcmd_check_abar_status = Command::new("check-abar-status")
        .about("Check the spending status and balance of ABAR")
        .args([
            arg!(-c --commitments <COMMITMENT> "Commitment of the ABAR")
                .required(true)
                .allow_hyphen_values(true),
            arg!(-a --"anon-keys" <"ANON KEYS PATH"> "Anon keys file path of the sender")
                .required(true),
        ]);

    let subcmd_replace_staker = Command::new("replace_staker")
    .about("Replace the staker of the validator with target address")
    .args([
        arg!(-t --target <"TARGET PUBLIC KEY"> "the public key of new staker, you must be the staker of the validator, you could use `fn setup` to configure your secret key and public key").required(true),
        arg!(--"td_address" <"TENDERMINT ADDRESS"> "the tendermint address that you may want to replace.").required(false),
        arg!(--"td_pubkey" <"TENDERMINT PUBKEY"> "the tendermint public key that you may want to replace.").required(false),
        arg!(--"fra-address" "if the fra address version of wallet is used"),
    ]);
    let mut subcmd_dev = Command::new("dev")
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
        .author(crate_authors!())
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
        .subcommand(subcmd_gen_anon_keys)
        .subcommand(subcmd_owned_abars.clone())
        .subcommand(subcmd_anon_balance.clone())
        .subcommand(subcmd_owned_open_abars)
        .subcommand(subcmd_owned_utxos)
        .subcommand(subcmd_convert_bar_to_abar.clone())
        .subcommand(subcmd_convert_abar_to_bar.clone())
        .subcommand(subcmd_anon_transfer.clone())
        .subcommand(subcmd_anon_transfer_batch.clone())
        .subcommand(subcmd_anon_fetch_merkle_proof.clone())
        .subcommand(subcmd_check_abar_status)
        .subcommand(subcmd_replace_staker)
        .subcommand(subcmd_dev.clone());

    let matches = cmd.clone().get_matches();

    match matches.subcommand() {
        Some(("genkey", m)) => {
            let is_address_fra = m.get_flag("fra-address");
            common::gen_key_and_print(is_address_fra);
        }
        Some(("wallet", m)) => {
            if m.get_flag("create") {
                let is_address_fra = m.get_flag("fra-address");
                common::gen_key_and_print(is_address_fra);
            } else if m.get_flag("show") {
                let seckey = match m.get_one::<String>("seckey") {
                    Some(path) => Some(
                        fs::read_to_string(path).c(d!("Failed to read seckey file"))?,
                    ),
                    None => None,
                };
                let is_address_fra = m.get_flag("fra-address");

                // FRA asset is the default case
                let asset = if let Some(code) = m.get_one::<String>("asset") {
                    match code.to_lowercase().as_str() {
                        "fra" => None,
                        _ => Some(code.as_str()),
                    }
                } else {
                    None
                };
                common::show_account(seckey.as_deref(), asset, is_address_fra)
                    .c(d!())?;
            } else {
                subcmd_wallet.print_help().unwrap();
            }
        }
        Some(("delegate", m)) => {
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let amount = m.get_one::<String>("amount");
            let validator = m.get_one::<String>("validator");
            let show_info = m.get_flag("info");
            let is_address_fra = m.get_flag("fra-address");
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
                subcmd_delegate.print_help().unwrap();
            }
        }
        Some(("undelegate", m)) => {
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let amount = m.get_one::<String>("amount");
            let validator = m.get_one::<String>("validator");
            let is_address_fra = m.get_flag("fra-address");
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
            common::undelegate(seckey.as_deref(), param, is_address_fra).c(d!())?;
        }
        Some(("asset", m)) => {
            if m.get_flag("create") {
                let memo = m.get_one::<String>("memo");
                if memo.is_none() {
                    subcmd_asset.print_help().unwrap();
                    return Ok(());
                }
                let transferable = m.get_flag("transferable");
                let is_address_fra = m.get_flag("fra-address");

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
                    memo.unwrap(),
                    decimal,
                    max_units,
                    transferable,
                    token_code,
                    is_address_fra,
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
                    Some(path) => Some(
                        fs::read_to_string(path).c(d!("Failed to read seckey file"))?,
                    ),
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
                let is_address_fra = m.get_flag("fra-address");

                common::issue_asset(
                    seckey.as_deref(),
                    code.unwrap(),
                    amount,
                    hidden,
                    is_address_fra,
                )
                .c(d!())?;
            } else {
                subcmd_show.print_help().unwrap();
            }
        }
        Some(("staker-update", m)) => {
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
            let is_address_fra = m.get_flag("fra-address");

            let cr = m
                .get_one::<String>("commission-rate")
                .map(|val| val.as_str());
            if vm.is_none() && cr.is_none() {
                subcmd_staker_update.print_help().unwrap();
                println!(
                    "Tips: to update the information of your node, please specify commission-rate or memo"
                );
            } else {
                common::staker_update(cr, vm, is_address_fra).c(d!())?;
            }
        }
        Some(("stake", m)) => {
            let am = m.get_one::<String>("amount");
            if m.get_flag("append") {
                let staker = match m.get_one::<String>("staker-priv-key") {
                    Some(path) => Some(
                        fs::read_to_string(path).c(d!("Failed to read seckey file"))?,
                    ),
                    None => None,
                };
                let td_addr = m
                    .get_one::<String>("validator-td-addr")
                    .map(|val| val.as_str());
                let is_address_fra = m.get_flag("fra-address");
                if am.is_none() {
                    subcmd_stake.print_help().unwrap();
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
                let cr = m
                    .get_one::<String>("commission-rate")
                    .map(|val| val.as_str());
                let vm = m
                    .get_one::<String>("validator-memo")
                    .map(|val| val.as_str());
                let force = m.get_flag("force");
                let is_address_fra = m.get_flag("fra-address");
                if am.is_none() || cr.is_none() {
                    subcmd_stake.print_help().unwrap();
                    println!(
                        "Tips: if you want to raise the power of your node, please use `fn stake --append [OPTIONS]`"
                    );
                } else {
                    common::stake(am.unwrap(), cr.unwrap(), vm, force, is_address_fra)
                        .c(d!())?;
                }
            }
        }
        Some(("unstake", m)) => {
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
            let is_address_fra = m.get_flag("fra-address");
            common::unstake(am, staker.as_deref(), td_addr, is_address_fra).c(d!())?;
        }
        Some(("claim", m)) => {
            let am = m.get_one::<String>("amount").map(|val| val.as_str());
            let seckey = match m.get_one::<String>("seckey") {
                Some(path) => {
                    Some(fs::read_to_string(path).c(d!("Failed to read seckey file"))?)
                }
                None => None,
            };
            let is_address_fra = m.get_flag("fra-address");
            common::claim(am, seckey.as_deref(), is_address_fra).c(d!())?;
        }
        Some(("show", m)) => {
            let basic = m.get_flag("basic");
            let is_address_fra = m.get_flag("fra-address");
            common::show(basic, is_address_fra).c(d!())?;
        }
        Some(("setup", m)) => {
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
        }
        Some(("transfer", m)) => {
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
            let is_address_fra = m.get_flag("fra-address");

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
                    is_address_fra,
                )
                .c(d!())?;
            }
        }
        Some(("transfer-batch", m)) => {
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
                                    .map(|addr| {
                                        wallet::public_key_from_bech32(addr.trim())
                                    })
                                    .collect::<Result<Vec<_>>>()
                                    .c(d!("invalid file"))
                            })
                        })
                })?;
            let am = m.get_one::<String>("amount");
            let is_address_fra = m.get_flag("fra-address");

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
                    is_address_fra,
                )
                .c(d!())?;
            }
        }
        Some(("gen-eth-key", _)) => {
            let (pair, phrase, _) = SecpPair::generate_with_phrase(None);
            let kp = hex::encode(pair.seed());
            println!(
                "\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mPrivateKey:\x1b[00m {}\n\x1b[31;01mAddress:\x1b[00m {}\n",
                phrase,
                kp,
                eth_checksum::checksum(&format!("{:?}", pair.address()))
            );
        }
        Some(("account", m)) => {
            let address = m.get_one::<String>("addr").map(|val| val.as_str());
            let sec_key = m.get_one::<String>("sec-key").map(|val| val.as_str());
            let is_address_fra = m.get_flag("fra-address");

            // FRA asset is the default case
            let asset = if let Some(code) = m.get_one::<String>("asset") {
                match code.to_lowercase().as_str() {
                    "fra" => None,
                    _ => Some(code.as_str()),
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
        }
        Some(("contract-deposit", m)) => {
            let amount = m
                .get_one::<String>("amount")
                .map(|val| val.as_str())
                .c(d!())?;
            let address = m.get_one::<String>("addr").map(|val| val.as_str());

            let asset = m.get_one::<String>("asset").map(|val| val.as_str());
            let lowlevel_data =
                m.get_one::<String>("lowlevel-data").map(|val| val.as_str());
            let is_address_fra = m.get_flag("fra-address");
            transfer_to_account(
                amount.parse::<u64>().c(d!())?,
                asset,
                address,
                lowlevel_data,
                is_address_fra,
            )?
        }
        Some(("contract-withdraw", m)) => {
            let amount = m.get_one::<String>("amount").c(d!())?;
            let address = m.get_one::<String>("addr").map(|val| val.as_str());
            let eth_key = m.get_one::<String>("eth-key").map(|val| val.as_str());
            let is_address_fra = m.get_flag("fra-address");
            transfer_from_account(
                amount.parse::<u64>().c(d!())?,
                address,
                eth_key,
                is_address_fra,
            )?
        }
        Some(("convert-bar-to-abar", m)) => {
            // sender Xfr secret key
            let f = read_file_path(
                m.get_one::<String>("from-seckey").map(|val| val.as_str()),
            )
            .c(d!())?;
            let owner_sk = f.as_ref();

            // receiver AXfr secret key parsed & the receiver Axfr address
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let target_addr = anon_keys.pub_key;

            // The TxoSID to be spent for conversion to ABAR(Anon Blind Asset Record)
            let txo_sid = m.get_one::<String>("txo-sid").map(|val| val.as_str());
            let is_address_fra = m.get_flag("fra-address");

            if txo_sid.is_none() {
                subcmd_convert_bar_to_abar.print_help().unwrap();
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
        }
        Some(("convert-abar-to-bar", m)) => {
            // get anon keys for conversion
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let spend_key = anon_keys.spend_key;
            // get the BAR receiver address
            let to = m
                .get_one::<String>("to-pubkey")
                .map(|val| val.as_str())
                .c(d!())
                .and_then(wallet::public_key_from_base64)
                .or_else(|_| {
                    m.get_one::<String>("to-wallet-address")
                        .c(d!())
                        .and_then(|addr| {
                            wallet::public_key_from_bech32(addr)
                                .c(d!("invalid wallet address"))
                        })
                })?;

            // get the commitments for abar conversion and anon_fee
            let commitment = m.get_one::<String>("commitment");

            if commitment.is_none() {
                subcmd_convert_abar_to_bar.print_help().unwrap();
            } else {
                // Build transaction and submit to network
                common::convert_abar2bar(
                    spend_key,
                    commitment.unwrap(),
                    &to,
                    m.get_flag("confidential-amount"),
                    m.get_flag("confidential-type"),
                )
                .c(d!())?;
            }
        }
        Some(("gen-anon-keys", m)) => {
            let mut prng = ChaChaRng::from_entropy();
            let keypair = AXfrKeyPair::generate(&mut prng);

            let keys = AnonKeys {
                spend_key: wallet::anon_secret_key_to_base64(&keypair),
                pub_key: wallet::anon_public_key_to_base64(&keypair.get_public_key()),
            };

            if let Some(path) = m.get_one::<String>("file-path") {
                serde_json::to_writer_pretty(&File::create(path).c(d!())?, &keys)
                    .c(d!())?;
                println!("Keys saved to file: {}", path);
            }

            // print keys to terminal
            println!("Keys :\n {}", serde_json::to_string_pretty(&keys).unwrap());
        }
        Some(("owned-abars", m)) => {
            // Generates a list of owned Abars (both spent and unspent)
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let spend_key =
                wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str())
                    .c(d!())?;

            let commitments_list =
                m.get_one::<String>("commitments").map(|val| val.as_str());
            if commitments_list.is_none() {
                subcmd_owned_abars.print_help().unwrap();
            }

            common::get_owned_abars(spend_key, commitments_list.unwrap())?;
        }
        Some(("anon-balance", m)) => {
            // Generates a list of owned Abars (both spent and unspent)
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let spend_key =
                wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str())
                    .c(d!())?;
            let asset = m.get_one::<String>("asset").map(|val| val.as_str());
            let commitments_list =
                m.get_one::<String>("commitments").map(|val| val.as_str());
            if commitments_list.is_none() {
                subcmd_anon_balance.print_help().unwrap();
            }
            common::anon_balance(spend_key, commitments_list.unwrap(), asset)?;
        }
        Some(("owned-open-abars", m)) => {
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let commitment_str = m.get_one::<String>("commitment");
            // create derived public key
            let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;
            let spend_key =
                wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str())
                    .c(d!())?;

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
        }
        Some(("owned-utxos", m)) => {
            // All assets are shown in the default case
            let asset = m.get_one::<String>("asset").map(|val| val.as_str());
            let is_address_fra = m.get_flag("fra-address");

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
        }
        Some(("anon-transfer", m)) => {
            // get anon keys of sender
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let secret_key = anon_keys.spend_key;

            // get commitments
            let commitment = m.get_one::<String>("commitment").map(|val| val.as_str());
            let fee_commitment = m
                .get_one::<String>("fra-commitment")
                .map(|val| val.as_str());

            // get receiver keys and amount
            let to_axfr_public_key = m
                .get_one::<String>("to-axfr-public-key")
                .map(|val| val.as_str());
            let amount = m.get_one::<String>("amount").map(|val| val.as_str());

            if commitment.is_none() || to_axfr_public_key.is_none() || amount.is_none() {
                subcmd_anon_transfer.print_help().unwrap();
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
        }
        Some(("anon-transfer-batch", m)) => {
            // get anon keys of sender
            let anon_keys = parse_anon_key_from_path(
                m.get_one::<String>("anon-keys").map(|val| val.as_str()),
            )?;
            let secret_key =
                wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str())
                    .c(d!())?;

            let to_axfr_public_keys = m
                .get_one::<String>("to-axfr-public-key-file")
                .c(d!())
                .and_then(|f| {
                    fs::read_to_string(f).c(d!()).and_then(|pks| {
                        pks.lines()
                            .map(|pk| wallet::anon_public_key_from_base64(pk.trim()))
                            .collect::<Result<Vec<_>>>()
                            .c(d!("invalid file"))
                    })
                })?;
            let commitments =
                m.get_one::<String>("commitment-file")
                    .c(d!())
                    .and_then(|f| {
                        fs::read_to_string(f).c(d!()).map(|rms| {
                            rms.lines().map(String::from).collect::<Vec<String>>()
                        })
                    })?;
            let amounts = m.get_one::<String>("amount-file").c(d!()).and_then(|f| {
                fs::read_to_string(f)
                    .c(d!())
                    .map(|ams| ams.lines().map(String::from).collect::<Vec<String>>())
            })?;
            let assets = m.get_one::<String>("asset-file").c(d!()).and_then(|f| {
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
                fs::read_to_string(f).c(d!()).map(|ams| {
                    ams.lines().map(token_code).collect::<Vec<AssetTypeCode>>()
                })
            })?;

            if to_axfr_public_keys.is_empty()
                || commitments.is_empty()
                || amounts.is_empty()
                || assets.is_empty()
            {
                subcmd_anon_transfer_batch.print_help().unwrap();
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
        }
        Some(("anon-fetch-merkle-proof", m)) => {
            let atxo_sid = m.get_one::<String>("atxo-sid").map(|val| val.as_str());

            if atxo_sid.is_none() {
                subcmd_anon_fetch_merkle_proof.print_help().unwrap();
            } else {
                let mt_leaf_info = common::get_mtleaf_info(atxo_sid.unwrap()).c(d!())?;
                println!("{:?}", serde_json::to_string_pretty(&mt_leaf_info));
            }
        }
        Some(("check-abar-status", m)) => {
            let anon_keys = match m.get_one::<String>("anon-keys") {
                Some(path) => {
                    let f = fs::read_to_string(path)
                        .c(d!("Failed to read anon-keys file"))?;
                    serde_json::from_str::<AnonKeys>(f.as_str()).c(d!())?
                }
                None => return Err(eg!("path for anon-keys file not found")),
            };
            let commitment_str =
                m.get_one::<String>("commitment").map(|val| val.as_str());
            let commitment = wallet::commitment_from_base58(commitment_str.unwrap())?;

            let spend_key =
                wallet::anon_secret_key_from_base64(anon_keys.spend_key.as_str())
                    .c(d!())?;

            let abar = utils::get_owned_abar(&commitment).c(d!())?;
            common::check_abar_status(spend_key, abar).c(d!())?;
        }
        Some(("replace_staker", m)) => {
            let target = m
                .get_one::<String>("target")
                .map(|val| val.as_str())
                .c(d!())
                .and_then(wallet::public_key_from_base64)?;

            let is_address_fra = m.get_flag("fra-address");
            common::replace_staker(target, None, is_address_fra)?;
        }
        Some(("dev", m)) => {
            let mut envcfg = EnvCfg::default();
            envcfg.ops = match m.subcommand() {
                Some(("create", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    if let Some(id) = m.get_one::<String>("evm_chain_id") {
                        envcfg.evm_chain_id = id.parse::<u64>().c(d!())?;
                    }
                    if let Some(itv) = m.get_one::<String>("block_itv_secs") {
                        envcfg.block_itv_secs = itv.parse::<u8>().c(d!())?;
                    }
                    if let Some(num) = m.get_one::<String>("validator_num") {
                        envcfg.initial_validator_num = num.parse::<u8>().c(d!())?;
                        if 64 < envcfg.initial_validator_num {
                            return Err(eg!(
                                "The number of initial validators should not exceed 64!"
                            ));
                        }
                    }
                    if let Some(file) = m.get_one::<String>("checkpoint_file") {
                        envcfg.checkpoint_file = Some(file.to_owned());
                    }
                    if let Some(ip) = m.get_one::<String>("host_ip") {
                        envcfg.host_ip = Some(ip.to_owned());
                    }
                    if let Some(abcid_bin) = m.get_one::<String>("abcid_bin_path") {
                        envcfg.abcid_bin = Some(abcid_bin.to_owned());
                    }
                    if let Some(tm_bin) = m.get_one::<String>("tendermint_bin_path") {
                        envcfg.tendermint_bin = Some(tm_bin.to_owned());
                    }
                    if let Some(flags) = m.get_one::<String>("abcid_extra_flags") {
                        envcfg.abcid_extra_flags = Some(flags.to_owned());
                    }
                    if let Some(flags) = m.get_one::<String>("tendermint_extra_flags") {
                        envcfg.tendermint_extra_flags = Some(flags.to_owned());
                    }
                    if m.get_flag("force") {
                        envcfg.force_create = true;
                    }
                    Ops::Create
                }
                Some(("destroy", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::Destroy
                }
                Some(("destroy-all", _)) => Ops::DestroyAll,
                Some(("start", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::Start
                }
                Some(("start-all", _)) => Ops::StartAll,
                Some(("stop", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::Stop
                }
                Some(("stop-all", _)) => Ops::StopAll,
                Some(("push-node", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::PushNode
                }
                Some(("pop-node", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::PopNode
                }
                Some(("show", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::Show
                }
                Some(("show-all", _)) => Ops::ShowAll,
                Some(("list", _)) => Ops::List,
                Some(("init", m)) => {
                    if let Some(name) = m.get_one::<String>("env_name") {
                        envcfg.name = name.to_owned();
                    }
                    Ops::Init
                }
                Some(("init-all", _)) => Ops::InitAll,
                Some((_, _)) => {
                    subcmd_dev.print_help().unwrap();
                    return Err(eg!("no this command"));
                }
                None => {
                    subcmd_dev.print_help().unwrap();
                    return Err(eg!("dev sub command not found"));
                }
            };
            envcfg.exec().c(d!())?;
        }
        Some((_, _)) => {
            cmd.print_help().unwrap();
        }
        None => {
            cmd.print_help().unwrap();
        }
    };
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
