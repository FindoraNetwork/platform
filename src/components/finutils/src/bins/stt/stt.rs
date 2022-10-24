//!
//! # staking_tester
//!
//! - init --server-addr=<127.0.0.1> --server-port=<8669>
//! - delegate --user=<cat1> --amount=<N> --validator=<dog1>
//! - undelegate --user=<cat1>
//! - claim --user=<cat1> --amount=<N>
//!

#![deny(warnings)]

mod init;

use {
    clap::{arg, crate_authors, Arg, Command},
    finutils::common,
    globutils::wallet,
    lazy_static::lazy_static,
    ledger::{
        data_model::{gen_random_keypair, Transaction, BLACK_HOLE_PUBKEY_STAKING},
        staking::{
            check_delegation_amount, td_addr_to_bytes, BLOCK_INTERVAL, FRA,
            FRA_PRE_ISSUE_AMOUNT,
        },
        utils::fra_gen_initial_tx,
    },
    noah::xfr::sig::{XfrKeyPair, XfrPublicKey},
    ruc::*,
    serde::Serialize,
    std::{collections::BTreeMap, env},
};

lazy_static! {
    static ref USER_LIST: BTreeMap<Name, User> = gen_user_list();
    static ref VALIDATOR_LIST: BTreeMap<Name, Validator> = gen_valiator_list();
    static ref ROOT_KP: XfrKeyPair =
        pnk!(wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC));
}

const ROOT_MNEMONIC: &str = "zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant";

type Name = String;
type NameRef<'a> = &'a str;

#[macro_export(crate)]
macro_rules! sleep_n_block {
    ($n_block: expr, $itv: expr) => {{
        let n = $n_block as f64;
        let itv = $itv as f64;
        sleep_ms!((n * itv * 1000.0) as u64);
    }};
    ($n_block: expr) => {
        sleep_n_block!($n_block, *ledger::staking::BLOCK_INTERVAL)
    };
}

fn main() {
    pnk!(run());
}

fn run() -> Result<()> {
    let subcmd_init = Command::new("init").args([
        arg!(--mainnet),
        arg!(-i --interval <Interval> "block interval."),
        arg!(-s --"skip-validator" "skip validator initialization."),
    ])
        .arg(
            Arg::new("staking-info-file")
                .long("staking-info-file")
                .required(false)
                .help("Json file that contains a list of tendermint address, public key and id(Xfr public key)."),
        )
        .arg(
            Arg::new("only-init")
            .long("only-init")
            .required(false)
            .help("If this flag is set, only initiate the validators and do nothing.")
        );

    let subcmd_test = Command::new("test");
    let subcmd_issue = Command::new("issue").about("issue FRA on demand");

    let mut subcmd_delegate = Command::new("delegate").args([
        arg!(-u --user <InterUserval> "user name of delegator"),
        arg!(-n --amount <Amount> "how much FRA units to delegate."),
        arg!(-v --validator <Validator> "which validator to delegate to."),
    ]);
    let mut subcmd_undelegate = Command::new("undelegate").args([
        arg!(-u --user <InterUserval> "user name of delegator"),
        arg!(-n --amount <Amount> "how much FRA to undelegate, needed for partial undelegation."),
        arg!(-v --validator <Validator> "which validator to undelegate from, needed for partial undelegation."),
    ]);
    let mut subcmd_claim = Command::new("claim").args([
        arg!(-u --user <InterUserval> "user name of delegator"),
        arg!(-n --amount <Amount> "how much FRA to claim."),
    ]);
    let mut subcmd_transfer = Command::new("transfer").args([
        arg!(-f --"from-user" <User> "transfer sender"),
        arg!(-t --"to-user" <User> "transfer receiver."),
        arg!(-n --amount <Amount> "how much FRA to transfer."),
    ]);
    let mut subcmd_show = Command::new("show").args([
        arg!(-r --"root-mnemonic" "show the pre-defined root mnemonic"),
        arg!(-U --"user-list" "show the pre-defined user list."),
        arg!(-v --"validator-list" "show the pre-defined validator list."),
        arg!(-u --user <InterUserval> "user name of delegator"),
    ]);
    let mut cmd = Command::new("stt")
        .version(env!("VERGEN_SHA"))
        .author(crate_authors!())
        .about("A manual test tool for the staking function.")
        .subcommand(subcmd_init)
        .subcommand(subcmd_test)
        .subcommand(subcmd_issue)
        .subcommand(subcmd_delegate.clone())
        .subcommand(subcmd_undelegate.clone())
        .subcommand(subcmd_claim.clone())
        .subcommand(subcmd_transfer.clone())
        .subcommand(subcmd_show.clone());
    let matches = cmd.clone().get_matches();

    match matches.subcommand() {
        Some(("init", m)) => {
            let interval = m
                .get_one::<String>("interval")
                .unwrap_or(&"0".to_string())
                .parse::<u64>()
                .c(d!())?;
            let is_mainnet = m.get_flag("mainnet");
            let skip_validator = m.get_flag("skip-validator");
            let staking_info_file = m
                .get_one::<String>("staking-info-file")
                .map(|val| val.as_str());
            let only_init = m.get_flag("only-init");

            init::init(
                interval,
                is_mainnet,
                skip_validator,
                staking_info_file,
                only_init,
            )
            .c(d!())?;
        }
        Some(("test", _)) => {
            init::i_testing::run_all().c(d!())?;
        }
        Some(("issue", _)) => {
            issue::issue().c(d!())?;
        }
        Some(("delegate", m)) => {
            let user = m.get_one::<String>("user");
            let amount = m.get_one::<String>("amount");
            let validator = m.get_one::<String>("validator");

            if user.is_none() || amount.is_none() || validator.is_none() {
                subcmd_delegate.print_help().unwrap();
            } else {
                let amount = amount.unwrap().parse::<u64>().c(d!())?;
                delegate::gen_tx(user.unwrap(), amount, validator.unwrap())
                    .c(d!())
                    .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
            }
        }
        Some(("undelegate", m)) => {
            let user = m.get_one::<String>("user");
            let amount = m.get_one::<String>("amount");
            let validator = m.get_one::<String>("validator").map(|val| val.as_str());

            if user.is_none()
                || user.unwrap().trim().is_empty()
                || matches!((amount, validator), (Some(_), None) | (None, Some(_)))
            {
                subcmd_undelegate.print_help().unwrap();
            } else {
                let amount = amount.and_then(|am| am.parse::<u64>().ok());
                undelegate::gen_tx(user.unwrap(), amount, validator)
                    .c(d!())
                    .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
            }
        }
        Some(("claim", m)) => {
            let user = m.get_one::<String>("user");
            if user.is_none() {
                subcmd_claim.print_help().unwrap();
            } else {
                let amount = if let Some(am) = m.get_one::<String>("amount") {
                    Some(am.parse::<u64>().c(d!())?)
                } else {
                    None
                };
                claim::gen_tx(user.unwrap(), amount)
                    .c(d!())
                    .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
            }
        }
        Some(("transfer", m)) => {
            let from = m.get_one::<String>("from-user");
            let to = m.get_one::<String>("to-user");
            let amount = m.get_one::<String>("amount");

            match (from, to, amount) {
                (Some(sender), Some(receiver), Some(am)) => {
                    let am = am.parse::<u64>().c(d!())?;
                    let owner_kp = search_kp(sender).c(d!())?;
                    let target_pk =
                        search_kp(receiver).c(d!()).map(|kp| kp.get_pk()).or_else(
                            |e| wallet::public_key_from_base64(receiver).c(d!(e)),
                        )?;
                    common::utils::transfer(
                        owner_kp, &target_pk, am, None, false, false,
                    )
                    .c(d!())?;
                }
                _ => {
                    subcmd_transfer.print_help().unwrap();
                }
            }
        }
        Some(("show", m)) => {
            let rm = m.get_flag("root-mnemonic");
            let ul = m.get_flag("user-list");
            let vl = m.get_flag("validator-list");
            let u = m.get_one::<String>("user").map(|val| val.as_str());

            if rm || ul || vl || u.is_some() {
                print_info(rm, ul, vl, u).c(d!())?;
            } else {
                subcmd_show.print_help().unwrap();
            }
        }
        Some((_, _)) => {
            cmd.print_help().unwrap();
            return Err(eg!("no this command"));
        }
        None => {
            cmd.print_help().unwrap();
            return Err(eg!("sub command not found"));
        }
    };
    Ok(())
}

mod issue {
    use {
        super::*,
        ledger::{
            data_model::{
                AssetTypeCode, IssueAsset, IssueAssetBody, IssuerKeyPair, Operation,
                TxOutput, ASSET_TYPE_FRA,
            },
            staking::FRA_PRE_ISSUE_AMOUNT,
        },
        noah::xfr::{
            asset_record::{build_blind_asset_record, AssetRecordType},
            structs::AssetRecordTemplate,
        },
        noah_crypto::basic::pedersen_comm::PedersenCommitmentRistretto,
        rand_chacha::rand_core::SeedableRng,
        rand_chacha::ChaChaRng,
    };

    pub fn issue() -> Result<()> {
        gen_issue_tx()
            .c(d!())
            .and_then(|tx| common::utils::send_tx(&tx).c(d!()))
    }

    fn gen_issue_tx() -> Result<Transaction> {
        let root_kp =
            wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC).c(d!())?;

        let mut builder = common::utils::new_tx_builder().c(d!())?;

        let template = AssetRecordTemplate::with_no_asset_tracing(
            FRA_PRE_ISSUE_AMOUNT / 2,
            ASSET_TYPE_FRA,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            root_kp.get_pk(),
        );
        let pc_gens = PedersenCommitmentRistretto::default();
        let outputs = (0..2)
            .map(|_| {
                let (ba, _, _) = build_blind_asset_record(
                    &mut ChaChaRng::from_entropy(),
                    &pc_gens,
                    &template,
                    vec![],
                );
                (
                    TxOutput {
                        id: None,
                        record: ba,
                        lien: None,
                    },
                    None,
                )
            })
            .collect::<Vec<_>>();
        let aib = IssueAssetBody::new(
            &AssetTypeCode {
                val: ASSET_TYPE_FRA,
            },
            builder.get_seq_id(),
            &outputs,
        )
        .c(d!())?;
        let asset_issuance_operation =
            IssueAsset::new(aib, &IssuerKeyPair { keypair: &root_kp }).c(d!())?;

        builder.add_operation(Operation::IssueAsset(asset_issuance_operation));
        builder.build_and_take_transaction()
    }
}

mod delegate {
    use {super::*, noah::xfr::asset_record::AssetRecordType};

    pub fn gen_tx(
        user: NameRef,
        amount: u64,
        validator: NameRef,
    ) -> Result<Transaction> {
        check_delegation_amount(amount, true).c(d!())?;

        let owner_kp = USER_LIST
            .get(user)
            .map(|u| &u.keypair)
            .or_else(|| VALIDATOR_LIST.get(user).map(|v| &v.keypair))
            .c(d!())?;
        let validator = &VALIDATOR_LIST.get(validator).c(d!())?.td_addr;

        let mut builder = common::utils::new_tx_builder().c(d!())?;

        common::utils::gen_transfer_op(
            owner_kp,
            vec![(&BLACK_HOLE_PUBKEY_STAKING, amount)],
            None,
            false,
            false,
            Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
        )
        .c(d!())
        .map(|principal_op| {
            builder.add_operation(principal_op);
            builder.add_operation_delegation(owner_kp, amount, validator.to_owned());
        })?;

        let mut tx = builder.build_and_take_transaction()?;
        tx.sign(owner_kp);
        Ok(tx)
    }
}

mod undelegate {
    use {super::*, ledger::staking::PartialUnDelegation};

    pub fn gen_tx(
        user: NameRef,
        amount: Option<u64>,
        validator: Option<NameRef>,
    ) -> Result<Transaction> {
        let owner_kp = &USER_LIST.get(user.trim()).c(d!())?.keypair;
        let validator = validator
            .and_then(|v| VALIDATOR_LIST.get(v))
            .map(|x| pnk!(td_addr_to_bytes(&x.td_addr)));

        let mut builder = common::utils::new_tx_builder().c(d!())?;

        common::utils::gen_fee_op(owner_kp).c(d!()).map(|op| {
            builder.add_operation(op);
            if let Some(amount) = amount {
                // partial undelegation
                builder.add_operation_undelegation(
                    owner_kp,
                    Some(PartialUnDelegation::new(
                        amount,
                        gen_random_keypair().get_pk(),
                        validator.unwrap(),
                    )),
                );
            } else {
                builder.add_operation_undelegation(owner_kp, None);
            }
        })?;

        builder.build_and_take_transaction()
    }
}

mod claim {
    use super::*;

    pub fn gen_tx(user: NameRef, amount: Option<u64>) -> Result<Transaction> {
        let owner_kp = search_kp(user).c(d!())?;

        let mut builder = common::utils::new_tx_builder().c(d!())?;

        common::utils::gen_fee_op(owner_kp).c(d!()).map(|op| {
            builder.add_operation(op);
            builder.add_operation_claim(owner_kp, amount);
        })?;

        builder.build_and_take_transaction()
    }
}

fn print_info(
    show_root_mnemonic: bool,
    show_user_list: bool,
    show_validator_list: bool,
    user: Option<NameRef>,
) -> Result<()> {
    if show_root_mnemonic {
        let kp = wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC).c(d!())?;
        println!(
            "\x1b[31;01mROOT MNEMONIC:\x1b[00m\n{}\nKeys: {}",
            ROOT_MNEMONIC,
            serde_json::to_string(&kp).c(d!())?
        );
    }

    if show_user_list {
        let user_list = serde_json::to_string_pretty(&*USER_LIST).c(d!())?;
        println!("\x1b[31;01mUSER LIST:\x1b[00m\n{}\n", user_list);
    }

    if show_validator_list {
        let validator_list = serde_json::to_string_pretty(&*VALIDATOR_LIST).c(d!())?;
        println!("\x1b[31;01mVALIDATOR LIST:\x1b[00m\n{}\n", validator_list);
    }

    if let Some(u) = user {
        let balance = get_balance(u).c(d!())?;
        println!("\x1b[31;01mUSER BALANCE:\x1b[00m\n{} FRA units\n", balance);

        let user_info = get_delegation_info(u).c(d!())?;
        println!("\x1b[31;01mUSER DELEGATION:\x1b[00m\n{}\n", user_info);
    }

    Ok(())
}

fn get_delegation_info(user: NameRef) -> Result<String> {
    let pk = USER_LIST
        .get(user)
        .map(|u| &u.pubkey)
        .or_else(|| VALIDATOR_LIST.get(user).map(|v| &v.pubkey))
        .c(d!())?;

    common::utils::get_delegation_info(pk)
        .c(d!())
        .and_then(|di| serde_json::to_string_pretty(&di).c(d!()))
}

fn get_balance(user: NameRef) -> Result<u64> {
    let kp = search_kp(user).c(d!())?;
    common::utils::get_balance(kp).c(d!())
}

#[derive(Debug, Serialize)]
struct User {
    name: String,
    mnemonic: String,
    pubkey: XfrPublicKey,
    keypair: XfrKeyPair,
}

fn gen_user_list() -> BTreeMap<Name, User> {
    const MNEMONIC_LIST: [&str; 5] = [
        "bunker boring twenty addict element cover owner economy catalog cause staff shock say wave rent submit clean cinnamon visit erase rescue transfer wave forget",
        "swap mail library enrich flee strike property flock unhappy betray bitter awake health glimpse armed good tip bicycle skill belt beyond smooth flush ring",
        "job latin tilt burden address grid opinion lazy mystery crystal pink pen lady public fall magnet method pact pill frost champion symptom zero problem",
        "ostrich pill knee divorce situate firm size dilemma cushion broccoli evolve carbon start virtual cave ask hat until physical nothing flash bunker inject thrive",
        "priority venue mail camp lens myself media base head fringe endorse amazing flower winter danger mammal walnut fabric please letter access suspect shed country",
    ];

    (0..MNEMONIC_LIST.len())
        .map(|i| {
            let keypair = pnk!(wallet::restore_keypair_from_mnemonic_default(
                MNEMONIC_LIST[i]
            ));
            let pubkey = keypair.get_pk();
            User {
                name: format!("u{}", 1 + i),
                mnemonic: MNEMONIC_LIST[i].to_owned(),
                pubkey,
                keypair,
            }
        })
        .map(|u| (u.name.clone(), u))
        .collect()
}

#[derive(Debug, Serialize)]
struct Validator {
    name: String,
    td_addr: String,
    pubkey: XfrPublicKey,
    keypair: XfrKeyPair,
}

fn gen_valiator_list() -> BTreeMap<Name, Validator> {
    const NUM: usize = 20;

    // locale env
    #[cfg(feature = "debug_env")]
    const TD_ADDR_LIST: [&str; NUM] = include!("td_addr_list.const.debug_env");

    // online env
    #[cfg(not(feature = "debug_env"))]
    const TD_ADDR_LIST: [&str; NUM] = include!("td_addr_list.const");

    const MNEMONIC_LIST: [&str; NUM] = include!("mnemonic_list.const");

    (0..NUM)
        .map(|i| {
            let td_addr = TD_ADDR_LIST[i].to_owned();
            let keypair = pnk!(wallet::restore_keypair_from_mnemonic_default(
                MNEMONIC_LIST[i]
            ));
            let pubkey = keypair.get_pk();
            Validator {
                name: format!("v{}", 1 + i),
                td_addr,
                pubkey,
                keypair,
            }
        })
        .map(|v| (v.name.clone(), v))
        .collect()
}

fn search_kp(user: NameRef) -> Option<&'static XfrKeyPair> {
    if "root" == user {
        return Some(&ROOT_KP);
    }

    USER_LIST
        .get(user)
        .map(|u| &u.keypair)
        .or_else(|| VALIDATOR_LIST.get(user).map(|v| &v.keypair))
}
