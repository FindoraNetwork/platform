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
    clap::{crate_authors, App, SubCommand},
    finutils::common,
    globutils::wallet,
    lazy_static::lazy_static,
    ledger::{
        data_model::{gen_random_keypair, Transaction, BLACK_HOLE_PUBKEY_STAKING},
        staking::{
            check_delegation_amount, td_addr_to_bytes, BLOCK_INTERVAL, FRA,
            FRA_PRE_ISSUE_AMOUNT,
        },
        store::utils::fra_gen_initial_tx,
    },
    ruc::*,
    serde::Serialize,
    std::{collections::BTreeMap, env},
    zei::xfr::sig::{XfrKeyPair, XfrPublicKey},
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
    let subcmd_init = SubCommand::with_name("init")
        .arg_from_usage("--mainnet")
        .arg_from_usage("-i, --interval=[Interval] 'block interval'")
        .arg_from_usage("-t, --integration-test 'perform integration test'")
        .arg_from_usage("-s, --skip-validator 'skip validator initialization'");
    let subcmd_test = SubCommand::with_name("test");
    let subcmd_issue = SubCommand::with_name("issue").about("issue FRA on demand");
    let subcmd_delegate = SubCommand::with_name("delegate")
        .arg_from_usage("-u, --user=[User] 'user name of delegator'")
        .arg_from_usage("-n, --amount=[Amount] 'how much FRA units to delegate'")
        .arg_from_usage("-v, --validator=[Validator] 'which validator to delegate to'");
    let subcmd_undelegate = SubCommand::with_name("undelegate")
        .arg_from_usage("-u, --user=[User] 'user name of the delegator'")
        .arg_from_usage("-n, --amount=[Amount] 'how much FRA to undelegate, needed for partial undelegation'")
        .arg_from_usage("-v, --validator=[Validator] 'which validator to undelegate from, needed for partial undelegation'");
    let subcmd_claim = SubCommand::with_name("claim")
        .arg_from_usage("-u, --user=[User] 'user name of delegator'")
        .arg_from_usage("-n, --amount=[Amount] 'how much FRA to claim'");
    let subcmd_transfer = SubCommand::with_name("transfer")
        .arg_from_usage("-f, --from-user=[User] 'transfer sender'")
        .arg_from_usage("-t, --to-user=[User] 'transfer receiver'")
        .arg_from_usage("-n, --amount=[Amount] 'how much FRA to transfer'");
    let subcmd_show = SubCommand::with_name("show")
        .arg_from_usage("-r, --root-mnemonic 'show the pre-defined root mnemonic'")
        .arg_from_usage("-U, --user-list 'show the pre-defined user list'")
        .arg_from_usage("-v, --validator-list 'show the pre-defined validator list'")
        .arg_from_usage("-u, --user=[User] 'user name of delegator'");

    let matches = App::new("stt")
        .version(common::version())
        .author(crate_authors!())
        .about("A manual test tool for the staking function.")
        .arg_from_usage("-v, --version")
        .subcommand(subcmd_init)
        .subcommand(subcmd_test)
        .subcommand(subcmd_issue)
        .subcommand(subcmd_delegate)
        .subcommand(subcmd_undelegate)
        .subcommand(subcmd_claim)
        .subcommand(subcmd_transfer)
        .subcommand(subcmd_show)
        .get_matches();

    if matches.is_present("version") {
        println!("{}", env!("VERGEN_SHA"));
    } else if let Some(m) = matches.subcommand_matches("init") {
        let interval = m
            .value_of("interval")
            .unwrap_or("0")
            .parse::<u64>()
            .c(d!())?;
        let is_mainnet = m.is_present("mainnet");
        let skip_validator = m.is_present("skip-validator");
        let integration_test = m.is_present("integration-test");
        init::init(interval, is_mainnet, skip_validator, integration_test).c(d!())?;
    } else if matches.is_present("test") {
        init::i_testing::run_all().c(d!())?;
    } else if matches.is_present("issue") {
        issue::issue().c(d!())?;
    } else if let Some(m) = matches.subcommand_matches("delegate") {
        let user = m.value_of("user");
        let amount = m.value_of("amount");
        let validator = m.value_of("validator");

        if user.is_none() || amount.is_none() || validator.is_none() {
            println!("{}", m.usage());
        } else {
            let amount = amount.unwrap().parse::<u64>().c(d!())?;
            delegate::gen_tx(user.unwrap(), amount, validator.unwrap())
                .c(d!())
                .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
        }
    } else if let Some(m) = matches.subcommand_matches("undelegate") {
        let user = m.value_of("user");
        let amount = m.value_of("amount");
        let validator = m.value_of("validator");

        if user.is_none()
            || user.unwrap().trim().is_empty()
            || matches!((amount, validator), (Some(_), None) | (None, Some(_)))
        {
            println!("{}", m.usage());
        } else {
            let amount = amount.and_then(|am| am.parse::<u64>().ok());
            undelegate::gen_tx(user.unwrap(), amount, validator)
                .c(d!())
                .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
        }
    } else if let Some(m) = matches.subcommand_matches("claim") {
        let user = m.value_of("user");

        if user.is_none() {
            println!("{}", m.usage());
        } else {
            let amount = if let Some(am) = m.value_of("amount") {
                Some(am.parse::<u64>().c(d!())?)
            } else {
                None
            };
            claim::gen_tx(user.unwrap(), amount)
                .c(d!())
                .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
        }
    } else if let Some(m) = matches.subcommand_matches("transfer") {
        let from = m.value_of("from-user");
        let to = m.value_of("to-user");
        let amount = m.value_of("amount");

        match (from, to, amount) {
            (Some(sender), Some(receiver), Some(am)) => {
                let am = am.parse::<u64>().c(d!())?;
                let owner_kp = search_kp(sender).c(d!())?;
                let target_pk = search_kp(receiver)
                    .c(d!())
                    .map(|kp| kp.get_pk())
                    .or_else(|e| wallet::public_key_from_base64(receiver).c(d!(e)))?;
                common::utils::transfer(owner_kp, &target_pk, am, None, false, false)
                    .c(d!())?;
            }
            _ => {
                println!("{}", m.usage());
            }
        }
    } else if let Some(m) = matches.subcommand_matches("show") {
        let rm = m.is_present("root-mnemonic");
        let ul = m.is_present("user-list");
        let vl = m.is_present("validator-list");
        let u = m.value_of("user");

        if rm || ul || vl || u.is_some() {
            print_info(rm, ul, vl, u).c(d!())?;
        } else {
            println!("{}", m.usage());
        }
    } else {
        println!("{}", matches.usage());
    }

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
        rand_chacha::rand_core::SeedableRng,
        rand_chacha::ChaChaRng,
        zei::setup::PublicParams,
        zei::xfr::{
            asset_record::{build_blind_asset_record, AssetRecordType},
            structs::AssetRecordTemplate,
        },
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
        let params = PublicParams::default();
        let outputs = (0..2)
            .map(|_| {
                let (ba, _, _) = build_blind_asset_record(
                    &mut ChaChaRng::from_entropy(),
                    &params.pc_gens,
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
        Ok(builder.take_transaction())
    }
}

mod delegate {
    use {super::*, zei::xfr::asset_record::AssetRecordType};

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

        let mut tx = builder.take_transaction();
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

        Ok(builder.take_transaction())
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

        Ok(builder.take_transaction())
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
