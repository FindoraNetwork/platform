//!
//! # staking_tester
//!
//! - init --server-addr=<127.0.0.1> --server-port=<8669>
//! - delegate --user=<cat1> --amount=<N> --validator=<dog1>
//! - undelegate --user=<cat1>
//! - claim --user=<cat1> --amount=<N>
//!

#![deny(warnings)]

use clap::{crate_authors, App, SubCommand};
use finutils::common;
use globutils::wallet;
use lazy_static::lazy_static;
use ledger::{
    data_model::{Transaction, BLACK_HOLE_PUBKEY_STAKING},
    staking::{
        check_delegation_amount, gen_random_keypair, td_addr_to_bytes, BLOCK_INTERVAL,
        FRA, FRA_PRE_ISSUE_AMOUNT,
    },
    store::utils::fra_gen_initial_tx,
};
use ruc::*;
use serde::Serialize;
use std::{collections::BTreeMap, env};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};

lazy_static! {
    static ref USER_LIST: BTreeMap<Name, User> = gen_user_list();
    static ref VALIDATOR_LIST: BTreeMap<Name, Validator> = gen_valiator_list();
    static ref ROOT_KP: XfrKeyPair =
        pnk!(wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC));
}

const ROOT_MNEMONIC: &str = "zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant";

type Name = String;
type NameRef<'a> = &'a str;

macro_rules! sleep_n_block {
    ($n_block: expr, $intvl: expr) => {
        sleep_ms!($n_block * $intvl * 1000);
    };
}

fn main() {
    pnk!(run());
}

fn run() -> Result<()> {
    let subcmd_init = SubCommand::with_name("init")
        .arg_from_usage("--mainnet")
        .arg_from_usage("-i, --interval=[Interval] 'block interval'")
        .arg_from_usage("-s, --skip-validator 'skip validator initialization'");
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
        let skip_validator = m.is_present("skip-validator");
        let is_mainnet = m.is_present("mainnet");
        init::init(interval, skip_validator, is_mainnet).c(d!())?;
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

mod init {
    use super::*;

    pub fn init(
        mut interval: u64,
        skip_validator: bool,
        is_mainnet: bool,
    ) -> Result<()> {
        // Customized interval(for devnet) or default(for mainnet)
        if interval == 0 {
            interval = BLOCK_INTERVAL;
        }
        println!(">>> block interval: {} seconds", interval);

        // The genesis key pair
        let root_kp =
            wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC).c(d!())?;

        // Reset genesis validators
        if !skip_validator {
            println!(">>> set initial validator set...");
            common::set_initial_validators().c(d!())?;

            alt!(is_mainnet, return Ok(()));

            println!(">>> wait 5 blocks...");
            sleep_n_block!(5, interval);
        }

        // Define and issue FRA
        println!(">>> define and issue FRA...");
        common::utils::send_tx(&fra_gen_initial_tx(&root_kp)).c(d!())?;

        println!(">>> wait 2 block...");
        sleep_n_block!(2, interval);

        if skip_validator {
            println!(">>> DONE !");
            return Ok(());
        }

        // Genesis validators trigger self-staking
        let mut target_list = USER_LIST
            .values()
            .map(|u| &u.pubkey)
            .chain(VALIDATOR_LIST.values().map(|v| &v.pubkey))
            .map(|pk| (pk, FRA_PRE_ISSUE_AMOUNT / 2_0000))
            .collect::<Vec<_>>();

        // Wallet Address: fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5
        // Mnemonic: field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan
        // Key: {
        //   "pub_key": "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=",
        //   "sec_key": "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg="
        // }
        let bank = pnk!(wallet::public_key_from_base64(
            "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ="
        ));
        target_list.push((&bank, FRA_PRE_ISSUE_AMOUNT / 100 * 99));

        println!(">>> transfer FRAs to validators...");
        common::utils::transfer_batch(&root_kp, target_list, None, false, false)
            .c(d!())?;

        println!(">>> wait 6 blocks ...");
        sleep_n_block!(6, interval);

        println!(">>> propose self-delegations...");
        for v in VALIDATOR_LIST.values() {
            delegate::gen_tx(&v.name, FRA, &v.name)
                .c(d!())
                .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
        }

        println!(">>> DONE !");
        Ok(())
    }
}

mod issue {
    use super::*;
    use ledger::{
        data_model::{
            AssetTypeCode, IssueAsset, IssueAssetBody, IssuerKeyPair, Operation,
            TxOutput, ASSET_TYPE_FRA,
        },
        staking::FRA_PRE_ISSUE_AMOUNT,
    };
    use rand_chacha::rand_core::SeedableRng;
    use rand_chacha::ChaChaRng;
    use zei::setup::PublicParams;
    use zei::xfr::{
        asset_record::{build_blind_asset_record, AssetRecordType},
        structs::AssetRecordTemplate,
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
    use super::*;

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
        )
        .c(d!())
        .map(|principal_op| {
            builder.add_operation(principal_op);
            builder.add_operation_delegation(owner_kp, amount, validator.to_owned());
        })?;

        Ok(builder.take_transaction())
    }
}

mod undelegate {
    use super::*;
    use ledger::staking::PartialUnDelegation;

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
    const TD_ADDR_LIST: [&str; NUM] = [
        "611C922247C3BE7EA13455B191B6EFD909F10196",
        "5A006EA8455C6DB35B4B60B7218774B2E589482B",
        "0F64C8259BFCD1A9F6E21958D0A60D9E370D9C13",
        "A9534BB329FE980838EC0FEB7550AD66228D581B",
        "7DEFDDA9E24A1C4320A9D45B8E7F14A40E479713",
        "4C2582DC314575DE73AD1EAA06726E555786900E",
        "82DEBD3B6C108095BDD3FE7352B9C538BDEFA621",
        "EC046D54F2FA16AE7126343425C1E91A96ED18BD",
        "325EC027285ABAA2A755286E1982E8F66633C05B",
        "CF7D19D604FF5EFE7EC90583D5700D7FF1CF63BA",
        "30E07994969FFE8007481914335521CE665BEEFE",
        "59A3EC547FCFA2434F64A09F0B85A9BB6262F71B",
        "88C045F586A338E90CE9A712FC4F13D04764E28F",
        "91F40F5F761DF9A09D9CA7E6200D02551BBA31F1",
        "57AF4341DE9A2A3725123718DEDBA5C7B9141E7D",
        "908D050231F5D568DB11F379DC5B3E8A7C8A453D",
        "D88C6FE77A7F3F84578D6D9AA2718BB034743902",
        "55B8CF069F6F6C75935F8EB5FAC6B8C8138BC954",
        "8424784D8505B2661F120831D18BE0021DD0CDA8",
        "9F832EE81DB4FBDAA8D3541ECA6ECEE0E97C119B",
    ];

    // online env
    #[cfg(not(feature = "debug_env"))]
    const TD_ADDR_LIST: [&str; NUM] = [
        "FD8C65634A9D8899FA14200177AF19D24F6E1C37",
        "0856654F7CD4BB0D6CC4409EF4892136C9D24692",
        "5C97EE9B91D90B332813078957E3A96B304791B4",
        "000E33AB7471186F3B1DE9FC08BB9C480F453590",
        "EA70EB6087E3D606730C4E9062CC24A5BD7D2B37",
        "E5705FED0049EDA431D37B37947A136F22F8F054",
        "9ED0D8D661C99A58F78F80816968E61AAE8DC649",
        "9AB077E00C8B731AE1F82DEC5E45CB3D1E9BBB12",
        "8CB713C8EA32223FCAC66B966FCFA9BAEE257946",
        "EAC5792572EB726AA0DBA9A7AFA9757F8063C6C9",
        "A50D65F2F63F65D845A7C5CBB989FF94D6688F38",
        "A8DFD116BA9664F38958C721688FA73E6320755B",
        "A07875BBD4E062BAB2C162E180237FC3B30C4ABC",
        "39F0C5E451394FAAE7213FD914EFBA8F963CCB90",
        "EE2F73BAA1605C998BB106E5A38DBD79B5209F1D",
        "09EF1DB6B67D1CBF7EBA6BD9B204611848993DF7",
        "AD2C69A9432E8F6634E1ADC3D6CA69EA9E1F4114",
        "510082967DFA7DEBA11267B26A6318D07A457B48",
        "60689516C566F27E03794329C431D0084299480A",
        "5C71532CEEFC43EE3857905AB94FDA505BFC06F3",
    ];

    const MNEMONIC_LIST: [&str; NUM] = [
        "noodle master spare innocent interest waste cram shaft cluster save middle only satoshi huge distance caught case oil rapid muscle tuition normal leader climb",
        "cram case solid tooth adult melody nasty minute glance start pencil travel possible roof unknown lottery race candy jungle weather version speak when gadget",
        "mean wrestle adapt ranch shiver bread napkin spice heavy select design office label material umbrella pause slogan twelve dinner original survey just unveil attend",
        "okay tonight tenant van credit noble baby arrest make say render layer damage elegant spray human truth dog cactus cruel energy reflect net bread",
        "entire vapor brand idea march legal mother fun process elegant segment hamster brother excess never public matrix senior three token pink century area obey",
        "pony celery approve woman shrimp matrix maximum trophy chicken start share naive secret ill slot obey solve kangaroo junior decide tail lamp coffee ethics",
        "note fiction island quote shoulder weapon tide rather whale execute deputy fox enter host crane broom mechanic dial embody fiction win spike isolate horse",
        "hat suit hockey flight bamboo always organ hungry chase garment raise inmate diet anxiety daughter diagram zone whip fiscal include globe cherry reveal such",
        "prevent bleak flush pizza web jump afford grid always dirt clump office drift alarm naive stone empty six aspect engage fashion hobby equal denial",
        "tube voyage assume load aware toilet broccoli genius puzzle angle wear describe clerk enroll hope cash creek million trial cup grain push romance little",
        "violin patch moral envelope decrease account moon mammal swear noble grit water course decade guide smoke egg delay glove ring mushroom absent trash globe",
        "cream rhythm present stumble trust impact evoke oil famous gloom release oven sphere atom pair sausage knee mom gallery deputy usage traffic guitar pulp",
        "issue choice enemy color load push banner bone ridge drastic vendor electric rely amount vibrant write ready slice exercise similar daughter hundred diamond just",
        "turtle thrive weird attitude cat okay super deposit tackle nest primary sock fruit ready erase motion uncle celery promote warrior sense window cheese valley",
        "rhythm short deer inquiry noble paper way canvas foam dynamic never bulk write laptop silk enjoy stay icon carbon obtain divide life stomach random",
        "boring flag lunch banner turtle attitude license empower witness kiwi ethics fade seek before tiger theme summer toy crush adapt stomach stove audit erosion",
        "carbon deny rabbit purse stage bench leave say city video disagree between wrong need credit option ripple random nose brisk tell pave leg recycle",
        "today doll gym surprise valve you effort abstract jacket account extend duck video eagle obtain rigid invite expand embody arch arrow monster clarify wrestle",
        "essay turkey planet sleep edit joy fence aim dice midnight wait clump staff adult east excuse garlic company myth clarify journey bundle quiz hope",
        "decade drift input differ sleep alley risk mutual direct diagram double soon garment subject track labor bubble sweet state laugh festival ridge produce float",
    ];

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
