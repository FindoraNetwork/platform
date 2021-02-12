// TODO: remove this when https://github.com/rust-lang/rust-clippy/issues/6066 gets fixed
#![allow(clippy::needless_collect)]
use crate::display_functions::{
    display_asset_type, display_op_metadata, display_txn, display_txn_builder,
    display_txo_entry,
};
use crate::{
    helpers::{
        compute_findora_dir, do_request, do_request_asset,
        do_request_authenticated_utxo, prompt_mnemonic,
    },
    kv::{MixedPair, NICK_FEE},
};
use crate::{
    print_conf, prompt_for_config, serialize_or_str, AssetTypeEntry, AssetTypeName,
    CliDataStore, CliError, FreshNamer, KeypairName, LedgerStateCommitment,
    NewPublicKeyFetch, NoTransactionInProgress, NoneValue, OpMetadata, PubkeyName,
    TxnBuilderName, TxnMetadata, TxnName, TxoCacheEntry, TxoName,
};

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::Infallible;
use std::fs::File;
use std::io::prelude::*;
use std::process::exit;
use std::{thread, time};

use ledger::data_model::errors::PlatformError;
use ledger::{data_model::*, store::fra_gen_initial_tx};
use ledger::{error_location, zei_fail};
use ledger_api_service::LedgerAccessRoutes;
use promptly::{prompt, prompt_default, prompt_opt};
use snafu::{Backtrace, GenerateBacktrace, OptionExt, ResultExt};

use rand::distributions::Alphanumeric;
use rand::Rng;
use submission_api::SubmissionRoutes;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::PolicyChoice;
use txn_builder::{BuildsTransactions, TransferOperationBuilder};
use utils::NetworkRoute;
use utils::{HashOf, SignatureOf};
use zei::setup::PublicParams;
use zei::xfr::asset_record::{open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::AssetRecordTemplate;

type GlobalState = (
    HashOf<Option<StateCommitmentData>>,
    u64,
    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
);

//////////////////// Simple API  ///////////////////////////////////////////////////////////////

pub fn setup<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    store.update_config(|conf| {
        *conf = prompt_for_config(Some(conf.clone()))?;
        Ok(())
    })?;
    Ok(())
}

pub fn list_config<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    let conf = store.get_config()?;
    print_conf(&conf);
    Ok(())
}

fn check_existing_key_pair<S: CliDataStore>(
    store: &mut S,
    nick: &str,
) -> Result<bool, CliError> {
    let res = if store.exists_keypair(&nick)? {
        println!(
            "Do you want to overwrite the existing key pair? CAUTION: this operation cannot be reverted. You may loose all your funds."
        );
        prompt_default(
            "", // We use println! above to ensure stdout is flushed
            false,
        )?
    } else {
        true
    };

    Ok(res)
}

pub fn key_gen<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
    if NICK_FEE == nick {
        return Err(CliError::NickName {
            msg: NICK_FEE.to_owned() + " is a nick name reserved by the system!",
        });
    }

    // Check if the key already exists
    let continue_key_gen = check_existing_key_pair(store, &nick)?;

    if continue_key_gen {
        // temporary solution, save passphrase here
        let phrase = wallet::generate_mnemonic_custom(24, "en").unwrap();
        let kp = wallet::restore_keypair_from_mnemonic_default(&phrase).unwrap();
        let mut name = compute_findora_dir()?;
        name.push(format!("{}_passphrase", &nick));
        let mut pass_file = File::create(name).unwrap();
        pass_file
            .write_all(phrase.as_ref())
            .expect("Failed to save passphrase");

        // add keys to store
        let pk = *kp.get_pk_ref();
        store.add_key_pair(&KeypairName(nick.to_string()), kp)?;
        store.add_public_key(&PubkeyName(nick.to_string()), pk)?;
        println!("New key pair added for '{}'", nick);
    } else {
        println!("Operation aborted by the user.");
    }

    Ok(())
}

pub fn list_keys<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    let kps = store.get_keypairs()?;
    let pks = store
        .get_pubkeys()?
        .into_iter()
        .map(|(k, pk)| (k.0, pk))
        .filter(|(k, _)| !kps.contains(&KeypairName(k.clone())))
        .map(|x| (x, false))
        .collect::<Vec<_>>();
    let kps = kps.into_iter();
    let mut new_kps = vec![];
    for k in kps {
        let pk = store
            .get_keypair_pubkey(&k)?
            .expect("A public key disappeared from the database.");
        new_kps.push((k, pk));
    }
    let kps = new_kps.into_iter().map(|(k, pk)| ((k.0, pk), true));
    for ((n, k), pair) in kps.chain(pks.into_iter()) {
        println!(
            "{} {}: '{}'",
            if pair { "keypair" } else { "public key" },
            n,
            serde_json::to_string(&k)?
        );
    }
    Ok(())
}

pub fn list_keypair<S: CliDataStore>(
    store: &mut S,
    nick: String,
    show_secret: bool,
) -> Result<(), CliError> {
    if !store.exists_keypair(&nick)? {
        eprintln!("No keypair with name '{}' found", nick);
        exit(-1);
    }

    store.with_keypair::<CliError, _>(
        &KeypairName(nick.to_string()),
        |kp| match kp {
            None => {
                eprintln!("No keypair with name '{}' found", nick);
                exit(-1);
            }
            Some(kp) => {
                if show_secret {
                    let kp = serde_json::to_string(&kp)?;
                    println!("{}", kp);
                } else {
                    let pk = serde_json::to_string(kp.get_pk_ref())?;
                    println!("{}", pk);
                }
                Ok(())
            }
        },
    )?;
    Ok(())
}

pub fn load_key_pair<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let continue_load_key_pair = check_existing_key_pair(store, &nick)?;

    if continue_load_key_pair {
        match serde_json::from_str::<XfrKeyPair>(&prompt::<String, _>(format!(
            "Please paste in the key pair for '{}'",
            nick
        ))?) {
            Err(e) => {
                eprintln!("Could not parse key pair: {}", e);
                exit(-1);
            }
            Ok(kp) => {
                store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())?;
                store.add_key_pair(&KeypairName(nick.to_string()), kp)?;
                println!("New key pair added for '{}'", nick);
            }
        }
    } else {
        println!("Operation aborted by the user.");
    }
    Ok(())
}

pub fn load_public_key<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    match serde_json::from_str(&prompt::<String, _>(format!(
        "Please paste in the public key for '{}'",
        nick
    ))?) {
        Err(e) => {
            eprintln!("Could not parse public key: {}", e);
            exit(-1);
        }
        Ok(pk) => {
            for (n, n_pk) in store.get_pubkeys()? {
                if pk == n_pk {
                    eprintln!("This public key is already registered as '{}'", n.0);
                    exit(-1);
                }
            }
            store.add_public_key(&PubkeyName(nick.to_string()), pk)?;
            println!("New public key added for '{}'", nick);
        }
    }
    Ok(())
}

pub fn delete_keypair<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    store.with_keypair::<CliError, _>(&KeypairName(nick.to_string()), |kp| {
        if kp.is_none() {
            eprintln!("No keypair with name '{}' found", nick);
            exit(-1);
        }
        Ok(())
    })?;
    if prompt_default(
        format!("Are you sure you want to delete keypair '{}'?", nick),
        false,
    )? {
        // TODO: do this atomically?
        store.delete_keypair(&KeypairName(nick.to_string()))?;
        store.delete_pubkey(&PubkeyName(nick.to_string()))?;
        println!("Keypair '{}' deleted", nick);
    }
    Ok(())
}

pub fn delete_public_key<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let pk = store.get_pubkey(&PubkeyName(nick.to_string()))?;
    let has_kp = store
        .get_keypair_pubkey(&KeypairName(nick.to_string()))?
        .is_some();
    match (pk, has_kp) {
        (None, _) => {
            eprintln!("No public key with name '{}' found", nick);
            exit(-1);
        }
        (Some(_), true) => {
            eprintln!(
                "'{}' is a keypair. Please use delete-keypair instead.",
                nick
            );
            exit(-1);
        }
        (Some(_), false) => {
            if prompt_default(
                format!("Are you sure you want to delete public key '{}'?", nick),
                false,
            )? {
                store.delete_pubkey(&PubkeyName(nick.to_string()))?;
                println!("Public key '{}' deleted", nick);
            }
        }
    }
    Ok(())
}

pub fn restore_from_mnemonic_bip44<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let continue_restore = check_existing_key_pair(store, &nick)?;
    if !continue_restore {
        println!("Error: Database already contains a key for {}.", nick);
        println!("Please delete the existing key first, or use a different nickname.");
        exit(-1);
    }

    let phrase = prompt_mnemonic(Some(&nick)).expect("Failed to read mnemonic");
    match wallet::restore_keypair_from_mnemonic_default(&phrase) {
        Ok(kp) => {
            store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())?;
            store.add_key_pair(&KeypairName(nick.to_string()), kp)?;
            println!("New key pair added for '{}'", nick);
        }
        Err(e) => {
            eprintln!("Could not restore key pair: {}", e);
            exit(-1);
        }
    }
    Ok(())
}

pub fn pic_random_txn_number() -> String {
    rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(10)
        .collect::<String>()
}

pub fn simple_define_asset<S: CliDataStore>(
    store: &mut S,
    issuer_nick: String,
    asset_nick: String,
    is_fra: bool,
) -> Result<(), CliError> {
    query_ledger_state(store, true)?; // TODO Why true?

    let nick_tx = pic_random_txn_number();

    prepare_transaction(store, nick_tx.clone())?;

    define_asset(store, nick_tx.clone(), issuer_nick, asset_nick, is_fra)?;

    build_transaction(store)?;

    submit(store, nick_tx)?;

    Ok(())
}

pub fn simple_issue_asset<S: CliDataStore>(
    store: &mut S,
    asset_nick: String,
    amount: u64,
) -> Result<(), CliError> {
    let nick_tx = pic_random_txn_number();

    prepare_transaction(store, nick_tx.clone())?;

    let seq_issue_number = query_asset_issuance_num(store, asset_nick.clone())?;

    issue_asset(store, nick_tx.clone(), asset_nick, seq_issue_number, amount)?;

    build_transaction(store)?;

    submit(store, nick_tx)?;

    Ok(())
}

pub fn list_public_key<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let pk = store.get_pubkey(&PubkeyName(nick.to_string()))?;
    let pk = pk
        .map(|x| serde_json::to_string(&x).unwrap())
        .unwrap_or(format!("No public key with name {} found", nick));
    println!("{}", pk);
    Ok(())
}

pub fn compute_balances<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    // Build a map pubkey => pubkey=> name
    let mut pub_key_to_name_map: HashMap<String, String> = HashMap::new();

    // We only loop over local key pairs
    let public_keys = store.get_local_pubkeys()?;

    for pk in public_keys {
        let pk_name = (pk.0).0;
        let pk_str = serde_json::to_string(&pk.1).unwrap();
        pub_key_to_name_map.insert(pk_str.clone(), pk_name.clone());
    }

    let mut balances: BTreeMap<(String, String), u64> = BTreeMap::new();

    // Loop over the utxos to compute the balances
    for (_nick, txo) in store.get_cached_txos()?.into_iter() {
        if !txo.unspent {
            continue;
        }

        // Fetch the amount
        let amount = txo.record.record.amount.get_amount();
        let amount = match amount {
            None => match txo.opened_record {
                None => 0_u64,
                Some(oar) => oar.amount,
            },
            Some(amt) => amt,
        };

        // Fetch the asset type
        let asset_type = txo.asset_type;
        let asset_type_name = match asset_type {
            None => {
                return Err(CliError::NoneValue {
                    backtrace: Backtrace::generate(),
                });
            } // This should not happen
            Some(asset_type) => asset_type.0,
        };

        // Compute and store the balances
        let pk = txo.record.record.public_key;
        let pk_str = serde_json::to_string(&pk).unwrap();
        let pk_name = pub_key_to_name_map.get(&pk_str);

        match pk_name {
            Some(pk_name_content) => {
                let the_balance = balances
                    .entry((pk_name_content.to_string(), asset_type_name.to_string()))
                    .or_insert(0);
                *the_balance += amount;
            }
            None => {
                // The public key does not correspond to some locally stored private key
                continue;
            }
        }
    }

    // Print the balances
    println!("=== Balances ===");
    for ((pk, asset_type), amount) in balances {
        println!("({},{}):{}", pk, asset_type, amount);
    }

    Ok(())
}

//////////////////// Advanced API  /////////////////////////////////////////////////////////////////

pub fn query_ledger_state<S: CliDataStore>(
    store: &mut S,
    forget_old_key: bool,
) -> Result<(), CliError> {
    store.update_config(|conf| {
        let mut new_key = forget_old_key;
        if !new_key && conf.ledger_sig_key.is_none() {
            println!("No signature key found for '{}'.", conf.ledger_server);
            new_key = new_key || prompt_default(" Retrieve a new one?", false)?;
            if !new_key {
                eprintln!("Cannot check ledger state validity without a signature key.");
                exit(-1);
            }
        }

        if new_key {
            let query = format!(
                "{}{}",
                conf.ledger_server,
                LedgerAccessRoutes::PublicKey.route()
            );
            let resp: XfrPublicKey =
                do_request::<XfrPublicKey>(&query).context(NewPublicKeyFetch)?;

            println!(
                "Saving ledger signing key '{}'",
                serde_json::to_string(&resp)?
            );
            conf.ledger_sig_key = Some(resp);
        }

        assert!(conf.ledger_sig_key.is_some());

        let query = format!(
            "{}{}",
            conf.ledger_server,
            LedgerAccessRoutes::GlobalState.route()
        );
        let resp: GlobalState =
            do_request::<GlobalState>(&query).map_err(|_| CliError::IOError {
                msg: format!("Error with http request to {}", query),
            })?;

        conf.ledger_state = Some(LedgerStateCommitment(resp));

        assert!(conf.ledger_state.is_some());

        println!("New state retrieved.");

        print_conf(&conf);
        Ok(())
    })?;
    Ok(())
}

fn query_asset_issuance_num<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<u64, CliError> {
    // Fetch and store Asset issuance sequence number
    let asset: AssetTypeEntry;
    match store.get_asset_type(&AssetTypeName(nick.clone()))? {
        None => {
            eprintln!("No asset type with name '{}' found", nick);
            exit(-1);
        }
        Some(a) => {
            asset = a;
        }
    }

    let conf = store.get_config()?;
    let codeb64 = asset.asset.code.to_base64();
    let query = format!(
        "{}{}",
        conf.ledger_server,
        LedgerAccessRoutes::AssetIssuanceNum.with_arg(&codeb64)
    );

    let resp: u64 = do_request::<u64>(&query).map_err(|_| CliError::IOError {
        msg: format!("Error with http request to {}", query),
    })?;

    Ok(resp)
}

pub fn list_txos<S: CliDataStore>(
    store: &mut S,
    unspent: bool,
    id: Option<String>,
) -> Result<(), CliError> {
    for (nick, txo) in store.get_cached_txos()?.into_iter() {
        if !txo.unspent && unspent {
            continue;
        }

        if id.clone().is_some() {
            match txo.owner.clone() {
                None => {
                    continue;
                }
                Some(owner_name) => {
                    if owner_name.0 != id.clone().unwrap() {
                        continue;
                    }
                }
            }
        }

        println!("TXO '{}'", nick.0);
        display_txo_entry(1, &txo);
    }
    println!("Done.");
    Ok(())
}

pub fn list_txo<S: CliDataStore>(store: &mut S, id: String) -> Result<(), CliError> {
    let txo = match store.get_cached_txo(&TxoName(id.clone()))? {
        None => {
            eprintln!("No txo '{}' found.", id);
            exit(-1);
        }
        Some(s) => s,
    };
    display_txo_entry(0, &txo);
    Ok(())
}

pub fn show_owner_memo<S: CliDataStore>(
    store: &mut S,
    id: String,
) -> Result<(), CliError> {
    match store.get_cached_txo(&TxoName(id.clone()))? {
        None => {
            eprintln!("No txo '{}' found.", id);
            exit(-1);
        }
        Some(txo) => {
            println!("{}", serde_json::to_string(&txo.owner_memo)?);
        }
    };
    Ok(())
}

pub fn load_owner_memo<S: CliDataStore>(
    store: &mut S,
    overwrite: bool,
    id: String,
) -> Result<(), CliError> {
    match store.get_cached_txo(&TxoName(id.clone()))? {
        None => {
            eprintln!("No txo '{}' found.", id);
            exit(-1);
        }
        Some(mut txo) => {
            if !overwrite && txo.owner_memo.is_some() {
                eprintln!("Txo '{}' already has an owner memo!", id);
                exit(-1);
            }
            txo.owner_memo = serde_json::from_str(&prompt::<String, _>("Owner memo?")?)?;
            store.cache_txo(&TxoName(id), txo)?;
        }
    }
    Ok(())
}

pub fn unlock_txo<S: CliDataStore>(store: &mut S, id: String) -> Result<(), CliError> {
    let mut txo = match store.get_cached_txo(&TxoName(id.clone()))? {
        None => {
            eprintln!("No txo '{}' found.", id);
            exit(-1);
        }
        Some(s) => s,
    };
    // if txo.opened_record.is_some() {
    //   eprintln!("Txo '{}' is already open.", id);
    //   return Ok(());
    // }

    match txo.owner.clone() {
        None => {
            eprintln!("I don't know who owns '{}'!", id);
            exit(-1);
        }
        Some(owner) => {
            let owner = KeypairName(owner.0);
            let asset_types = store.get_asset_types()?.into_iter();
            store.with_keypair::<PlatformError, _>(&owner, |kp| match kp {
                None => {
                    eprintln!("No keypair found for '{}'.", owner.0);
                    exit(-1);
                }
                Some(kp) => {
                    let open_rec = open_blind_asset_record(
                        &txo.record.record,
                        &txo.owner_memo,
                        kp.get_sk_ref(),
                    )
                    .map_err(|x| zei_fail!(x))?;
                    let tp = open_rec.asset_type;
                    txo.opened_record = Some(open_rec);

                    if txo.asset_type.is_none() {
                        let tp = AssetTypeCode { val: tp };
                        for (n, asset) in asset_types {
                            if tp == asset.asset.code {
                                txo.asset_type = Some(n);
                                break;
                            }
                        }
                    }

                    println!("Opened '{}':", id);
                    display_txo_entry(1, &txo);
                    Ok(())
                }
            })?;
        }
    }

    store.cache_txo(&TxoName(id), txo)?;

    Ok(())
}

pub fn query_txos<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    let txos = store.get_cached_txos()?;
    for (n, txo) in txos {
        if let Some(sid) = txo.sid {
            query_txo(store, n.0, Some(sid.0))?;
        }
    }
    Ok(())
}

pub fn query_txo<S: CliDataStore>(
    store: &mut S,
    nick: String,
    sid: Option<u64>,
) -> Result<(), CliError> {
    let nick = TxoName(nick);
    let mut sid = sid;
    if let Some(orig_ent) = store.get_cached_txo(&nick)? {
        if let Some(orig_sid) = orig_ent.sid {
            if let Some(new_sid) = sid {
                if orig_sid.0 != new_sid {
                    eprintln!(
                        "TXO nicknamed '{}' refers to SID {}, not {}",
                        nick.0, orig_sid.0, new_sid
                    );
                    exit(-1);
                }
            } else {
                println!("TXO nicknamed '{}' refers to SID {}.", nick.0, orig_sid.0);
                sid = Some(orig_sid.0);
            }
        }
    }

    let sid = match sid {
        None => {
            eprintln!("No TXO nicknamed '{}' found and no SID given!", nick.0);
            exit(-1);
        }
        Some(s) => s,
    };

    let conf = store.get_config()?;
    let ledger_state = match conf.ledger_state.as_ref() {
        None => {
            eprintln!(concat!(
                "I don't know what the ledger's state is!\n",
                "Please run query-ledger-state first."
            ));
            exit(-1);
        }
        Some(s) => s.clone(),
    };

    let query = format!(
        "{}{}/{}",
        conf.ledger_server,
        LedgerAccessRoutes::UtxoSid.route(),
        sid
    );

    let resp: AuthenticatedUtxo =
        do_request_authenticated_utxo(&query, sid, &ledger_state)?;

    // TODO: do something better to ensure that we pull any existing
    // things from orig_ent
    let mut ent = TxoCacheEntry {
        sid: Some(TxoSID(sid)),
        owner: None,
        asset_type: None,
        ledger_state: Some(ledger_state),
        record: resp.utxo.0,
        owner_memo: None,
        opened_record: None,
        unspent: true,
    };

    if let Some(orig_ent) = store.get_cached_txo(&nick)? {
        if orig_ent.sid.is_some() {
            assert_eq!(orig_ent.sid, ent.sid);
        }
        assert!(orig_ent.unspent);
        assert_eq!(ent.record, orig_ent.record);
        ent.owner_memo = orig_ent.owner_memo;
        ent.opened_record = orig_ent.opened_record;
        if let Some(orig_state) = orig_ent.ledger_state {
            assert!(
                (orig_state.0).1 <= (ent.ledger_state.as_ref().context(NoneValue)?.0).1
            );
        }
        ent.owner = orig_ent.owner;
        ent.asset_type = orig_ent.asset_type;
    }

    if ent.owner.is_none() {
        for (n, pk) in store.get_pubkeys()?.into_iter() {
            if pk == ent.record.record.public_key {
                ent.owner = Some(n);
                break;
            }
        }
    }

    if ent.asset_type.is_none() {
        if let Some(tp) = ent.record.record.asset_type.get_asset_type() {
            let tp = AssetTypeCode { val: tp };
            for (n, asset) in store.get_asset_types()?.into_iter() {
                if tp == asset.asset.code {
                    ent.asset_type = Some(n);
                    break;
                }
            }
        }
    }

    println!("TXO entry {} updated:", nick.0);
    display_txo_entry(1, &ent);

    store.cache_txo(&nick, ent)?;

    // TODO: this is... jank as hell.
    for (n, (_, meta)) in store.get_built_transactions()? {
        if let Some(fin) = meta.finalized_txos.as_ref() {
            if let Some(i) = fin.iter().position(|x| x == &nick) {
                store.update_txn_metadata::<CliError, _>(&n, |meta| {
                    (meta.new_txos[i].1).sid = Some(TxoSID(sid));
                    Ok(())
                })?;
            }
        }
    }

    Ok(())
}

pub fn list_asset_types<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    for (nick, a) in store.get_asset_types()?.into_iter() {
        println!("Asset '{}'", nick.0);
        display_asset_type(1, &a);
    }
    Ok(())
}

pub fn list_asset_type<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let a = store.get_asset_type(&AssetTypeName(nick.clone()))?;
    match a {
        None => {
            eprintln!("'{}' does not refer to any known asset type", nick);
            exit(-1);
        }
        Some(a) => {
            display_asset_type(0, &a);
        }
    }
    Ok(())
}

pub fn query_asset_type<S: CliDataStore>(
    store: &mut S,
    replace: bool,
    nick: String,
    code: String,
) -> Result<(), CliError> {
    if !replace
        && store
            .get_asset_type(&AssetTypeName(nick.clone()))?
            .is_some()
    {
        eprintln!("Asset type with the nickname '{}' already exists.", nick);
        exit(-1);
    }

    let conf = store.get_config()?;
    let code_b64 = code.clone();
    let _ = AssetTypeCode::new_from_base64(&code)?;
    let query = format!(
        "{}{}/{}",
        conf.ledger_server,
        LedgerAccessRoutes::AssetToken.route(),
        code_b64
    );
    let resp = do_request_asset(&query)?;

    let issuer_nick = {
        let mut ret = None;
        for (n, pk) in store.get_pubkeys()?.into_iter() {
            if pk == resp.issuer.key {
                ret = Some(n);
                break;
            }
        }
        ret
    };

    let asset_code = resp.code.val;
    let ret = AssetTypeEntry {
        asset: Box::new(resp),
        issuer_nick,
        issue_seq_num: 0,
    };
    store.add_asset_type(&AssetTypeName(nick.clone()), ret)?;

    let issue_seq_number = query_asset_issuance_num(store, nick.clone())?;

    store.update_asset_type::<Infallible, _>(&AssetTypeName(nick.clone()), |a| {
        a.issue_seq_num = issue_seq_number;
        Ok(())
    })?;

    for (n, mut txo) in store.get_cached_txos()? {
        if txo.asset_type.is_none() {
            if let Some(open_rec) = txo.opened_record.as_ref() {
                if open_rec.asset_type == asset_code {
                    txo.asset_type = Some(AssetTypeName(nick.clone()));
                    store.cache_txo(&n, txo)?;
                }
            }
        }
    }

    println!("Asset type '{}' saved as '{}'", code_b64, nick);
    Ok(())
}

pub fn prepare_transaction<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let seq_id = match store.get_config()?.ledger_state {
        None => {
            eprintln!(concat!(
                "I don't know what block ID the ledger is on!\n",
                "Please run query-ledger-state first."
            ));
            exit(-1);
        }
        Some(s) => (s.0).1,
    };

    if store
        .get_txn_builder(&TxnBuilderName(nick.clone()))?
        .is_some()
    {
        eprintln!(
            "Transaction builder with the name '{}' already exists.",
            nick
        );
        exit(-1);
    }

    println!(
        "Preparing transaction '{}' for block id '{}'...",
        nick, seq_id
    );
    store.prepare_transaction(&TxnBuilderName(nick.clone()), seq_id)?;
    store.update_config(|conf| {
        conf.active_txn = Some(TxnBuilderName(nick));
        Ok(())
    })?;
    println!("Done.");
    Ok(())
}

pub fn list_txn<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    // Fetch the name of the current transaction
    let nick = store
        .get_config()?
        .active_txn
        .context(NoTransactionInProgress)?;

    let builder = match store.get_txn_builder(&TxnBuilderName(nick.0.clone()))? {
        None => {
            eprintln!("No txn builder '{}' found.", nick.0);
            exit(-1);
        }
        Some(s) => s,
    };

    display_txn_builder(0, &builder);
    Ok(())
}

pub fn list_built_transaction<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let txn = match store.get_built_transaction(&TxnName(nick.clone()))? {
        None => {
            eprintln!("No txn '{}' found.", nick);
            exit(-1);
        }
        Some(s) => s,
    };
    display_txn(0, &txn);
    Ok(())
}

pub fn list_built_transactions<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    for (nick, txn) in store.get_built_transactions()?.into_iter() {
        println!("{}:", nick.0);
        display_txn(1, &txn);
    }
    Ok(())
}

pub fn status<S: CliDataStore>(store: &mut S, txn: String) -> Result<(), CliError> {
    let txn = match store.get_built_transaction(&TxnName(txn.clone()))? {
        None => {
            eprintln!("No txn '{}' found.", txn);
            exit(-1);
        }
        Some(s) => s,
    };
    println!(
        "handle {}: {}",
        serialize_or_str(&txn.1.handle, "<UNKNOWN>"),
        serialize_or_str(&txn.1.status, "<UNKNOWN>")
    );
    Ok(())
}

fn get_status<S: CliDataStore>(
    store: &mut S,
    txn: String,
) -> Result<TxnStatus, CliError> {
    let conf = store.get_config()?;
    let txn_nick = txn.clone();
    let txn = match store.get_built_transaction(&TxnName(txn.clone()))? {
        None => {
            eprintln!("No txn '{}' found.", txn);
            exit(-1);
        }
        Some(s) => s,
    };

    let handle;
    match txn.1.handle.as_ref() {
        None => {
            eprintln!(
                "No handle for txn '{}' found. Have you submitted it?",
                txn_nick
            );
            exit(-1);
        }
        Some(h) => {
            handle = h;
        }
    }

    let query = format!(
        "{}{}/{}",
        conf.submission_server,
        SubmissionRoutes::TxnStatus.route(),
        handle.0
    );
    let resp = do_request::<TxnStatus>(&query).map_err(|_| CliError::IOError {
        msg: format!("Error with http request to {}", query),
    })?;
    Ok(resp)
}

pub fn status_check<S: CliDataStore>(
    store: &mut S,
    txn_nick: String,
) -> Result<(), CliError> {
    let resp = get_status(store, txn_nick.clone())?;

    println!("Got status: {}", serde_json::to_string(&resp)?);
    let txn = match store.get_built_transaction(&TxnName(txn_nick.clone()))? {
        None => {
            eprintln!("No txn '{}' found.", txn_nick);
            exit(-1);
        }
        Some(s) => s,
    };
    let metadata = txn.1;
    update_if_committed(store, resp.clone(), metadata, txn_nick.clone())?;
    store.update_txn_metadata::<std::convert::Infallible, _>(
        &TxnName(txn_nick),
        |metadata| {
            metadata.status = Some(resp);
            Ok(())
        },
    )?;
    Ok(())
}

pub fn update_if_committed<S: CliDataStore>(
    store: &mut S,
    resp: TxnStatus,
    metadata: TxnMetadata,
    nick: String,
) -> Result<(), CliError> {
    if let TxnStatus::Committed((_, txo_sids)) = resp {
        println!("Committed!");
        if let Some(TxnStatus::Committed(_)) = metadata.status {
            println!("Not updating, status is {:?}", &metadata.status);
        } else {
            println!("Updating, status is {:?}", &metadata.status);
            for nick in metadata.spent_txos.iter() {
                println!("Spending TXO '{}'...", nick.0);
                let mut txo = store.get_cached_txo(nick)?.context(NoneValue)?;
                assert!(txo.unspent);
                txo.unspent = false;
                store.cache_txo(nick, txo)?;
            }

            for (nick, ent) in metadata.new_asset_types.iter() {
                println!("New asset type '{}'...", nick.0);
                store.add_asset_type(&nick, ent.clone())?;
            }

            let new_txos = metadata
                .new_txos
                .iter()
                .filter(|x| x.1.unspent)
                .collect::<Vec<_>>();
            assert_eq!(new_txos.len(), txo_sids.len());
            for pref in FreshNamer::new(nick.clone(), ".".to_string()) {
                let mut entries = vec![];
                let mut bad = false;
                for ((n, txo), sid) in new_txos.iter().zip(txo_sids.iter()) {
                    let mut txo = txo.clone();
                    let n = format!("{}:{}", pref, n);

                    if store.get_cached_txo(&TxoName(n.clone()))?.is_some() {
                        bad = true;
                        break;
                    }

                    txo.sid = Some(*sid);
                    entries.push((TxoName(n), txo));
                }

                if !bad {
                    let mut finalized = vec![];
                    for (k, v) in entries {
                        println!("Caching TXO '{}':", k.0);
                        display_txo_entry(1, &v);
                        store.cache_txo(&k, v)?;
                        finalized.push(k);
                    }

                    store.update_txn_metadata::<std::convert::Infallible, _>(
                        &TxnName(nick),
                        |metadata| {
                            assert!(metadata.finalized_txos.is_none());
                            metadata.finalized_txos = Some(finalized);
                            Ok(())
                        },
                    )?;
                    break;
                }
            }
            println!("Done caching TXOs.");
        }
    } else {
        println!("NOT: {:?}", resp);
    }
    Ok(())
}

pub fn define_asset<S: CliDataStore>(
    store: &mut S,
    txn_nick: String,
    issuer_nick: String,
    asset_nick: String,
    is_fra: bool,
) -> Result<(), CliError> {
    if is_fra {
        define_asset_x(
            store,
            txn_nick,
            issuer_nick,
            asset_nick,
            Some(AssetTypeCode::new_from_vec(
                ASSET_TYPE_FRA_BYTES[..].to_vec(),
            )),
        )
    } else {
        define_asset_x(store, txn_nick, issuer_nick, asset_nick, None)
    }
}

pub fn define_asset_x<S: CliDataStore>(
    store: &mut S,
    txn_nick: String,
    issuer_nick: String,
    asset_nick: String,
    asset_code: Option<AssetTypeCode>,
) -> Result<(), CliError> {
    let issuer_nick = KeypairName(issuer_nick);
    let config = store.get_config()?;
    let builder_opt = Some(txn_nick).map(TxnBuilderName).or(config.active_txn);

    let builder_name;
    match builder_opt {
        None => {
            eprintln!("I don't know which transaction to use!");
            exit(-1);
        }
        Some(t) => {
            builder_name = t;
        }
    }

    let mut new_builder;
    match store.get_txn_builder(&builder_name)? {
        None => {
            eprintln!("Transaction builder '{}' not found.", builder_name.0);
            exit(-1);
        }
        Some(b) => {
            new_builder = b;
        }
    }

    store.with_keypair::<PlatformError, _>(&issuer_nick, |kp| match kp {
        None => {
            let err_msg = format!("No key pair '{}' found.", issuer_nick.0);
            eprintln!("{}", err_msg);
            Err(PlatformError::IoError(err_msg))
        }
        Some(kp) => {
            let mut asset_rules: AssetRules = Default::default();
            let max_units = prompt::<u64, _>("max units? (default=unlimited)").unwrap();
            if max_units > 0 {
                asset_rules.set_max_units(Some(max_units));
            }
            let updatable = prompt_default("memo updatable?", false).unwrap();
            asset_rules.set_updatable(updatable);
            new_builder.builder.add_operation_create_asset(
                &kp,
                asset_code,
                asset_rules,
                &prompt::<String, _>("memo?").map_err(|_| {
                    PlatformError::IoError(String::from(
                        "It was not possible to read the memo.",
                    ))
                })?,
                PolicyChoice::Fungible(),
            )?;
            Ok(())
        }
    })?;

    store.with_txn_builder::<PlatformError, _>(&builder_name, |builder| {
        *builder = new_builder;
        match builder.builder.transaction().body.operations.last() {
            Some(Operation::DefineAsset(def)) => {
                builder.new_asset_types.insert(
                    AssetTypeName(asset_nick.clone()),
                    AssetTypeEntry {
                        asset: def.body.asset.clone(),
                        issuer_nick: Some(PubkeyName(issuer_nick.0.clone())),
                        issue_seq_num: 0_u64,
                    },
                );
            }
            _ => {
                panic!("The transaction builder doesn't include our operation!");
            }
        }
        if !builder.signers.contains(&issuer_nick) {
            builder.signers.push(issuer_nick.clone());
        }
        builder.operations.push(OpMetadata::DefineAsset {
            issuer_nick: PubkeyName(issuer_nick.0.clone()),
            asset_nick: AssetTypeName(asset_nick.clone()),
        });
        println!("{}:", asset_nick);

        display_asset_type(
            1,
            builder
                .new_asset_types
                .get(&AssetTypeName(asset_nick.clone()))
                .ok_or_else(|| {
                    PlatformError::InputsError(format!(
                        "Asset type with name {:?} is None.",
                        &AssetTypeName(asset_nick.clone())
                    ))
                })?,
        );
        Ok(())
    })?;
    Ok(())
}

pub fn issue_asset<S: CliDataStore>(
    store: &mut S,
    txn_nick: String,
    asset_nick: String,
    issue_seq_num: u64,
    amount: u64,
) -> Result<(), CliError> {
    let config = store.get_config()?;
    let builder_opt = Some(txn_nick).map(TxnBuilderName).or(config.active_txn);
    let builder_nick;
    match builder_opt {
        None => {
            eprintln!("I don't know which transaction to use!");
            exit(-1);
        }
        Some(t) => {
            builder_nick = t;
        }
    }

    let builder_opt = store.get_txn_builder(&builder_nick)?;
    let mut builder;
    match builder_opt {
        None => {
            eprintln!("Transaction builder '{}' not found.", builder_nick.0);
            exit(-1);
        }
        Some(b) => {
            builder = b;
        }
    }

    let asset_nick = AssetTypeName(asset_nick);
    let asset;
    match store
        .get_asset_type(&asset_nick)?
        .or_else(|| builder.new_asset_types.get(&asset_nick).cloned())
    {
        None => {
            eprintln!("No asset type with name '{}' found", asset_nick.0);
            exit(-1);
        }
        Some(a) => {
            asset = a;
        }
    }

    let issuer_nick;
    match asset.issuer_nick.as_ref() {
        None => {
            let asset_issuer_key_str = serde_json::to_string(&asset.asset.issuer.key)
                .expect("The serialization of the asset issuer key failed.");
            eprintln!(
                "I don't know an identity for public key '{}'",
                asset_issuer_key_str
            );
            exit(-1);
        }
        Some(nick) => {
            issuer_nick = KeypairName(nick.0.clone());
        }
    }

    store.with_keypair::<PlatformError, _>(&issuer_nick, |iss_kp| match iss_kp {
         None => {
           eprintln!("No keypair nicknamed '{}' found.", issuer_nick.0);
           exit(-1);
         }
         Some(iss_kp) => {
           println!("IssueAsset: {} of '{}' ({}), authorized by '{}'",
                    amount,
                    asset.asset.code.to_base64(),
                    asset_nick.0,
                    issuer_nick.0);

           builder.builder.add_basic_issue_asset(
        iss_kp, &asset.asset.code, issue_seq_num, amount,
        AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        &PublicParams::new())?;

           let out_name = format!("utxo{}", builder.new_txos.len());

           match builder.builder.transaction().body.operations.last() {
             Some(Operation::IssueAsset(iss)) => {
               assert_eq!(iss.body.records.len(), 1);
               let (txo, memo) = iss.body.records[0].clone();
               let oar = open_blind_asset_record(&txo.record, &memo, iss_kp.get_sk_ref())?;

               builder.new_txos.push((out_name.clone(),
                                      TxoCacheEntry { sid: None,
                                                      asset_type: Some(asset_nick.clone()),
                                                      record: txo,
                                                      owner_memo: memo,
                                                      ledger_state: None,
                                                      owner:
                                                        Some(PubkeyName(issuer_nick.0.clone())),
                                                      opened_record: Some(oar),
                                                      unspent: true }));
             }
             x => {
               panic!("The transaction builder doesn't include our operation! Got {:?}",
                      x);
             }
           }
           if !builder.signers.contains(&issuer_nick) {
             builder.signers.push(issuer_nick.clone());
           }
           builder.operations
                  .push(OpMetadata::IssueAsset { issuer_nick: PubkeyName(issuer_nick.0.clone()),
                                                 asset_nick: asset_nick.clone(),
                                                 output_name: out_name,
                                                 output_amt: amount,
                                                 issue_seq_num });
           Ok(())
         }
       })?;

    store.with_txn_builder::<PlatformError, _>(&builder_nick, |the_builder| {
        *the_builder = builder;
        Ok(())
    })?;

    println!("Successfully added to '{}'", builder_nick.0);

    Ok(())
}

pub fn transfer_assets<S: CliDataStore>(
    store: &mut S,
    builder: Option<String>,
) -> Result<(), CliError> {
    let config = store.get_config()?;
    let builder_opt = builder.map(TxnBuilderName).or(config.active_txn);
    let builder_nick;
    match builder_opt {
        None => {
            eprintln!("I don't know which transaction to use!");
            exit(-1); // TODO return error
        }
        Some(t) => {
            builder_nick = t;
        }
    }

    let builder_opt = store.get_txn_builder(&builder_nick)?;
    let mut builder;
    match builder_opt {
        None => {
            eprintln!("Transaction builder '{}' not found.", builder_nick.0);
            exit(-1); // TODO return error
        }
        Some(b) => {
            builder = b;
        }
    }

    let mut txn_utxos = builder
        .new_txos
        .clone()
        .into_iter()
        .filter(|(_, ent)| ent.unspent)
        .collect::<BTreeMap<_, _>>();

    let txos = store.get_cached_txos()?;

    let mut utxos = BTreeMap::new();

    for (n, ent) in txos.into_iter() {
        if builder.spent_txos.contains(&n) {
            continue;
        }
        let orig_ent = ent.clone();
        match (
            ent.unspent,
            ent.sid,
            ent.opened_record,
            ent.owner,
            ent.asset_type,
        ) {
            (true, Some(sid), Some(opened), Some(owner), Some(asset_type)) => {
                utxos.insert(n, (orig_ent, sid, opened, owner, asset_type));
            }
            _ => {
                continue;
            }
        }
    }

    if utxos.is_empty() {
        println!(
            "There is no input available for building the transaction. You need to issue some asset or receive some funds to your address."
        );
        return Ok(());
    }

    let mut trn_builder = TransferOperationBuilder::new();
    let mut trn_signers = vec![];
    let mut asset_types = BTreeMap::new();
    let mut inp_amounts = BTreeMap::new();
    let mut out_txos = vec![];
    let mut out_tps: Vec<(String, PubkeyName, TxoName)> = vec![];
    let mut trn_inps = vec![];
    let mut trn_outs = vec![];

    {
        let mut first = true;

        while (!utxos.is_empty() || !txn_utxos.is_empty())
            && ({
                let prev = first;
                first = false;
                prev
            } || prompt_default("Add another input?", true)?)
        {
            println!("TXOs from this transaction:");
            for (k, v) in txn_utxos.iter() {
                println!(
                    " {}: {} ({}) of '{}' ({}) owned by '{}'",
                    k,
                    v.opened_record.as_ref().context(NoneValue)?.amount,
                    if v.record.record.amount.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    v.asset_type.as_ref().context(NoneValue)?.0,
                    if v.record.record.asset_type.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    v.owner.as_ref().context(NoneValue)?.0
                );
            }

            println!("Other TXOs:");
            for (k, (ent, _sid, opened, owner, asset_type)) in utxos.iter() {
                println!(
                    " {}: {} ({}) of '{}' ({}) owned by '{}'",
                    k.0,
                    opened.amount,
                    if ent.record.record.amount.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    asset_type.0,
                    if ent.record.record.asset_type.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    owner.0
                );
            }

            if let Some(inp) = prompt_opt::<String, _>("Which input would you like?")? {
                let local_val = match (
                    txn_utxos.contains_key(&inp),
                    utxos.contains_key(&TxoName(inp.clone())),
                ) {
                    (true, false) => true,
                    (false, true) => false,
                    (true, true) => prompt_default(
                        format!(
                            "'{}' is ambiguous -- choose the '{}' from this transaction?",
                            inp, inp
                        ),
                        false,
                    )?,
                    (false, false) => {
                        eprintln!("No TXO with name '{}' found", inp);
                        continue;
                    }
                };

                let (txo_ref, ent) = if local_val {
                    let i = builder
                        .new_txos
                        .iter()
                        .position(|(x, _)| x == &inp)
                        .context(NoneValue)?;
                    builder.new_txos[i].1.unspent = false;
                    (
                        TxoRef::Relative((builder.new_txos.len() - 1 - i) as u64),
                        txn_utxos.remove(&inp).context(NoneValue)?,
                    )
                } else {
                    let inp_k = TxoName(inp.clone());
                    let (ent, sid, _, _, _) = utxos.remove(&inp_k).context(NoneValue)?;
                    builder.spent_txos.push(inp_k);
                    (TxoRef::Absolute(sid), ent)
                };

                println!(
                    " Adding {}: {} ({}) of '{}' ({}) owned by '{}'",
                    inp,
                    ent.opened_record.as_ref().context(NoneValue)?.amount,
                    if ent.record.record.amount.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    ent.asset_type.as_ref().context(NoneValue)?.0,
                    if ent.record.record.asset_type.is_confidential() {
                        "SECRET"
                    } else {
                        "PUBLIC"
                    },
                    ent.owner.as_ref().context(NoneValue)?.0
                );

                let tp = ent.asset_type.as_ref().context(NoneValue)?.0.clone();
                let amt = ent.opened_record.as_ref().context(NoneValue)?.amount;

                *inp_amounts.entry(tp.clone()).or_insert(0) += amt;
                asset_types.insert(
                    tp,
                    (
                        ent.asset_type.clone().context(NoneValue)?,
                        ent.opened_record.as_ref().context(NoneValue)?.asset_type,
                    ),
                );

                trn_inps.push((inp.clone(), ent.clone()));

                trn_signers.push((
                    ent.owner.clone().context(NoneValue)?,
                    inp.clone(),
                    ent.clone(),
                ));

                trn_builder.add_input(
                    txo_ref,
                    ent.opened_record.context(NoneValue)?,
                    /* TODO: tracing policies */ None,
                    /* TODO: identity */ None,
                    amt,
                )?;
            }
        }
    }

    {
        let mut first = true;

        while (!inp_amounts.is_empty())
            && ({
                let prev = first;
                first = false;
                prev
            } || prompt_default("Add another output?", true)?)
        {
            println!("Remaining to spend:");
            for (k, v) in inp_amounts.iter() {
                println!(" {} of '{}'", v, k);
            }

            if let Some(inp) = if inp_amounts.len() == 1 {
                Some(inp_amounts.iter().next().context(NoneValue)?.0.clone())
            } else {
                prompt_opt::<String, _>("Which type of output?")?
            } {
                let amt_remaining = match inp_amounts.get_mut(&inp) {
                    None => {
                        eprintln!("No asset type with name '{}' found", inp);
                        continue;
                    }
                    Some(x) => x,
                };
                let amt = prompt(format!(
                    "How much '{}'? ({} available)",
                    inp, *amt_remaining
                ))?;
                if amt == 0 {
                    eprintln!("Amount must be nonzero.");
                    continue;
                }
                if amt > *amt_remaining {
                    eprintln!("Only {} available.", *amt_remaining);
                    continue;
                }
                let conf_amt = prompt_default("Secret amount?", true)?;

                let conf_tp = prompt_default("Secret asset type?", true)?;
                let receiver = prompt::<String, _>("For whom?")?;
                let receiver = PubkeyName(receiver);

                let pubkey = match store.get_pubkey(&receiver)? {
                    None => {
                        eprintln!("No public key with name '{}' found", receiver.0);
                        continue;
                    }
                    Some(pk) => pk,
                };

                let art = AssetRecordType::from_booleans(conf_amt, conf_tp);
                let template = AssetRecordTemplate::with_no_asset_tracking(
                    amt,
                    asset_types.get(&inp).context(NoneValue)?.1,
                    art,
                    pubkey,
                );

                trn_builder.add_output(
                    &template, /* TODO: tracing */ None,
                    /* TODO: identity */ None, /* TODO: credential */ None,
                )?;

                let txo_name =
                    TxoName(format!("utxo{}", builder.new_txos.len() + out_tps.len()));
                if builder
                    .new_txos
                    .iter()
                    .map(|(x, _)| x.clone())
                    .chain(out_tps.iter().map(|(_, _, x)| x.0.clone()))
                    .any(|x| x == txo_name.0)
                {
                    return Err(CliError::FindoraPlatformError {
                        source: PlatformError::IoError(
                            "Problem trying to build transaction for asset transfer."
                                .to_string(),
                        ),
                    });
                };

                out_tps.push((inp.clone(), receiver.clone(), txo_name.clone()));

                *amt_remaining -= amt;
                if *amt_remaining == 0 {
                    inp_amounts.remove(&inp);
                }
            }
        }
    }

    trn_builder.create(TransferType::Standard)?;

    let trn = match trn_builder.transaction()? {
        Operation::TransferAsset(trn) => trn,
        x => {
            panic!(
                "The transfer builder doesn't include our operation! Got {:?}",
                x
            );
        }
    };
    let outputs = trn
        .get_outputs_ref()
        .into_iter()
        .zip(trn.get_owner_memos_ref());

    for ((inp, receiver, txo_name), (out_record, out_memo)) in
        out_tps.into_iter().zip(outputs)
    {
        let mut txo = TxoCacheEntry {
            sid: None,
            asset_type: Some(AssetTypeName(inp.clone())),
            record: out_record.clone(),
            owner_memo: out_memo.cloned(),
            ledger_state: None,
            owner: Some(receiver.clone()),
            opened_record: None,
            unspent: true,
        };

        if store
            .get_keypairs()?
            .contains(&KeypairName(receiver.0.clone()))
        {
            println!("Recipient '{}' is a local keypair.", receiver.0);
            if prompt_default("Unlock this output?", true)? {
                store.with_keypair::<CliError, _>(
                    &KeypairName(receiver.0.clone()),
                    |kp| {
                        let sk = kp.unwrap().get_sk_ref();
                        txo.opened_record = Some(
                            open_blind_asset_record(
                                &txo.record.record,
                                &txo.owner_memo,
                                sk,
                            )
                            .map_err(|e| CliError::ZeiError { source: e })?,
                        );
                        Ok(())
                    },
                )?;
            }
        }

        trn_outs.push((txo_name.0.clone(), txo.clone()));
        out_txos.push((txo_name.0, txo));
    }

    let mut sig_keys = vec![];
    let mut sigs = vec![];
    for (ix, (s, inp, ent)) in trn_signers.into_iter().enumerate() {
        assert_eq!(Some(s.clone()), ent.owner);
        println!(
            "Signing for input #{} ({}): {} ({}) of '{}' ({}) owned by '{}'",
            ix + 1,
            inp,
            ent.opened_record.as_ref().context(NoneValue)?.amount,
            if ent.record.record.amount.is_confidential() {
                "SECRET"
            } else {
                "PUBLIC"
            },
            ent.asset_type.as_ref().context(NoneValue)?.0,
            if ent.record.record.asset_type.is_confidential() {
                "SECRET"
            } else {
                "PUBLIC"
            },
            ent.owner.as_ref().context(NoneValue)?.0
        );

        if sig_keys.contains(&s.0) {
            println!("'{}' has already signed.", s.0);
            continue;
        }
        sig_keys.push(s.0.clone());

        store.with_keypair::<PlatformError, _>(
            &KeypairName(s.0.clone()),
            |kp| match kp {
                None => {
                    eprintln!("No keypair nicknamed '{}' found.", s.0);
                    exit(-1);
                }
                Some(kp) => {
                    sigs.push(trn_builder.create_input_signature(kp)?);
                    Ok(())
                }
            },
        )?;
    }

    for s in sigs {
        trn_builder.attach_signature(s)?;
    }

    builder.builder.add_operation(trn_builder.transaction()?);
    builder.new_txos.extend(out_txos);

    println!("Adding Transfer:");
    let trn_op = OpMetadata::TransferAssets {
        inputs: trn_inps,
        outputs: trn_outs,
    };
    display_op_metadata(1, &trn_op);

    builder.operations.push(trn_op);

    store.with_txn_builder::<PlatformError, _>(&builder_nick, |the_builder| {
        *the_builder = builder;
        Ok(())
    })?;

    println!("Successfully added to '{}'", builder_nick.0);

    Ok(())
}

pub fn build_transaction<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
    let builder_opt = store.get_config()?.active_txn;

    let nick;
    match builder_opt {
        None => {
            eprintln!("I don't know which transaction to use!");
            exit(-1);
        }
        Some(t) => {
            nick = t;
        }
    }

    let txn_nick = TxnName(nick.0.clone());

    if store.get_built_transaction(&txn_nick)?.is_some() {
        eprintln!("A txn with nickname '{}' already exists", txn_nick.0);
        exit(-1);
    }
    println!("Building '{}'", nick.0);

    let mut metadata: TxnMetadata = Default::default();
    let builder = match store.get_txn_builder(&nick)? {
        None => {
            eprintln!("No txn builder '{}' found.", nick.0); // TODO do we need this?
            exit(-1);
        }
        Some(b) => b,
    };

    let sigs = {
        let mut sigs = vec![];
        for kp_name in builder.signers.iter() {
            store.with_keypair::<std::convert::Infallible, _>(
                &kp_name,
                |kp| match kp {
                    None => {
                        eprintln!(
                            "Could not find keypair for required signer '{}'.",
                            kp_name.0
                        );
                        exit(-1);
                    }
                    Some(kp) => {
                        sigs.push((
                            *kp.get_pk_ref(),
                            SignatureOf::new(kp, &builder.builder.transaction().body),
                        ));
                        Ok(())
                    }
                },
            )?;
        }
        sigs
    };

    store.with_txn_builder::<PlatformError, _>(&nick, |builder| {
        for (pk, sig) in sigs {
            builder.builder.add_signature(&pk, sig)?;
        }

        std::mem::swap(&mut metadata.new_asset_types, &mut builder.new_asset_types);
        std::mem::swap(&mut metadata.new_txos, &mut builder.new_txos);
        std::mem::swap(&mut metadata.spent_txos, &mut builder.spent_txos);
        let mut signers = Default::default();
        std::mem::swap(&mut signers, &mut builder.signers);
        metadata.signers.extend(signers.into_iter());
        std::mem::swap(&mut metadata.operations, &mut builder.operations);
        Ok(())
    })?;

    store.build_transaction(&nick, &txn_nick, metadata)?;

    store.update_config(|conf| {
        conf.active_txn = None;
        Ok(())
    })?;

    println!("Built transaction '{}'", txn_nick.0);
    Ok(())
}

pub fn submit<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
    let (txn, metadata) = store
        .get_built_transaction(&TxnName(nick.clone()))?
        .unwrap_or_else(|| {
            eprintln!("No transaction '{}' found.", nick);
            exit(-1);
        });

    for (nick, _) in metadata.new_asset_types.iter() {
        if store.get_asset_type(&nick)?.is_some() {
            eprintln!("Asset type '{}' already exists!", nick.0);
            exit(-1);
        }
    }

    if let Some(h) = metadata.handle {
        eprintln!(
            "Transaction '{}' has already been submitted. Its handle is: '{}'",
            nick, h.0
        );
        exit(-1);
    }

    let conf = store.get_config()?;
    let query = format!(
        "{}{}",
        conf.submission_server,
        SubmissionRoutes::SubmitTransaction.route()
    );

    println!("Submitting to '{}':", query);
    display_txn(1, &(txn.clone(), metadata.clone()));

    if !prompt_default("Is this correct?", true)? {
        println!("Exiting.");
        return Ok(());
    }

    let client = reqwest::blocking::Client::builder().build()?;
    let resp = client
        .post(&query)
        .json(&txn)
        .send()?
        .error_for_status()?
        .text()?;
    let handle = serde_json::from_str::<TxnHandle>(&resp)?;

    store.update_txn_metadata::<std::convert::Infallible, _>(
        &TxnName(nick.clone()),
        |metadata| {
            metadata.handle = Some(handle.clone());
            Ok(())
        },
    )?;
    println!("Submitted '{}': got handle '{}'", nick, &handle.0);

    // Wait for the transaction to be committed
    let mut committed = false;
    while !committed {
        thread::sleep(time::Duration::from_secs(2));
        let txn_status = get_status(store, nick.clone());

        if let Ok(res) = txn_status {
            committed = matches!(res, TxnStatus::Committed((_, _)));
        }
    }

    if prompt_default("Retrieve its status?", true)? {
        let query = format!(
            "{}{}/{}",
            conf.submission_server,
            SubmissionRoutes::TxnStatus.route(),
            &handle.0
        );
        let resp = do_request::<TxnStatus>(&query);

        match resp {
            Ok(v) => {
                println!("Got status: {:?}", serde_json::to_string(&v));

                update_if_committed(store, v.clone(), metadata, nick.clone())?;

                store.update_txn_metadata::<std::convert::Infallible, _>(
                    &TxnName(nick),
                    |metadata| {
                        metadata.status = Some(v);
                        Ok(())
                    },
                )?;
            }
            _ => {
                return Err(CliError::IOError {
                    msg: format!("Problem with http request with query {}", query),
                });
            }
        }
    }
    Ok(())
}

/// Currently only used for effect verification of negative scenes.
pub fn init_fra<S: CliDataStore>(
    store: &mut S,
    issuer_nick: String,
) -> Result<(), CliError> {
    let query = format!(
        "{}{}",
        store.get_config()?.submission_server,
        SubmissionRoutes::SubmitTransaction.route()
    );

    let name = KeypairName(issuer_nick);
    if let Some(mixed_pair) = store.get_encrypted_keypair(&name)? {
        println!("Please enter current password for {}", name.0);
        let kp = crate::helpers::prompt_with_retries(3, None, |pass| {
            mixed_pair.encrypted(pass.as_bytes())
        })
        .context(crate::Password)?;

        reqwest::blocking::Client::builder()
            .build()?
            .post(&query)
            .json(&fra_gen_initial_tx(&kp))
            .send()?
            .error_for_status()
            .map(|_| ())
            .map_err(|e| dbg!(e).into())
    } else {
        Err(CliError::NickName {
            msg: "Name not found".to_owned(),
        })
    }
}

pub fn export_keypair<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let name = KeypairName(nick);
    let mixed_pair = store.get_encrypted_keypair(&name)?;
    if let Some(mixed_pair) = mixed_pair {
        let key_pair = serde_json::to_string(&mixed_pair).unwrap();
        println!("{}", key_pair);
        Ok(())
    } else {
        println!("Error: No keypair found for {}", name.0);
        exit(-1);
    }
}

pub fn change_keypair_password<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    let name = KeypairName(nick);
    let mixed_pair = store.get_encrypted_keypair(&name)?;
    if let Some(mixed_pair) = mixed_pair {
        println!("Please enter current password for {}", name.0);
        let keypair = crate::helpers::prompt_with_retries(3, None, |pass| {
            mixed_pair.encrypted(pass.as_bytes())
        })
        .context(crate::Password)?;
        println!("Please select a new password for {}", name.0);
        store.add_key_pair(&name, keypair)?;
        Ok(())
    } else {
        println!("Error: No existing keypair found for {}", name.0);
        exit(-1);
    }
}

pub fn import_encrypted_keypair<S: CliDataStore>(
    store: &mut S,
    nick: String,
) -> Result<(), CliError> {
    // Check to make sure the user isn't about to override a key
    if store.exists_keypair(&nick)? {
        println!("Error: Database already contains a key for {}.", nick);
        println!("Please delete the existing key first, or use a different nickname.");
        exit(-1);
    } else {
        // Ask for the keypair
        let serial: String =
            promptly::prompt("Please copy/paste exported, encrypted, keypair")?;
        // Attempt to deserialize it
        let pair: MixedPair<XfrPublicKey, XfrKeyPair> =
            if let Ok(pair) = serde_json::from_str(&serial) {
                pair
            } else {
                println!("Error: Unable to deserialize keypair.");
                exit(-1);
            };
        // Ask the user to enter the password to verify that they know it
        println!("Please verify the password for {}", nick);
        let _ = crate::helpers::prompt_with_retries(3, None, |pass| {
            pair.encrypted(pass.as_bytes())
        })
        .context(crate::Password)?;
        let nick = KeypairName(nick);
        store.add_encrypted_keypair(&nick, pair)?;
        println!("Key has been added to keystore as {}", nick.0);
        Ok(())
    }
}
