use crate::{
  display_asset_type, display_txn, display_txn_builder, display_txo_entry, print_conf,
  prompt_for_config, serialize_or_str, AssetTypeEntry, AssetTypeName, CliDataStore, CliError,
  FreshNamer, KeypairName, LedgerStateCommitment, OpMetadata, PubkeyName, TxnBuilderName,
  TxnMetadata, TxnName, TxoCacheEntry, TxoName,
};

use ledger::data_model::*;
use ledger_api_service::LedgerAccessRoutes;
use promptly::{prompt, prompt_default};
use std::process::exit;
use submission_api::SubmissionRoutes;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::BuildsTransactions;
use txn_builder::PolicyChoice;
use utils::NetworkRoute;
use utils::{HashOf, SignatureOf};
use zei::setup::PublicParams;
use zei::xfr::asset_record::{open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};

use crate::helpers::{do_request, do_request_asset, do_request_authenticated_utxo};
use rand::distributions::Alphanumeric;
use rand::Rng;

type GlobalState = (HashOf<Option<StateCommitmentData>>,
                    u64,
                    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>);

//////////////////// Simple API  ///////////////////////////////////////////////////////////////

pub fn setup<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
  store.update_config(|conf| {
         *conf = prompt_for_config(Some(conf.clone())).unwrap();
       })?;
  Ok(())
}

pub fn list_config<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
  let conf = store.get_config()?;
  print_conf(&conf);
  Ok(())
}

pub fn key_gen<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let kp = XfrKeyPair::generate(&mut rand::thread_rng());
  store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())?;
  store.add_key_pair(&KeypairName(nick.to_string()), kp)?;
  println!("New key pair added for `{}`", nick);
  Ok(())
}

pub fn list_keys<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
  let kps = store.get_keypairs()?;
  let pks = store.get_pubkeys()?
                 .into_iter()
                 .map(|(k, pk)| (k.0, pk))
                 .filter(|(k, _)| !kps.contains(&KeypairName(k.clone())))
                 .map(|x| (x, false))
                 .collect::<Vec<_>>();
  let kps = kps.into_iter();
  let mut new_kps = vec![];
  for k in kps {
    let mut pk = None;
    store.with_keypair::<std::convert::Infallible, _>(&k, |kp| match kp {
           None => panic!("A key disappeared from the database! {}", k.0),
           Some(kp) => {
             pk = Some(*kp.get_pk_ref());
             Ok(())
           }
         })?;
    new_kps.push((k, pk.unwrap()));
  }
  let kps = new_kps.into_iter().map(|(k, pk)| ((k.0, pk), true));
  for ((n, k), pair) in kps.chain(pks.into_iter()) {
    println!("{} {}: `{}`",
             if pair { "keypair" } else { "public key" },
             n,
             serde_json::to_string(&k).unwrap());
  }
  Ok(())
}

pub fn list_keypair<S: CliDataStore>(store: &mut S,
                                     nick: String,
                                     show_secret: bool)
                                     -> Result<(), CliError> {
  store.with_keypair::<CliError, _>(&KeypairName(nick.to_string()), |kp| match kp {
         None => {
           eprintln!("No keypair with name `{}` found", nick);
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
       })?;
  Ok(())
}

pub fn load_key_pair<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  match serde_json::from_str::<XfrKeyPair>(&prompt::<String,_>(format!("Please paste in the key pair for `{}`",nick)).unwrap()) {
    Err(e) => {
      eprintln!("Could not parse key pair: {}",e);
      exit(-1);
    }
    Ok(kp) => {
      store.add_public_key(&PubkeyName(nick.to_string()), *kp.get_pk_ref())
        ?;
      store.add_key_pair(&KeypairName(nick.to_string()), kp)
        ?;
      println!("New key pair added for `{}`", nick);
    }
  }
  Ok(())
}

pub fn load_public_key<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  match serde_json::from_str(&prompt::<String,_>(format!("Please paste in the public key for `{}`",nick))?) {
    Err(e) => {
      eprintln!("Could not parse key pair: {}",e);
      exit(-1);
    }
    Ok(pk) => {
      for (n,n_pk) in store.get_pubkeys()? {
        if pk == n_pk {
          eprintln!("This public key is already registered as `{}`",n.0);
          exit(-1);
        }
      }
      store.add_public_key(&PubkeyName(nick.to_string()), pk)
        ?;
      println!("New public key added for `{}`", nick);
    }
  }
  Ok(())
}

pub fn delete_keypair<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  store.with_keypair::<CliError, _>(&KeypairName(nick.to_string()), |kp| {
         if kp.is_none() {
           eprintln!("No keypair with name `{}` found", nick);
           exit(-1);
         }
         Ok(())
       })?;
  if prompt_default(format!("Are you sure you want to delete keypair `{}`?", nick),
                    false)?
  {
    // TODO: do this atomically?
    store.delete_keypair(&KeypairName(nick.to_string()))?;
    store.delete_pubkey(&PubkeyName(nick.to_string()))?;
    println!("Keypair `{}` deleted", nick);
  }
  Ok(())
}

pub fn delete_public_key<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let pk = store.get_pubkey(&PubkeyName(nick.to_string()))?;
  let mut has_kp = false;
  store.with_keypair::<std::convert::Infallible, _>(&KeypairName(nick.to_string()), |kp| {
         has_kp = kp.is_some();
         Ok(())
       })?;
  match (pk, has_kp) {
    (None, _) => {
      eprintln!("No public key with name `{}` found", nick);
      exit(-1);
    }
    (Some(_), true) => {
      eprintln!("`{}` is a keypair. Please use delete-keypair instead.",
                nick);
      exit(-1);
    }
    (Some(_), false) => {
      if prompt_default(format!("Are you sure you want to delete public key `{}`?", nick),
                        false)?
      {
        store.delete_pubkey(&PubkeyName(nick.to_string()))?;
        println!("Public key `{}` deleted", nick);
      }
    }
  }
  Ok(())
}

pub fn pic_random_txn_number() -> String {
  rand::thread_rng().sample_iter(&Alphanumeric)
                    .take(10)
                    .collect::<String>()
}

pub fn simple_define_asset<S: CliDataStore>(store: &mut S,
                                            issuer_nick: String,
                                            asset_nick: String)
                                            -> Result<(), CliError> {
  query_ledger_state(store, true)?; // TODO Why true?

  let nick_tx = pic_random_txn_number();

  prepare_transaction(store, nick_tx.clone(), true)?;

  define_asset(store, Some(nick_tx.clone()), issuer_nick, asset_nick)?;

  build_transaction(store, Some(nick_tx.clone()), Some(nick_tx.clone()), true)?;

  submit(store, nick_tx)?;

  Ok(())
}

pub fn simple_issue_asset<S: CliDataStore>(store: &mut S,
                                           asset_nick: String,
                                           amount: u64)
                                           -> Result<(), CliError> {
  let nick_tx = pic_random_txn_number();

  prepare_transaction(store, nick_tx.clone(), true)?;

  let seq_issue_number = query_asset_issuance_num(store, asset_nick.clone())?;

  issue_asset(store, Some(nick_tx), asset_nick, seq_issue_number, amount)?;

  Ok(())
}

pub fn list_public_key<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let pk = store.get_pubkey(&PubkeyName(nick.to_string()))?;
  let pk = pk.map(|x| serde_json::to_string(&x).unwrap())
             .unwrap_or(format!("No public key with name {} found", nick));
  println!("{}", pk);
  Ok(())
}

//////////////////// Advanced API  /////////////////////////////////////////////////////////////////

pub fn query_ledger_state<S: CliDataStore>(store: &mut S,
                                           forget_old_key: bool)
                                           -> Result<(), CliError> {
  store.update_config(|conf| {
         let mut new_key = forget_old_key;
         if !new_key && conf.ledger_sig_key.is_none() {
           println!("No signature key found for `{}`.", conf.ledger_server);
           new_key = new_key || prompt_default(" Retrieve a new one?", false).unwrap();
           if !new_key {
             eprintln!("Cannot check ledger state validity without a signature key.");
             exit(-1);
           }
         }

         if new_key {
           let query = format!("{}{}",
                               conf.ledger_server,
                               LedgerAccessRoutes::PublicKey.route());
           let resp: XfrPublicKey = do_request::<XfrPublicKey>(&query);

           println!("Saving ledger signing key `{}`",
                    serde_json::to_string(&resp).unwrap());
           conf.ledger_sig_key = Some(resp);
         }

         assert!(conf.ledger_sig_key.is_some());

         let query = format!("{}{}",
                             conf.ledger_server,
                             LedgerAccessRoutes::GlobalState.route());
         let resp: GlobalState = do_request::<GlobalState>(&query);

         if let Err(e) = resp.2
                             .verify(&conf.ledger_sig_key.unwrap(), &(resp.0.clone(), resp.1))
         {
           eprintln!("Ledger responded with invalid signature: {}", e);
           exit(-1);
         }

         conf.ledger_state = Some(LedgerStateCommitment(resp));

         assert!(conf.ledger_state.is_some());

         println!("New state retrieved.");

         print_conf(&conf);
       })?;
  Ok(())
}

fn query_asset_issuance_num<S: CliDataStore>(store: &mut S, nick: String) -> Result<u64, CliError> {
  // Fetch and store Asset issuance sequence number
  let asset: AssetTypeEntry;
  match store.get_asset_type(&AssetTypeName(nick.clone()))? {
    None => {
      eprintln!("No asset type with name `{}` found", nick);
      exit(-1);
    }
    Some(a) => {
      asset = a;
    }
  }

  let conf = store.get_config()?;
  let codeb64 = asset.asset.code.to_base64();
  let query = format!("{}{}",
                      conf.ledger_server,
                      LedgerAccessRoutes::AssetIssuanceNum.with_arg(&codeb64));

  let resp: u64 = do_request::<u64>(&query);

  Ok(resp)
}

pub fn list_txos<S: CliDataStore>(store: &mut S, unspent: bool) -> Result<(), CliError> {
  for (nick, txo) in store.get_cached_txos()?.into_iter() {
    if !txo.unspent && unspent {
      continue;
    }
    println!("TXO `{}`", nick.0);
    display_txo_entry(1, &txo);
  }
  println!("Done.");
  Ok(())
}

pub fn list_txo<S: CliDataStore>(store: &mut S, id: String) -> Result<(), CliError> {
  let txo = match store.get_cached_txo(&TxoName(id.clone()))? {
    None => {
      eprintln!("No txo `{}` found.", id);
      exit(-1);
    }
    Some(s) => s,
  };
  display_txo_entry(0, &txo);
  Ok(())
}

pub fn query_txo<S: CliDataStore>(store: &mut S, nick: String, sid: u64) -> Result<(), CliError> {
  let nick = TxoName(nick);
  if let Some(orig_ent) = store.get_cached_txo(&nick)? {
    if orig_ent.sid.is_some() && orig_ent.sid != Some(TxoSID(sid)) {
      eprintln!("TXO nicknamed `{}` refers to SID {}", nick.0, sid);
      exit(-1);
    }
  }
  let conf = store.get_config()?;
  let ledger_state = match conf.ledger_state.as_ref() {
    None => {
      eprintln!(concat!("I don't know what the ledger's state is!\n",
                        "Please run query-ledger-state first."));
      exit(-1);
    }
    Some(s) => s.clone(),
  };

  let query = format!("{}{}/{}",
                      conf.ledger_server,
                      LedgerAccessRoutes::UtxoSid.route(),
                      sid);

  let resp: AuthenticatedUtxo = do_request_authenticated_utxo(&query, sid, &ledger_state);

  // TODO: do something better to ensure that we pull any existing
  // things from orig_ent
  let mut ent = TxoCacheEntry { sid: Some(TxoSID(sid)),
                                owner: None,
                                asset_type: None,
                                ledger_state: Some(ledger_state),
                                record: resp.utxo.0,
                                owner_memo: None,
                                opened_record: None,
                                unspent: true };

  if let Some(orig_ent) = store.get_cached_txo(&nick)? {
    if orig_ent.sid.is_some() {
      assert_eq!(orig_ent.sid, ent.sid);
    }
    assert!(orig_ent.unspent);
    assert_eq!(ent.record, orig_ent.record);
    ent.owner_memo = orig_ent.owner_memo;
    ent.opened_record = orig_ent.opened_record;
    if let Some(orig_state) = orig_ent.ledger_state {
      assert!((orig_state.0).1 <= (ent.ledger_state.as_ref().unwrap().0).1);
    }
    ent.owner = orig_ent.owner;
    ent.asset_type = orig_ent.asset_type;
  }

  if ent.owner.is_none() {
    for (n, pk) in store.get_pubkeys()?.into_iter() {
      if pk == ent.record.0.public_key {
        ent.owner = Some(n);
        break;
      }
    }
  }

  if ent.asset_type.is_none() {
    if let Some(tp) = ent.record.0.asset_type.get_asset_type() {
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
    println!("Asset `{}`", nick.0);
    display_asset_type(1, &a);
  }
  Ok(())
}

pub fn list_asset_type<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let a = store.get_asset_type(&AssetTypeName(nick.clone()))?;
  match a {
    None => {
      eprintln!("`{}` does not refer to any known asset type", nick);
      exit(-1);
    }
    Some(a) => {
      display_asset_type(0, &a);
    }
  }
  Ok(())
}

pub fn query_asset_type<S: CliDataStore>(store: &mut S,
                                         replace: bool,
                                         nick: String,
                                         code: String)
                                         -> Result<(), CliError> {
  if !replace
     && store.get_asset_type(&AssetTypeName(nick.clone()))?
             .is_some()
  {
    eprintln!("Asset type with the nickname `{}` already exists.", nick);
    exit(-1);
  }

  let conf = store.get_config()?;
  let code_b64 = code.clone();
  let _ = AssetTypeCode::new_from_base64(&code).unwrap();
  let query = format!("{}{}/{}",
                      conf.ledger_server,
                      LedgerAccessRoutes::AssetToken.route(),
                      code_b64);
  let resp = do_request_asset(&query);

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

  let issue_seq_number = query_asset_issuance_num(store, nick.clone())?;
  println!("issue_seq_number: {}", issue_seq_number);

  let ret = AssetTypeEntry { asset: resp,
                             issuer_nick,
                             issue_seq_num: issue_seq_number };
  store.add_asset_type(&AssetTypeName(nick.clone()), ret)?;
  println!("Asset type `{}` saved as `{}`", code_b64, nick);
  Ok(())
}

pub fn prepare_transaction<S: CliDataStore>(store: &mut S,
                                            nick: String,
                                            exact: bool)
                                            -> Result<(), CliError> {
  let seq_id = match store.get_config()?.ledger_state {
    None => {
      eprintln!(concat!("I don't know what block ID the ledger is on!\n",
                        "Please run query-ledger-state first."));
      exit(-1);
    }
    Some(s) => (s.0).1,
  };

  let mut nick = nick;
  if store.get_txn_builder(&TxnBuilderName(nick.clone()))?
          .is_some()
  {
    if exact {
      eprintln!("Transaction builder with the name `{}` already exists.",
                nick);
      exit(-1);
    }

    for n in FreshNamer::new(nick.clone(), ".".to_string()) {
      if store.get_txn_builder(&TxnBuilderName(n.clone()))?.is_none() {
        nick = n;
        break;
      }
    }
  }

  println!("Preparing transaction `{}` for block id `{}`...",
           nick, seq_id);
  store.prepare_transaction(&TxnBuilderName(nick.clone()), seq_id)?;
  store.update_config(|conf| {
         conf.active_txn = Some(TxnBuilderName(nick));
       })?;
  println!("Done.");
  Ok(())
}

pub fn list_txn_builders<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
  for (nick, builder) in store.get_txn_builders()? {
    println!("{}:", nick.0);
    display_txn_builder(1, &builder);
  }
  println!("Done.");
  Ok(())
}

pub fn list_txn_builder<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let builder = match store.get_txn_builder(&TxnBuilderName(nick.clone()))? {
    None => {
      eprintln!("No txn builder `{}` found.", nick);
      exit(-1);
    }
    Some(s) => s,
  };

  display_txn_builder(0, &builder);
  Ok(())
}

pub fn list_built_transaction<S: CliDataStore>(store: &mut S,
                                               nick: String)
                                               -> Result<(), CliError> {
  let txn = match store.get_built_transaction(&TxnName(nick.clone()))? {
    None => {
      eprintln!("No txn `{}` found.", nick);
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
      eprintln!("No txn `{}` found.", txn);
      exit(-1);
    }
    Some(s) => s,
  };
  println!("handle {}: {}",
           serialize_or_str(&txn.1.handle, "<UNKNOWN>"),
           serialize_or_str(&txn.1.status, "<UNKNOWN>"));
  Ok(())
}

pub fn status_check<S: CliDataStore>(store: &mut S, txn: String) -> Result<(), CliError> {
  let conf = store.get_config()?;
  let txn_nick = txn.clone();
  let txn = match store.get_built_transaction(&TxnName(txn.clone()))? {
    None => {
      eprintln!("No txn `{}` found.", txn);
      exit(-1);
    }
    Some(s) => s,
  };

  let handle;
  match txn.1.handle.as_ref() {
    None => {
      eprintln!("No handle for txn `{}` found. Have you submitted it?",
                txn_nick);
      exit(-1);
    }
    Some(h) => {
      handle = h;
    }
  }

  let query = format!("{}{}/{}",
                      conf.submission_server,
                      SubmissionRoutes::TxnStatus.route(),
                      handle.0);
  let resp = do_request::<TxnStatus>(&query);

  println!("Got status: {}", serde_json::to_string(&resp)?);
  // TODO: do something if it's committed
  store.update_txn_metadata::<std::convert::Infallible, _>(&TxnName(txn_nick), |metadata| {
         metadata.status = Some(resp);
         Ok(())
       })?;
  Ok(())
}

pub fn define_asset<S: CliDataStore>(store: &mut S,
                                     builder: Option<String>,
                                     issuer_nick: String,
                                     asset_nick: String)
                                     -> Result<(), CliError> {
  let issuer_nick = KeypairName(issuer_nick);
  let builder_opt = builder.map(TxnBuilderName)
                           .or_else(|| store.get_config().unwrap().active_txn);
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
      eprintln!("Transaction builder `{}` not found.", builder_name.0);
      exit(-1);
    }
    Some(b) => {
      new_builder = b;
    }
  }
  store.with_keypair::<ledger::data_model::errors::PlatformError, _>(&issuer_nick, |kp| match kp {
    None => {
      eprintln!("No key pair `{}` found.", issuer_nick.0);
      exit(-1);
    }
    Some(kp) => {
      new_builder.builder.add_operation_create_asset(&kp,
                                                      None,
                                                      Default::default(),
                                                      &prompt::<String, _>("memo?").unwrap(),
                                                      PolicyChoice::Fungible())?;
      Ok(())
    }
  })?;

  store.with_txn_builder::<ledger::data_model::errors::PlatformError, _>(&builder_name, |builder| {
    *builder = new_builder;
    match builder.builder.transaction().body.operations.last() {
      Some(Operation::DefineAsset(def)) => {
        builder.new_asset_types
               .insert(AssetTypeName(asset_nick.clone()),
                       AssetTypeEntry { asset: def.body.asset.clone(),
                                        issuer_nick: Some(PubkeyName(issuer_nick.0.clone())),
                                        issue_seq_num: 0_u64 });
      }
      _ => {
        panic!("The transaction builder doesn't include our operation!");
      }
    }
    if !builder.signers.contains(&issuer_nick) {
      builder.signers.push(issuer_nick.clone());
    }
    builder.operations
           .push(OpMetadata::DefineAsset { issuer_nick: PubkeyName(issuer_nick.0.clone()),
                                           asset_nick: AssetTypeName(asset_nick.clone()) });
    println!("{}:", asset_nick);
    display_asset_type(1,
                       builder.new_asset_types
                              .get(&AssetTypeName(asset_nick.clone()))
                              .unwrap());
    Ok(())
  })?;
  Ok(())
}

pub fn issue_asset<S: CliDataStore>(store: &mut S,
                                    builder: Option<String>,
                                    asset_nick: String,
                                    issue_seq_num: u64,
                                    amount: u64)
                                    -> Result<(), CliError> {
  let builder_opt = builder.map(TxnBuilderName)
                           .or_else(|| store.get_config().unwrap().active_txn);
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
      eprintln!("Transaction builder `{}` not found.", builder_nick.0);
      exit(-1);
    }
    Some(b) => {
      builder = b;
    }
  }

  let asset_nick = AssetTypeName(asset_nick);
  let asset;
  match store.get_asset_type(&asset_nick)?
             .or_else(|| builder.new_asset_types.get(&asset_nick).cloned())
  {
    None => {
      eprintln!("No asset type with name `{}` found", asset_nick.0);
      exit(-1);
    }
    Some(a) => {
      asset = a;
    }
  }

  let issuer_nick;
  match asset.issuer_nick.as_ref() {
    None => {
      eprintln!("I don't know an identity for public key `{}`",
                serde_json::to_string(&asset.asset.issuer.key).unwrap());
      exit(-1);
    }
    Some(nick) => {
      issuer_nick = KeypairName(nick.0.clone());
    }
  }

  store.with_keypair::<errors::PlatformError, _>(&issuer_nick, |iss_kp| match iss_kp {
         None => {
           eprintln!("No keypair nicknamed `{}` found.", issuer_nick.0);
           exit(-1);
         }
         Some(iss_kp) => {
           println!("IssueAsset: {} of `{}` ({}), authorized by `{}`",
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
               builder.new_txos
            .push((out_name.clone(),
              TxoCacheEntry {
                  sid: None, asset_type: Some(asset_nick.clone()), record: txo.clone(),
                  owner_memo: memo.clone(), ledger_state: None,
                  owner: Some(PubkeyName(issuer_nick.0.clone())),
                  opened_record:
                    Some(open_blind_asset_record(&txo.0,
                          &memo, iss_kp.get_sk_ref()).unwrap()),
                  unspent: true }));
             }
             _ => {
               panic!("The transaction builder doesn't include our operation!");
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

  store.with_txn_builder::<errors::PlatformError, _>(&builder_nick, |the_builder| {
         *the_builder = builder;
         Ok(())
       })?;

  println!("Successfully added to `{}`", builder_nick.0);

  Ok(())
}

pub fn build_transaction<S: CliDataStore>(store: &mut S,
                                          builder: Option<String>,
                                          txn_nick: Option<String>,
                                          exact: bool)
                                          -> Result<(), CliError> {
  let mut used_default = false;
  let builder_opt = builder.map(TxnBuilderName).or_else(|| {
                                                 used_default = true;
                                                 store.get_config().unwrap().active_txn
                                               });
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

  let mut txn_nick = TxnName(txn_nick.unwrap_or_else(|| nick.0.clone()));

  if store.get_built_transaction(&txn_nick)?.is_some() {
    if !exact {
      for n in FreshNamer::new(txn_nick.0.clone(), ".".to_string()) {
        if store.get_built_transaction(&TxnName(n.clone()))?.is_none() {
          txn_nick = TxnName(n);
          break;
        }
      }
    } else {
      eprintln!("A txn with nickname `{}` already exists", txn_nick.0);
      exit(-1);
    }
  }
  println!("Building `{}` as `{}`...", nick.0, txn_nick.0);

  let mut metadata: TxnMetadata = Default::default();
  let builder = match store.get_txn_builder(&nick)? {
    None => {
      eprintln!("No txn builder `{}` found.", nick.0);
      exit(-1);
    }
    Some(b) => b,
  };

  let sigs = {
    let mut sigs = vec![];
    for kp_name in builder.signers.iter() {
      store.with_keypair::<std::convert::Infallible, _>(&kp_name, |kp| match kp {
             None => {
               eprintln!("Could not find keypair for required signer `{}`.",
                         kp_name.0);
               exit(-1);
             }
             Some(kp) => {
               sigs.push((*kp.get_pk_ref(),
                          SignatureOf::new(kp, &builder.builder.transaction().body)));
               Ok(())
             }
           })?;
    }
    sigs
  };

  store.with_txn_builder::<errors::PlatformError, _>(&nick, |builder| {
         for (pk, sig) in sigs {
           builder.builder.add_signature(&pk, sig)?;
         }

         std::mem::swap(&mut metadata.new_asset_types, &mut builder.new_asset_types);
         std::mem::swap(&mut metadata.new_txos, &mut builder.new_txos);
         let mut signers = Default::default();
         std::mem::swap(&mut signers, &mut builder.signers);
         metadata.signers.extend(signers.into_iter());
         std::mem::swap(&mut metadata.operations, &mut builder.operations);
         Ok(())
       })?;

  store.build_transaction(&nick, &txn_nick, metadata)?;
  if used_default {
    store.update_config(|conf| {
           conf.active_txn = None;
         })?;
  }

  println!("Built transaction `{}` from builder `{}`.",
           txn_nick.0, nick.0);
  Ok(())
}

pub fn submit<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let (txn, metadata) = store.get_built_transaction(&TxnName(nick.clone()))?
                             .unwrap_or_else(|| {
                               eprintln!("No transaction `{}` found.", nick);
                               exit(-1);
                             });
  if let Some(h) = metadata.handle {
    eprintln!("Transaction `{}` has already been submitted. Its handle is: `{}`",
              nick, h.0);
    exit(-1);
  }

  let conf = store.get_config()?;
  let query = format!("{}{}",
                      conf.submission_server,
                      SubmissionRoutes::SubmitTransaction.route());

  println!("Submitting to `{}`:", query);
  display_txn(1, &(txn.clone(), metadata.clone()));

  if !prompt_default("Is this correct?", true)? {
    println!("Exiting.");
    return Ok(());
  }

  let client = reqwest::blocking::Client::builder().build()?;
  let resp = client.post(&query)
                   .json(&txn)
                   .send()?
                   .error_for_status()?
                   .text()?;
  let handle = serde_json::from_str::<TxnHandle>(&resp)?;

  for (nick, ent) in metadata.new_asset_types.iter() {
    store.add_asset_type(&nick, ent.clone())?;
  }
  store.update_txn_metadata::<std::convert::Infallible, _>(&TxnName(nick.clone()), |metadata| {
         metadata.handle = Some(handle.clone());
         Ok(())
       })?;
  println!("Submitted `{}`: got handle `{}`", nick, &handle.0);

  if prompt_default("Retrieve its status?", true)? {
    let query = format!("{}{}/{}",
                        conf.submission_server,
                        SubmissionRoutes::TxnStatus.route(),
                        &handle.0);
    let resp = do_request::<TxnStatus>(&query);

    println!("Got status: {}", serde_json::to_string(&resp)?);
    // TODO: do something if it's committed
    store.update_txn_metadata::<std::convert::Infallible, _>(&TxnName(nick.clone()), |metadata| {
           metadata.status = Some(resp.clone());
           Ok(())
         })?;

    if let TxnStatus::Committed((_, txo_sids)) = resp {
      if let Some(TxnStatus::Committed(_)) = metadata.status {
      } else {
        // TODO: internally-spent TXOs
        assert_eq!(metadata.new_txos.len(), txo_sids.len());
        for pref in FreshNamer::new(nick.clone(), ".".to_string()) {
          let mut entries = vec![];
          let mut bad = false;
          for ((n, txo), sid) in metadata.new_txos.iter().zip(txo_sids.iter()) {
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
              println!("Caching TXO `{}`:", k.0);
              display_txo_entry(1, &v);
              store.cache_txo(&k, v)?;
              finalized.push(k);
            }

            store.update_txn_metadata::<std::convert::Infallible, _>(&TxnName(nick), |metadata| {
                   assert!(metadata.finalized_txos.is_none());
                   metadata.finalized_txos = Some(finalized);
                   Ok(())
                 })?;
            break;
          }
        }
        println!("Done caching TXOs.");
      }
    }
  }
  Ok(())
}
