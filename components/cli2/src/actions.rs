use crate::{
  display_asset_type, display_txn, display_txn_builder, print_conf, prompt_for_config,
  serialize_or_str, AssetTypeEntry, AssetTypeName, CliDataStore, CliError, FreshNamer, KeypairName,
  LedgerStateCommitment, OpMetadata, PubkeyName, TxnBuilderName, TxnMetadata, TxnName,
  TxoCacheEntry,
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
use utils::Serialized;
use utils::{HashOf, SignatureOf};
use zei::setup::PublicParams;
use zei::xfr::asset_record::{open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};

//////////////////// Simple API  ///////////////////////////////////////////////////////////////

pub fn setup<S: CliDataStore>(store: &mut S) -> Result<(), CliError> {
  store.update_config(|conf| {
         *conf = prompt_for_config(Some(conf.clone())).unwrap();
       })?;
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
                 .filter(|(k, _)| !kps.contains_key(&KeypairName(k.clone())))
                 .map(|x| (x, false))
                 .collect::<Vec<_>>();
  let kps = kps.into_iter()
               .map(|(k, kp)| (k.0, *kp.get_pk_ref()))
               .map(|x| (x, true));
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
  let kp = store.get_keypair(&KeypairName(nick.to_string()))?;
  if show_secret {
    let kp = kp.map(|x| serde_json::to_string(&x).unwrap())
               .unwrap_or(format!("No keypair with name `{}` found", nick));
    println!("{}", kp);
  } else {
    let pk = kp.map(|x| serde_json::to_string(x.get_pk_ref()).unwrap())
               .unwrap_or(format!("No keypair with name `{}` found", nick));
    println!("{}", pk);
  }
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
  let kp = store.get_keypair(&KeypairName(nick.to_string()))?;
  match kp {
    None => {
      eprintln!("No keypair with name `{}` found", nick);
      exit(-1);
    }
    Some(_) => {
      if prompt_default(format!("Are you sure you want to delete keypair `{}`?", nick),
                        false)?
      {
        // TODO: do this atomically?
        store.delete_keypair(&KeypairName(nick.to_string()))?;
        store.delete_pubkey(&PubkeyName(nick.to_string()))?;
        println!("Keypair `{}` deleted", nick);
      }
    }
  }
  Ok(())
}

pub fn delete_public_key<S: CliDataStore>(store: &mut S, nick: String) -> Result<(), CliError> {
  let pk = store.get_pubkey(&PubkeyName(nick.to_string()))?;
  let kp = store.get_keypair(&KeypairName(nick.to_string()))?;
  match (pk, kp) {
    (None, _) => {
      eprintln!("No public key with name `{}` found", nick);
      exit(-1);
    }
    (Some(_), Some(_)) => {
      eprintln!("`{}` is a keypair. Please use delete-keypair instead.",
                nick);
      exit(-1);
    }
    (Some(_), None) => {
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
           let resp: XfrPublicKey;
           match reqwest::blocking::get(&query) {
             Err(e) => {
               eprintln!("Request `{}` failed: {}", query, e);
               exit(-1);
             }
             Ok(v) => match v.text()
                             .map(|x| serde_json::from_str::<XfrPublicKey>(&x).map_err(|e| (x, e)))
             {
               Err(e) => {
                 eprintln!("Failed to decode response: {}", e);
                 exit(-1);
               }
               Ok(Err((x, e))) => {
                 eprintln!("Failed to parse response `{}`: {}", x, e);
                 exit(-1);
               }
               Ok(Ok(v)) => {
                 resp = v;
               }
             },
           }

           println!("Saving ledger signing key `{}`",
                    serde_json::to_string(&resp).unwrap());
           conf.ledger_sig_key = Some(resp);
         }

         assert!(conf.ledger_sig_key.is_some());

         let query = format!("{}{}",
                             conf.ledger_server,
                             LedgerAccessRoutes::GlobalState.route());
         let resp: (HashOf<Option<StateCommitmentData>>,
                    u64,
                    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>);
         match reqwest::blocking::get(&query) {
           Err(e) => {
             eprintln!("Request `{}` failed: {}", query, e);
             exit(-1);
           }
           Ok(v) => match v.text()
                           .map(|x| serde_json::from_str::<_>(&x).map_err(|e| (x, e)))
           {
             Err(e) => {
               eprintln!("Failed to decode response: {}", e);
               exit(-1);
             }
             Ok(Err((x, e))) => {
               eprintln!("Failed to parse response `{}`: {}", x, e);
               exit(-1);
             }
             Ok(Ok(v)) => {
               resp = v;
             }
           },
         }

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
  let resp: Asset;
  match reqwest::blocking::get(&query) {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(v) => match v.text()
                    .map(|x| serde_json::from_str::<AssetType>(&x).map_err(|e| (x, e)))
    {
      Err(e) => {
        eprintln!("Failed to decode response: {}", e);
        exit(-1);
      }
      Ok(Err((x, e))) => {
        eprintln!("Failed to parse response `{}`: {}", x, e);
        exit(-1);
      }
      Ok(Ok(v)) => {
        resp = v.properties;
      }
    },
  }

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
  let ret = AssetTypeEntry { asset: resp,
                             issuer_nick };
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
  let resp;
  match reqwest::blocking::get(&query) {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(v) => match v.text()
                    .map(|x| serde_json::from_str::<TxnStatus>(&x).map_err(|e| (x, e)))
    {
      Err(e) => {
        eprintln!("Failed to decode `{}` response: {}", query, e);
        exit(-1);
      }
      Ok(Err((x, e))) => {
        eprintln!("Failed to parse `{}` response to `{}`: {}", query, x, e);
        exit(-1);
      }
      Ok(Ok(v)) => {
        resp = v;
      }
    },
  }

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
  let kp = match store.get_keypair(&issuer_nick)? {
    None => {
      eprintln!("No key pair `{}` found.", issuer_nick.0);
      exit(-1);
    }
    Some(s) => s,
  };
  let builder_opt = builder.map(TxnBuilderName)
                           .or_else(|| store.get_config().unwrap().active_txn);
  let builder;
  match builder_opt {
    None => {
      eprintln!("I don't know which transaction to use!");
      exit(-1);
    }
    Some(t) => {
      builder = t;
    }
  }

  if store.get_txn_builder(&builder)?.is_none() {
    eprintln!("Transaction builder `{}` not found.", builder.0);
    exit(-1);
  }

  store.with_txn_builder::<ledger::data_model::errors::PlatformError, _>(&builder, |builder| {
         builder.builder.add_operation_create_asset(&kp,
                                                     None,
                                                     Default::default(),
                                                     &prompt::<String, _>("memo?").unwrap(),
                                                     PolicyChoice::Fungible())?;
         match builder.builder.transaction().body.operations.last() {
           Some(Operation::DefineAsset(def)) => {
             builder.new_asset_types
                    .insert(AssetTypeName(asset_nick.clone()),
                            AssetTypeEntry { asset: def.body.asset.clone(),
                                             issuer_nick: Some(PubkeyName(issuer_nick.0
                                                                                     .clone())) });
           }
           _ => {
             panic!("The transaction builder doesn't include our operation!");
           }
         }
         builder.signers
                .insert(issuer_nick.clone(), Serialized::new(&kp));
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
  let builder;
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

  let iss_kp;
  match store.get_keypair(&issuer_nick)? {
    None => {
      eprintln!("No keypair nicknamed `{}` found.", issuer_nick.0);
      exit(-1);
    }
    Some(kp) => {
      iss_kp = kp;
    }
  }

  println!("IssueAsset: {} of `{}` ({}), authorized by `{}`",
           amount,
           asset.asset.code.to_base64(),
           asset_nick.0,
           issuer_nick.0);

  store.with_txn_builder::<errors::PlatformError, _>(&builder_nick, |builder| {
         builder.builder.add_basic_issue_asset(
      &iss_kp, &asset.asset.code, issue_seq_num, amount,
      AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
      &PublicParams::new())?;

         let out_name = format!("utxo{}", builder.new_txos.len());

         match builder.builder.transaction().body.operations.last() {
           Some(Operation::IssueAsset(iss)) => {
             assert_eq!(iss.body.records.len(), 1);
             let (txo, memo) = iss.body.records[0].clone();
             builder.new_txos
               .push((out_name.clone(),
                      TxoCacheEntry { sid: None,
                                      record: txo.clone(),
                                      owner_memo: memo.clone(),
                        ledger_state: None,
                        owner: Some(PubkeyName(issuer_nick.0.clone())),
                                      opened_record:
                                        Some(open_blind_asset_record(&txo.0,
                                                                     &memo,
                                                                     iss_kp.get_sk_ref()).unwrap()),
                                      unspent: true }));
           }
           _ => {
             panic!("The transaction builder doesn't include our operation!");
           }
         }
         builder.signers
                .insert(issuer_nick.clone(), Serialized::new(&iss_kp));
         builder.operations
                .push(OpMetadata::IssueAsset { issuer_nick: PubkeyName(issuer_nick.0.clone()),
                                               asset_nick: asset_nick.clone(),
                                               output_name: out_name,
                                               output_amt: amount,
                                               issue_seq_num });
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
  store.with_txn_builder(&nick, |builder| {
         for (_, kp) in builder.signers.iter() {
           builder.builder.sign(&kp.deserialize());
         }
         std::mem::swap(&mut metadata.new_asset_types, &mut builder.new_asset_types);
         std::mem::swap(&mut metadata.new_txos, &mut builder.new_txos);
         let mut signers = Default::default();
         std::mem::swap(&mut signers, &mut builder.signers);
         metadata.signers.extend(signers.into_iter().map(|(k, _)| k));
         std::mem::swap(&mut metadata.operations, &mut builder.operations);
         let ret: Result<(), std::convert::Infallible> = Ok(());
         ret
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
    let resp;
    match reqwest::blocking::get(&query) {
      Err(e) => {
        eprintln!("Request `{}` failed: {}", query, e);
        exit(-1);
      }
      Ok(v) => match v.text()
                      .map(|x| serde_json::from_str::<TxnStatus>(&x).map_err(|e| (x, e)))
      {
        Err(e) => {
          eprintln!("Failed to decode response: {}", e);
          exit(-1);
        }
        Ok(Err((x, e))) => {
          eprintln!("Failed to parse response `{}`: {}", x, e);
          exit(-1);
        }
        Ok(Ok(v)) => {
          resp = v;
        }
      },
    }

    println!("Got status: {}", serde_json::to_string(&resp)?);
    // TODO: do something if it's committed
    store.update_txn_metadata::<std::convert::Infallible, _>(&TxnName(nick), |metadata| {
           metadata.status = Some(resp);
           Ok(())
         })?;
  }
  Ok(())
}
