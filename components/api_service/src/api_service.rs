extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate serde_json;

use actix_web::{web, App, HttpServer};
use ledger::data_model::{
  AssetPolicyKey, AssetType, AssetTypeCode, CustomAssetPolicy, SmartContract, SmartContractKey,
  TxnSID, TxoSID, Utxo,
};
use ledger::store::{ArchiveAccess, LedgerAccess, LedgerUpdate, TxnEffect};
use percent_encoding::percent_decode_str;
use rand::{CryptoRng, Rng};
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};

pub struct RestfulApiService {
  web_runtime: actix_rt::SystemRunner,
}

fn query_utxo<LA>(data: web::Data<Arc<RwLock<LA>>>,
                  info: web::Path<String>)
                  -> actix_web::Result<web::Json<Utxo>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(txo_sid) = info.parse::<u64>() {
    if let Some(txo) = reader.get_utxo(TxoSID(txo_sid)) {
      Ok(web::Json(txo.clone()))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified txo does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid txo sid encoding"))
  }
}

fn query_asset<LA>(data: web::Data<Arc<RwLock<LA>>>,
                   info: web::Path<String>)
                   -> actix_web::Result<web::Json<AssetType>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
    if let Some(asset) = reader.get_asset_type(&token_code) {
      Ok(web::Json(asset.clone()))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified asset definition does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid asset definition encoding."))
  }
}

fn query_txn<AA>(data: web::Data<Arc<RwLock<AA>>>,
                 info: web::Path<String>)
                 -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  let reader = data.read().unwrap();
  if let Ok(txn_sid) = info.parse::<usize>() {
    if let Some(txn) = reader.get_transaction(TxnSID(txn_sid)) {
      Ok(serde_json::to_string(&*txn)?)
    } else {
      Err(actix_web::error::ErrorNotFound("Specified transaction does not exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid txn sid encoding."))
  }
}

fn stringer(data: &[u8; 32]) -> String {
  let mut result = "".to_string();

  for i in 0..data.len() {
    result = result + &format!("{:02x}", data[i]);
  }

  result
}

fn query_global_state<AA>(data: web::Data<Arc<RwLock<AA>>>,
                          _info: web::Path<String>)
                          -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  let reader = data.read().unwrap();
  let (hash, version) = reader.get_global_hash();
  let result = format!("{} {}", stringer(&hash.0), version);
  Ok(result)
}

fn query_proof<AA>(data: web::Data<Arc<RwLock<AA>>>,
                   info: web::Path<String>)
                   -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  if let Ok(txn_sid) = info.parse::<usize>() {
    let reader = data.read().unwrap();
    if let Some(proof) = reader.get_proof(TxnSID(txn_sid)) {
      Ok(serde_json::to_string(&proof)?)
    } else {
      Err(actix_web::error::ErrorNotFound("That transaction doesn't exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid txn sid encoding."))
  }
}

fn query_utxo_map_checksum<AA>(data: web::Data<Arc<RwLock<AA>>>,
                               info: web::Path<String>)
                               -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  if let Ok(version) = info.parse::<u64>() {
    let reader = data.read().unwrap();

    if let Some(vec) = reader.get_utxo_checksum(version) {
      Ok(serde_json::to_string(&vec)?)
    } else {
      Err(actix_web::error::ErrorNotFound("That version is unavailable."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid version encoding."))
  }
}

fn query_utxo_partial_map<AA>(data: web::Data<Arc<RwLock<AA>>>,
                              info: web::Path<String>)
                              -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorNotFound("unimplemented"))
  // if let Some(block_list) = parse_blocks(info.to_string()) {
  //   let mut reader = data.write().unwrap();

  //   if let Some(vec) = reader.get_utxos(block_list) {
  //     Ok(serde_json::to_string(&vec)?)
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("The map is unavailable."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorNotFound("Invalid block list encoding."))
  // }
}

fn parse_blocks(block_input: String) -> Option<Vec<usize>> {
  let blocks = block_input.split(',');
  let mut result = Vec::new();

  for block_str in blocks {
    if let Ok(block_usize) = block_str.parse::<usize>() {
      result.push(block_usize);
    } else {
      return None;
    }
  }

  Some(result)
}

fn query_utxo_map<AA>(data: web::Data<Arc<RwLock<AA>>>,
                      _info: web::Path<String>)
                      -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  let mut reader = data.write().unwrap();

  let vec = reader.serialize_utxo_map();
  Ok(serde_json::to_string(&vec)?)
}

fn query_policy<LA>(data: web::Data<Arc<RwLock<LA>>>,
                    info: web::Path<String>)
                    -> actix_web::Result<web::Json<CustomAssetPolicy>>
  where LA: LedgerAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorNotFound("unimplemented"))
  // let reader = data.read().unwrap();
  // if let Ok(asset_policy_key) = AssetPolicyKey::new_from_base64(&*info) {
  //   if let Some(policy) = reader.get_asset_policy(&asset_policy_key) {
  //     Ok(web::Json(policy))
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("Specified asset policy does not currently exist."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorNotFound("Invalid asset policy encoding."))
  // }
}

fn submit_transaction<RNG, U>(data: web::Data<Arc<RwLock<U>>>,
                              info: web::Path<String>)
                              -> Result<String, actix_web::error::Error>
  where RNG: Rng + CryptoRng,
        U: LedgerUpdate<RNG>
{
  // TODO: Handle submission to Tendermint layer
  let mut ledger = data.write().unwrap();
  let uri_string = percent_decode_str(&*info).decode_utf8().unwrap();
  let tx = serde_json::from_str(&uri_string).map_err(|e| actix_web::error::ErrorBadRequest(e))?;

  let txn_effect =
    TxnEffect::compute_effect(ledger.get_prng(), tx).map_err(|e| {
                                                      actix_web::error::ErrorBadRequest(e)
                                                    })?;

  let ret = ledger.apply_transaction(txn_effect)
                  .map_err(|e| actix_web::error::ErrorBadRequest(e))?;

  Ok(serde_json::to_string(&ret)?)
}

fn query_contract<LA>(data: web::Data<Arc<RwLock<LA>>>,
                      info: web::Path<String>)
                      -> actix_web::Result<web::Json<SmartContract>>
  where LA: LedgerAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorNotFound("unimplemented"))

  // let reader = data.read().unwrap();
  // if let Ok(smart_contract_key) = SmartContractKey::new_from_base64(&*info) {
  //   if let Some(contract) = reader.get_smart_contract(&smart_contract_key) {
  //     Ok(web::Json(contract))
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("Specified smart contract does not currently exist."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorNotFound("Invalid smart contract encoding."))
  // }
}

impl RestfulApiService {
  pub fn create<RNG, LA>(ledger_access: Arc<RwLock<LA>>,
                         host: &str,
                         port: &str)
                         -> io::Result<RestfulApiService>
    where RNG: 'static + Rng + CryptoRng,
          LA: 'static + LedgerAccess + ArchiveAccess + LedgerUpdate<RNG> + Sync + Send
  {
    let web_runtime = actix_rt::System::new("eian API");
    let data = ledger_access.clone();
    let addr = format!("{}:{}", host, port);
    HttpServer::new(move || {
      App::new().data(data.clone())
                .route("/utxo_sid/{sid}", web::get().to(query_utxo::<LA>))
                .route("/asset_token/{token}", web::get().to(query_asset::<LA>))
                .route("/txn_sid/{sid}", web::get().to(query_txn::<LA>))
                .route("/proof/{sid}", web::get().to(query_proof::<LA>))
                .route("/utxo_map", web::get().to(query_utxo_map::<LA>))
                .route("/global_state", web::get().to(query_global_state::<LA>))
                .route("/utxo_map_checksum",
                       web::get().to(query_utxo_map_checksum::<LA>))
                .route("/utxo_partial_map/{sidlist}",
                       web::get().to(query_utxo_partial_map::<LA>))
                .route("/policy_key/{key}", web::get().to(query_policy::<LA>))
                .route("/contract_key/{key}", web::get().to(query_contract::<LA>))
                .route("/submit_transaction/{tx}",
                       web::post().to(submit_transaction::<RNG, LA>))
    }).bind(&addr)?
      .start();
    Ok(RestfulApiService { web_runtime })
  }
  // call from a thread; this will block.
  pub fn run(self) -> io::Result<()> {
    self.web_runtime.run()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use actix_web::dev::Service;
  use actix_web::{test, web, App};
  use ledger::data_model::{Operation, Transaction};
  use ledger::store::helpers::*;
  use ledger::store::{LedgerState, LedgerUpdate};
  use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
  use rand::SeedableRng;
  use rand_chacha::ChaChaRng;

  #[test]
  fn test_query_utxo() {}

  #[test]
  fn test_query_txn() {}

  #[test]
  fn test_query_policy() {}

  #[test]
  fn test_query_proof() {}

  #[test]
  fn test_query_contract() {}

  #[test]
  fn test_query_asset() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let effect = TxnEffect::compute_effect(state.get_prng(), tx).unwrap();
    state.apply_transaction(effect).unwrap();

    let mut app = test::init_service(App::new().data(Arc::new(RwLock::new(state)))
                                               .route("/asset_token/{token}",
                                                      web::get().to(query_asset::<LedgerState>)));

    let req = test::TestRequest::get().uri(&format!("/asset_token/{}", token_code1.to_base64()))
                                      .to_request();
    let resp = test::block_on(app.call(req)).unwrap();

    assert!(resp.status().is_success());
  }
  #[test]
  fn test_query_transaction_and_query() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let state = LedgerState::test_ledger();
    let mut tx = Transaction::default();

    let token_code1 = AssetTypeCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::DefineAsset(asset_create));

    let mut app =
      test::init_service(App::new().data(Arc::new(RwLock::new(state)))
                                   .route("/submit_transaction/{tx}",
                                          web::post().to(submit_transaction::<ChaChaRng,
                                                                            LedgerState>))
                                   .route("/asset_token/{token}",
                                          web::get().to(query_asset::<LedgerState>)));

    let serialize = serde_json::to_string(&tx).unwrap();
    // Set of invalid URI characters that may appear in a JSON transaction
    // TODO: (Noah) make sure set is complete
    const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ')
                                         .add(b'"')
                                         .add(b'`')
                                         .add(b'{')
                                         .add(b'/')
                                         .add(b'}');
    let uri_string = utf8_percent_encode(&serialize, FRAGMENT).to_string();

    let submit_req = test::TestRequest::post().uri(&format!("/submit_transaction/{}", uri_string))
                                              .to_request();

    let query_req = test::TestRequest::get().uri(&format!("/asset_token/{}",
                                                          token_code1.to_base64()))
                                            .to_request();

    test::block_on(app.call(submit_req)).unwrap();
    let query_resp = test::block_on(app.call(query_req)).unwrap();

    assert!(query_resp.status().is_success());
  }
}
