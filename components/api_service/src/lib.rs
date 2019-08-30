extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate serde_json;

use actix_web::{web, App, HttpServer};
use ledger::data_model::{
  AssetPolicyKey, AssetToken, AssetTokenCode, CustomAssetPolicy, SmartContract, SmartContractKey,
  TxnSID, TxoSID, Utxo,
};
use ledger::store::{ArchiveAccess, LedgerAccess};
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
    if let Some(txo) = reader.check_utxo(TxoSID { index: txo_sid }) {
      Ok(web::Json(txo))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified txo does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid txo sid encoding"))
  }
}

fn query_asset<LA>(data: web::Data<Arc<RwLock<LA>>>,
                   info: web::Path<String>)
                   -> actix_web::Result<web::Json<AssetToken>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(token_code) = AssetTokenCode::new_from_base64(&*info) {
    if let Some(asset) = reader.get_asset_token(&token_code) {
      Ok(web::Json(asset))
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
    if let Some(txn) = reader.get_transaction(TxnSID { index: txn_sid }) {
      Ok(serde_json::to_string(&*txn)?)
    } else {
      Err(actix_web::error::ErrorNotFound("Specified transaction does not exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid txn sid encoding."))
  }
}

fn query_proof<AA>(data: web::Data<Arc<RwLock<AA>>>,
                   info: web::Path<String>)
                   -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  if let Ok(txn_sid) = info.parse::<usize>() {
    let reader = data.read().unwrap();
    if let Some(proof) = reader.get_proof(TxnSID { index: txn_sid }) {
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

fn query_utxo_map<AA>(data: web::Data<Arc<RwLock<AA>>>,
                      _info: web::Path<String>)
                      -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  let mut reader = data.write().unwrap();

  if let Some(vec) = reader.get_utxo_map() {
    Ok(serde_json::to_string(&vec)?)
  } else {
    Err(actix_web::error::ErrorNotFound("The bitmap is unavailable."))
  }
}

fn query_policy<LA>(data: web::Data<Arc<RwLock<LA>>>,
                    info: web::Path<String>)
                    -> actix_web::Result<web::Json<CustomAssetPolicy>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(asset_policy_key) = AssetPolicyKey::new_from_base64(&*info) {
    if let Some(policy) = reader.get_asset_policy(&asset_policy_key) {
      Ok(web::Json(policy))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified asset policy does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid asset policy encoding."))
  }
}

fn query_contract<LA>(data: web::Data<Arc<RwLock<LA>>>,
                      info: web::Path<String>)
                      -> actix_web::Result<web::Json<SmartContract>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(smart_contract_key) = SmartContractKey::new_from_base64(&*info) {
    if let Some(contract) = reader.get_smart_contract(&smart_contract_key) {
      Ok(web::Json(contract))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified smart contract does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorNotFound("Invalid smart contract encoding."))
  }
}

impl RestfulApiService {
  pub fn create<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(
    ledger_access: Arc<RwLock<LA>>)
    -> io::Result<RestfulApiService> {
    let web_runtime = actix_rt::System::new("eian API");
    let data = web::Data::new(ledger_access.clone());
    HttpServer::new(move || {
      App::new().data(data.clone())
                .route("/utxo_sid/{sid}", web::get().to(query_utxo::<LA>))
                .route("/asset_token/{token}", web::get().to(query_asset::<LA>))
                .route("/txn_sid/{sid}", web::get().to(query_txn::<LA>))
                .route("/proof/{sid}", web::get().to(query_proof::<LA>))
                .route("/utxo_map", web::get().to(query_utxo_map::<LA>))
                .route("/utxo_map_checksum",
                       web::get().to(query_utxo_map_checksum::<LA>))
                .route("/policy_key/{key}", web::get().to(query_policy::<LA>))
                .route("/contract_key/{key}", web::get().to(query_contract::<LA>))
    }).bind("127.0.0.1:8668")?
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
  use ledger::store::{ArchiveUpdate, LedgerState, LedgerUpdate};
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

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    state.apply_transaction(&mut tx);
    state.append_transaction(tx);

    let mut app = test::init_service(App::new().data(Arc::new(RwLock::new(state)))
                                               .route("/asset_token/{token}",
                                                      web::get().to(query_asset::<LedgerState>)));

    let req = test::TestRequest::get().uri(&format!("/asset_token/{}", token_code1.to_base64()))
                                      .to_request();
    let resp = test::block_on(app.call(req)).unwrap();

    assert!(resp.status().is_success());
  }
}
