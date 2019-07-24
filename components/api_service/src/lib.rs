extern crate actix_rt;
extern crate actix_web;
extern crate core;
extern crate serde_json;

use actix_web::{web, App, HttpServer};
use core::data_model::{
  AssetTokenCode, SmartContractKey, AssetPolicyKey, AssetToken, CustomAssetPolicy, SmartContract,
  TxnSID, TxoSID, Utxo,
};
use core::store::{ArchiveAccess, LedgerAccess};
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
  // TODO(Jonathan) This will not compile.
  // #[test]
  // fn query_get_blind_asset_record(_query: &str) {}
}
