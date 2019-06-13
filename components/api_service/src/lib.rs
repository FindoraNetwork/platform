extern crate actix_rt;
extern crate actix_web;
extern crate core;
extern crate serde_json;

use actix_web::{web, App, HttpServer};
use core::data_model::{TxoSID, Utxo, AssetTokenCode, AssetToken, AssetPolicyKey, CustomAssetPolicy, SmartContractKey, SmartContract, TxnSID};
use core::store::{ArchiveAccess, LedgerAccess};
use std::io;
use std::sync::{Arc, RwLock};
use std::marker::{Sync, Send};

pub struct RestfulApiService {
  web_runtime: actix_rt::SystemRunner,
}

fn query_utxo<LA>(data: web::Data<Arc<RwLock<LA>>>, info: web::Path<TxoSID>) -> actix_web::Result<web::Json<Utxo>>
where LA: LedgerAccess {
  let reader = data.read().unwrap();
  if let Some(txo) = reader.check_utxo(*info) {
      Ok(web::Json(txo))
  } else {
      Err(actix_web::error::ErrorNotFound("Specified txo does not currently exist."))
  }
}

fn query_asset<LA>(data: web::Data<Arc<RwLock<LA>>>, info: web::Path<AssetTokenCode>) -> actix_web::Result<web::Json<AssetToken>>
where LA: LedgerAccess {
  let reader = data.read().unwrap();
  if let Some(asset) = reader.get_asset_token(&*info) {
      Ok(web::Json(asset))
  } else {
      Err(actix_web::error::ErrorNotFound("Specified asset definition does not currently exist."))
  }
}

fn query_txn<AA>(data: web::Data<Arc<RwLock<AA>>>, info: web::Path<TxnSID>) -> actix_web::Result<String>
where AA: ArchiveAccess {
  let reader = data.read().unwrap();
  if let Some(txn) = reader.get_transaction(*info) {
      Ok(serde_json::to_string(&*txn)?)
  } else {
      Err(actix_web::error::ErrorNotFound("Specified transaction does not exist."))
  }
}

fn query_policy<LA>(data: web::Data<Arc<RwLock<LA>>>, info: web::Path<AssetPolicyKey>) -> actix_web::Result<web::Json<CustomAssetPolicy>>
where LA: LedgerAccess {
  let reader = data.read().unwrap();
  if let Some(policy) = reader.get_asset_policy(&*info) {
      Ok(web::Json(policy))
  } else {
      Err(actix_web::error::ErrorNotFound("Specified asset policy does not currently exist."))
  }
}

fn query_contract<LA>(data: web::Data<Arc<RwLock<LA>>>, info: web::Path<SmartContractKey>) -> actix_web::Result<web::Json<SmartContract>>
where LA: LedgerAccess {
  let reader = data.read().unwrap();
  if let Some(contract) = reader.get_smart_contract(&*info) {
      Ok(web::Json(contract))
  } else {
      Err(actix_web::error::ErrorNotFound("Specified smart contract definition does not currently exist."))
  }
}

impl RestfulApiService {
  pub fn create<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(ledger_access: Arc<RwLock<LA>>) -> io::Result<RestfulApiService> {
    let web_runtime = actix_rt::System::new("eian API");
    let data = web::Data::new(ledger_access.clone());
    HttpServer::new(move || { App::new()
        .data(data.clone())
        .route("/utxo_sid/{sid}", web::get().to(query_utxo::<LA>))
        .route("/asset_token/{token}", web::get().to(query_asset::<LA>))
        .route("/txn_sid/{sid}", web::get().to(query_txn::<LA>))
        .route("/policy_key/{key}", web::get().to(query_policy::<LA>))
        .route("/contract_key/{key}", web::get().to(query_contract::<LA>)) })
      .bind("127.0.0.1:8668")?
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
  #[test]
  fn query_get_blind_asset_record(_query: &str) {}
}
