#![deny(warnings)]
extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate serde_json;

use actix_cors::Cors;
use actix_web::{dev, error, middleware, web, App, HttpResponse, HttpServer};
use ledger::data_model::*;
use ledger::store::{ArchiveAccess, LedgerAccess};
use std::io;
use std::marker::{Send, Sync};
use std::sync::{Arc, RwLock};

pub struct RestfulApiService {
  web_runtime: actix_rt::SystemRunner,
}

// Future refactor:
// Merge query functions
//
// Query functions for LedgerAccess are very similar, especially these three:
//   query_asset
//   query_policy
//   query_contract
// If we add more functions with the similar pattern, it will be good to merge them

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
    Err(actix_web::error::ErrorBadRequest("Invalid txo sid encoding"))
  }
}

fn query_asset_issuance_num<LA>(data: web::Data<Arc<RwLock<LA>>>,
                                info: web::Path<String>)
                                -> actix_web::Result<web::Json<u64>>
  where LA: LedgerAccess
{
  let reader = data.read().unwrap();
  if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
    if let Some(iss_num) = reader.get_issuance_num(&token_code) {
      Ok(web::Json(iss_num))
    } else {
      Err(actix_web::error::ErrorNotFound("Specified asset definition does not currently exist."))
    }
  } else {
    Err(actix_web::error::ErrorBadRequest("Invalid asset definition encoding."))
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
    Err(actix_web::error::ErrorBadRequest("Invalid asset definition encoding."))
  }
}

#[allow(unused)]
fn query_policy<LA>(data: web::Data<Arc<RwLock<LA>>>,
                    info: web::Path<String>)
                    -> actix_web::Result<web::Json<CustomAssetPolicy>>
  where LA: LedgerAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorBadRequest("unimplemented"))
  // let reader = data.read().unwrap();
  // if let Ok(asset_policy_key) = AssetPolicyKey::new_from_base64(&*info) {
  //   if let Some(policy) = reader.get_asset_policy(&asset_policy_key) {
  //     Ok(web::Json(policy))
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("Specified asset policy does not currently exist."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorBadRequest("Invalid asset policy encoding."))
  // }
}

#[allow(unused)]
fn query_contract<LA>(data: web::Data<Arc<RwLock<LA>>>,
                      info: web::Path<String>)
                      -> actix_web::Result<web::Json<SmartContract>>
  where LA: LedgerAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorBadRequest("unimplemented"))

  // let reader = data.read().unwrap();
  // if let Ok(smart_contract_key) = SmartContractKey::new_from_base64(&*info) {
  //   if let Some(contract) = reader.get_smart_contract(&smart_contract_key) {
  //     Ok(web::Json(contract))
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("Specified smart contract does not currently exist."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorBadRequest("Invalid smart contract encoding."))
  // }
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
    Err(actix_web::error::ErrorBadRequest("Invalid txn sid encoding."))
  }
}

fn stringer(data: &[u8; 32]) -> String {
  let mut result = "".to_string();

  for d in data.iter() {
    result = result + &format!("{:02x}", *d);
  }

  result
}

fn query_global_state<AA>(data: web::Data<Arc<RwLock<AA>>>) -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  let reader = data.read().unwrap();
  let (hash, version) = reader.get_global_block_hash();
  let result = format!("{} {}", stringer(&hash.0), version);
  Ok(result)
}

fn query_blocks_since<AA>(data: web::Data<Arc<RwLock<AA>>>,
                          block_id: web::Path<usize>)
                          -> web::Json<Vec<(usize, Vec<FinalizedTransaction>)>>
  where AA: ArchiveAccess
{
  let reader = data.read().unwrap();
  let mut ret = Vec::new();
  for ix in block_id.into_inner()..reader.get_block_count() {
    let sid = BlockSID(ix);
    let block = reader.get_block(sid).unwrap();
    ret.push((sid.0, block.clone()));
  }
  web::Json(ret)
}

fn query_block_log<AA>(data: web::Data<Arc<RwLock<AA>>>) -> impl actix_web::Responder
  where AA: ArchiveAccess
{
  let reader = data.read().unwrap();
  let mut res = String::new();
  res.push_str("<html><head><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"></head><body><table border=\"1\">");
  res.push_str("<tr>");
  res.push_str("<th>Block ID</th>");
  res.push_str("<th>Transactions</th>");
  res.push_str("</tr>");
  for ix in 0..reader.get_block_count() {
    let block = reader.get_block(BlockSID(ix)).unwrap();
    res.push_str("<tr>");

    res.push_str(&format!("<td>{}</td>", ix));

    res.push_str("<td><table border=\"1\">");
    res.push_str("<tr>");
    res.push_str("<th>TXN ID</th>");
    res.push_str("<th>merkle id</th>");
    res.push_str("<th>Operations</th>");
    res.push_str("</tr>");

    for txn in block.iter() {
      res.push_str("<tr>");
      res.push_str(&format!("<td>{}</td>", txn.tx_id.0));
      res.push_str(&format!("<td>{}</td>", txn.merkle_id));
      res.push_str("<td>");
      res.push_str("<table border=\"1\">");
      for op in txn.txn.operations.iter() {
        res.push_str(&format!("<tr><td><pre>{}</pre></td></tr>",
                              serde_json::to_string_pretty(&op).unwrap()));
      }
      res.push_str("</table></td>");
      res.push_str("</tr>");
    }

    res.push_str("</table></td></tr>");
  }
  res.push_str("</table></body></html>");
  HttpResponse::Ok().content_type("text/html; charset=utf-8")
                    .body(res)
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
    Err(actix_web::error::ErrorBadRequest("Invalid txn sid encoding."))
  }
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
    Err(actix_web::error::ErrorBadRequest("Invalid version encoding."))
  }
}

#[allow(unused)]
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

#[allow(unused)]
fn query_utxo_partial_map<AA>(data: web::Data<Arc<RwLock<AA>>>,
                              info: web::Path<String>)
                              -> actix_web::Result<String>
  where AA: ArchiveAccess
{
  // TODO(joe?): Implement this
  Err(actix_web::error::ErrorBadRequest("unimplemented"))
  // if let Some(block_list) = parse_blocks(info.to_string()) {
  //   let mut reader = data.write().unwrap();

  //   if let Some(vec) = reader.get_utxos(block_list) {
  //     Ok(serde_json::to_string(&vec)?)
  //   } else {
  //     Err(actix_web::error::ErrorNotFound("The map is unavailable."))
  //   }
  // } else {
  //   Err(actix_web::error::ErrorBadRequest("Invalid block list encoding."))
  // }
}

enum ServiceInterface {
  LedgerAccess,
  ArchiveAccess,
}

trait Route {
  fn set_route<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(self,
                                                                         service_interface: ServiceInterface)
                                                                         -> Self;

  fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(self) -> Self;

  fn set_route_for_archive_access<AA: 'static + ArchiveAccess + Sync + Send>(self) -> Self;
}

impl<T, B> Route for App<T, B>
  where B: actix_web::dev::MessageBody,
        T: actix_service::NewService<Config = (),
                                     Request = dev::ServiceRequest,
                                     Response = dev::ServiceResponse<B>,
                                     Error = error::Error,
                                     InitError = ()>
{
  // Call the appropraite function depending on the interface
  fn set_route<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(self,
                                                                         service_interface: ServiceInterface)
                                                                         -> Self {
    match service_interface {
      ServiceInterface::LedgerAccess => self.set_route_for_ledger_access::<LA>(),
      ServiceInterface::ArchiveAccess => self.set_route_for_archive_access::<LA>(),
    }
  }

  // Set routes for the LedgerAccess interface
  fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(self) -> Self {
    self.route("/utxo_sid/{sid}", web::get().to(query_utxo::<LA>))
        .route("/asset_issuance_num/{token}",
               web::get().to(query_asset_issuance_num::<LA>))
        .route("/asset_token/{token}", web::get().to(query_asset::<LA>))
        .route("/policy_key/{key}", web::get().to(query_policy::<LA>))
        .route("/contract_key/{key}", web::get().to(query_contract::<LA>))
  }

  // Set routes for the ArchiveAccess interface
  fn set_route_for_archive_access<AA: 'static + ArchiveAccess + Sync + Send>(self) -> Self {
    self.route("/txn_sid/{sid}", web::get().to(query_txn::<AA>))
        .route("/global_state", web::get().to(query_global_state::<AA>))
        .route("/proof/{sid}", web::get().to(query_proof::<AA>))
        .route("/block_log", web::get().to(query_block_log::<AA>))
        .route("/blocks_since/{block_sid}",
               web::get().to(query_blocks_since::<AA>))
        .route("/utxo_map", web::get().to(query_utxo_map::<AA>))
        .route("/utxo_map_checksum",
               web::get().to(query_utxo_map_checksum::<AA>))
        .route("/utxo_partial_map/{sidlist}",
               web::get().to(query_utxo_partial_map::<AA>))
  }
}

impl RestfulApiService {
  pub fn create<LA: 'static + LedgerAccess + ArchiveAccess + Sync + Send>(
    ledger_access: Arc<RwLock<LA>>,
    host: &str,
    port: &str)
    -> io::Result<RestfulApiService> {
    let web_runtime = actix_rt::System::new("findora API");

    HttpServer::new(move || {
      App::new().wrap(middleware::Logger::default())
                .wrap(Cors::new().supports_credentials())
                .data(ledger_access.clone())
                .set_route::<LA>(ServiceInterface::LedgerAccess)
                .set_route::<LA>(ServiceInterface::ArchiveAccess)
    }).bind(&format!("{}:{}", host, port))?
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
  use ledger::store::{LedgerState, LedgerUpdate, TxnEffect};
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;

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
    {
      let mut block = state.start_block().unwrap();
      state.apply_transaction(&mut block, effect).unwrap();
      state.finish_block(block).unwrap();
    }

    let mut app = test::init_service(App::new().data(Arc::new(RwLock::new(state)))
                                               .route("/asset_token/{token}",
                                                      web::get().to(query_asset::<LedgerState>)));

    let req = test::TestRequest::get().uri(&format!("/asset_token/{}", token_code1.to_base64()))
                                      .to_request();
    let resp = test::block_on(app.call(req)).unwrap();

    assert!(resp.status().is_success());
  }
}
