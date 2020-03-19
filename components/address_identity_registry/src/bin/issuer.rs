// #![deny(warnings)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#[macro_use]
extern crate lazy_static;

use std::env;
use std::sync::Mutex;
use warp::Filter;

mod shared;
/// Provides a RESTful web server which manages credential types. Uses
/// JSON as a data transportation format.
///
/// API:
///
/// - `GET /credinfo`: return list of names with number of attributes.
/// - `GET /issuer_pk/:credname`: return Issuer public key for named credential type.
/// - `PUT /sign/:uname`: creates a signature for the credname, user_pk, attrs.
#[tokio::main]
async fn main() {
  if env::var_os("RUST_LOG").is_none() {
    // Set `RUST_LOG=issuer=debug` to see debug logs,
    // this only shows access logs.
    env::set_var("RUST_LOG", "issuer=info");
  }
  pretty_env_logger::init();

  let db = models::make_db();

  let api = filters::issuer(db);

  // View access logs by setting `RUST_LOG=issuer`.
  let routes = api.with(warp::log("issuer"));
  // Start up the server...
  warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;
}

mod filters {
  use super::handlers;
  use super::models::{CredentialKind, Db, ListOptions};
  use crate::shared::UserCreds;
  use warp::Filter;

  /// The Issuer filters combined.
  pub fn issuer(db: Db)
                -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    get_credinfo(db.clone()).or(get_issuer_pk(db.clone()))
                            .or(sign_credential(db))
  }

  /// GET /crednames?offset=3&limit=5
  pub fn get_credinfo(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("credinfo").and(warp::get())
                           .and(warp::query::<ListOptions>())
                           .and(with_db(db))
                           .and_then(handlers::get_credinfo)
  }

  /// GET /public_key/:credname
  pub fn get_issuer_pk(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("issuer_pk" / String).and(warp::get())
                                     .and(with_db(db))
                                     .and_then(handlers::get_issuer_pk)
  }

  /// PUT /sign/:credname -- We pass a
  pub fn sign_credential(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("sign" / String).and(warp::put())
                                .and(json_body())
                                .and(with_db(db))
                                .and_then(handlers::sign_credential)
  }

  fn with_db(db: Db) -> impl Filter<Extract = (Db,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
  }

  fn json_body() -> impl Filter<Extract = (UserCreds,), Error = warp::Rejection> + Clone {
    // When accepting a body, we want a JSON body
    // (and to reject huge payloads)...
    warp::body::content_length_limit(1024 * 16).and(warp::body::json())
  }
}

/// These are our API handlers, the ends of each filter chain.
/// Notice how thanks to using `Filter::and`, we can define a function
/// with the exact arguments we'd expect from each filter in the chain.
/// No tuples are needed, it's auto flattened for the functions.
mod handlers {
  use super::models::{to_pubcreds, CredentialKind, Db, ListOptions};
  use crate::shared::{PubCreds, UserCreds};
  use rand_chacha::ChaChaRng;
  use std::convert::Infallible;
  use warp::http::StatusCode;
  use zei::api::anon_creds::ac_sign;

  /// GET /crednames?offset=3&limit=5
  pub async fn get_credinfo(opts: ListOptions, db: Db) -> Result<impl warp::Reply, Infallible> {
    // Just return a JSON array of credentials, applying the limit and offset.
    let global_state = db.lock().await;
    let credential_kinds: Vec<PubCreds> = global_state.credkinds
                                                      .values()
                                                      .cloned()
                                                      .skip(opts.offset.unwrap_or(0))
                                                      .take(opts.limit.unwrap_or(std::usize::MAX))
                                                      .map(|c| to_pubcreds(&c))
                                                      .collect();

    Ok(warp::reply::json(&credential_kinds))
  }

  /// GET /issuer_pk/:credname
  pub async fn get_issuer_pk(credname: String, db: Db) -> Result<impl warp::Reply, Infallible> {
    let credname = urldecode::decode(credname);
    log::debug!("get_issuer_pk: credname={}", credname);
    let global_state = db.lock().await;
    // Look for the specified Credential...
    if let Some(credkind) = global_state.credkinds.get(&credname) {
      Ok(warp::reply::json(&to_pubcreds(credkind)))
    } else {
      Ok(warp::reply::json(&"{}"))
    }
  }

  /// PUT /sign_credential/:credname
  pub async fn sign_credential(credname: String,
                               user_creds: UserCreds,
                               db: Db)
                               -> Result<impl warp::Reply, Infallible> {
    let mut global_state = db.lock().await;
    let credkinds = global_state.credkinds.clone();
    let cred_kind = credkinds.get(&credname).unwrap();
    let attrs: Vec<&[u8]> = user_creds.attrs.iter().map(|s| s.as_bytes()).collect();
    let sig = ac_sign(&mut global_state.prng,
                                          &cred_kind.issuer_sk,
                                          &user_creds.user_pk,
                                          &attrs).unwrap();

    Ok(warp::reply::json(&sig))
  }
}

mod models {
  use crate::shared::{PubCreds, UserCreds};
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use serde_derive::{Deserialize, Serialize};
  use std::collections::HashMap;
  use std::sync::Arc;
  use tokio::sync::Mutex;
  use zei::api::anon_creds::{
    ac_keygen_issuer, ACIssuerPublicKey, ACIssuerSecretKey, ACUserPublicKey,
  };
  /// So we don't have to tackle how different database work, we'll just use
  /// a simple in-memory DB, a HashMap synchronized by a mutex.
  pub type Db = Arc<Mutex<GlobalState>>;
  pub struct GlobalState {
    pub prng: ChaChaRng,
    pub credkinds: HashMap<String, CredentialKind>,
  }

  pub fn make_db() -> Db {
    let mut prng = ChaChaRng::from_entropy();
    let a = [(String::from("passport"), mk_credkind(&mut prng, "passport", 4)),
             (String::from("drivers license"), mk_credkind(&mut prng, "drivers license", 4)),
             (String::from("security clearance"), mk_credkind(&mut prng, "security clearance", 8))];
    let credkinds: HashMap<String, CredentialKind> = a.iter().cloned().collect();
    Arc::new(Mutex::new(GlobalState { prng, credkinds }))
  }

  #[derive(Debug, Deserialize, Serialize, Clone)]
  pub struct CredentialKind {
    pub name: String,
    pub num_attrs: u64,
    pub issuer_sk: ACIssuerSecretKey,
    pub issuer_pk: ACIssuerPublicKey,
  }

  pub fn to_pubcreds(credkind: &CredentialKind) -> PubCreds {
    PubCreds { name: credkind.name.clone(),
               num_attrs: credkind.num_attrs,
               issuer_pk: credkind.issuer_pk.clone() }
  }

  fn mk_credkind(mut prng: &mut ChaChaRng, name: &str, num_attrs: u64) -> CredentialKind {
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, num_attrs as usize);
    CredentialKind { name: String::from(name),
                     num_attrs,
                     issuer_sk,
                     issuer_pk }
  }

  #[derive(Debug, Deserialize, Serialize, Clone)]
  pub struct SignatureParams {
    name: String, // credential name
    user_pk: ACUserPublicKey,
    attrs: Vec<String>,
  }

  // The query parameters for list_credentials.
  #[derive(Debug, Deserialize)]
  pub struct ListOptions {
    pub offset: Option<usize>,
    pub limit: Option<usize>,
  }
}

#[cfg(test)]
mod tests {}
