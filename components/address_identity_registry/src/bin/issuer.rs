// #![deny(warnings)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
use std::env;
use warp::Filter;

/// Provides a RESTful web server which manages credential types. Uses
/// JSON as a data transportation format.
///
/// API:
///
/// - `GET /credinfo`: return list of names with number of attributes.
/// - `GET /keypair/:uname/:credtype`: return User keypair for uname/credtype.
/// - `GET /issuer_pk/:credname`: return Issuer public key for named credential type.
/// - `PUT /keypair/:uname/:credtype`: create User keypair.
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
  use warp::Filter;

  /// The 4 Issuer filters combined.
  pub fn issuer(db: Db)
                -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    get_credinfo(db.clone()).or(get_keypair(db.clone()))
                            .or(get_issuer_pk(db.clone()))
                            .or(put_keypair(db))
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

  /// GET /keypair/:uname/:credtype
  pub fn get_keypair(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("keypair" / String / String).and(warp::get())
                                            .and(with_db(db))
                                            .and_then(handlers::get_keypair)
  }

  /// GET /public_key/:credname
  pub fn get_issuer_pk(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("public_key" / String).and(warp::get())
                                      .and(with_db(db))
                                      .and_then(handlers::get_issuer_pk)
  }

  /// PUT /keypair/:uname/:credtype with JSON body
  pub fn put_keypair(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("keypair" / String / String).and(warp::put())
//                                            .and(json_body())
                                            .and(with_db(db))
                                            .and_then(handlers::put_keypair)
  }
  /*
    /// DELETE /credentials/:id
    pub fn credentials_delete(
      db: Db)
      -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
      // We'll make one of our endpoints admin-only to show how authentication filters are used
      let admin_only = warp::header::exact("authorization", "Bearer admin");

      warp::path!("credentials" / String)
                                         // It is important to put the auth check _after_ the path filters.
                                         // If we put the auth check before, the request `PUT /credentials/invalid-string`
                                         // would try this filter and reject because the authorization header doesn't match,
                                         // rather because the param is wrong for that other path.
                                         .and(admin_only)
                                         .and(warp::delete())
                                         .and(with_db(db))
                                         .and_then(handlers::delete_credential)
    }
  */

  fn with_db(db: Db) -> impl Filter<Extract = (Db,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
  }

  fn json_body() -> impl Filter<Extract = (CredentialKind,), Error = warp::Rejection> + Clone {
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
  use super::models::{CredentialKind, Db, ListOptions};
  use std::convert::Infallible;
  use warp::http::StatusCode;

  /// GET /crednames?offset=3&limit=5
  pub async fn get_credinfo(opts: ListOptions, db: Db) -> Result<impl warp::Reply, Infallible> {
    // Just return a JSON array of credentials, applying the limit and offset.
    let credential_kinds = db.lock().await;
    let credential_kinds: Vec<CredentialKind> =
      credential_kinds.clone()
                      .into_iter()
                      .skip(opts.offset.unwrap_or(0))
                      .take(opts.limit.unwrap_or(std::usize::MAX))
                      .collect();
    Ok(warp::reply::json(&credential_kinds))
  }

  /// GET /keypair/:uname/:credtype
  pub async fn get_keypair(uname: String,
                           credname: String,
                           db: Db)
                           -> Result<impl warp::Reply, Infallible> {
    // Just return a JSON array of credentials, applying the limit and offset.
    let uname = urldecode::decode(uname);
    let credname = urldecode::decode(credname);
    log::debug!("get_keypair: uname={}, credname={}", uname, credname);
    let credentials = db.lock().await;
    // Look for the specified Credential...
    for credential in credentials.iter() {
      if credential.name == uname {
        return Ok(warp::reply::json(&credential));
      }
    }

    Ok(warp::reply::json(&"{}"))
  }

  /// GET /issuer_pk/:credname
  pub async fn get_issuer_pk(credname: String, db: Db) -> Result<impl warp::Reply, Infallible> {
    Ok(warp::reply::json(&"{}"))
  }

  /// PUT /keypair/:uname/:credtype
  pub async fn put_keypair(uname: String,
                           credname: String,
                           db: Db)
                           -> Result<impl warp::Reply, Infallible> {
    Ok(warp::reply::json(&"{}"))
  }
}

mod models {
  use serde_derive::{Deserialize, Serialize};
  use std::sync::Arc;
  use tokio::sync::Mutex;
  /*
  use zei::api::anon_creds::{
    ac_keygen_issuer, ac_keygen_user, ac_sign,
    ACCommitment, ACCommitmentKey, ACIssuerPublicKey, ACIssuerSecretKey, ACPoK, ACSignature,
    ACUserPublicKey, ACUserSecretKey,
  };
  */
  /// So we don't have to tackle how different database work, we'll just use
  /// a simple in-memory DB, a vector synchronized by a mutex.
  pub type Db = Arc<Mutex<Vec<CredentialKind>>>;

  pub fn make_db() -> Db {
    Arc::new(Mutex::new(vec![mk_credkind("passport", 4),
                             mk_credkind("drivers license", 4),
                             mk_credkind("security clearance", 8)]))
  }

  #[derive(Debug, Deserialize, Serialize, Clone)]
  pub struct CredentialKind {
    pub name: String,
    pub num_attrs: u64,
  }

  fn mk_credkind(name: &str, num_attrs: u64) -> CredentialKind {
    CredentialKind { name: String::from(name),
                     num_attrs }
  }
  // The query parameters for list_credentials.
  #[derive(Debug, Deserialize)]
  pub struct ListOptions {
    pub offset: Option<usize>,
    pub limit: Option<usize>,
  }
}

#[cfg(test)]
mod tests {
}
