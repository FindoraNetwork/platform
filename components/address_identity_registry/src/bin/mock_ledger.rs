#![deny(warnings)]

use std::env;
use warp::Filter;

/// Provides a RESTful web server managing some Credentials.
///
/// API will be:
///
/// - `GET /credentials`: return a JSON list of Credentials.
/// - `POST /credentials`: create a new Credential.
/// - `PUT /credentials/:id`: update a specific Credential.
/// - `DELETE /credentials/:id`: delete a specific Credential.
#[tokio::main]
async fn main() {
  if env::var_os("RUST_LOG").is_none() {
    // Set `RUST_LOG=mock_ledger=debug` to see debug logs,
    // this only shows access logs.
    env::set_var("RUST_LOG", "mock_ledger=info");
  }
  pretty_env_logger::init();

  let db = models::blank_db();

  let api = filters::credentials(db);

  // View access logs by setting `RUST_LOG=mock_ledger`.
  let routes = api.with(warp::log("mock_ledger"));
  // Start up the server...
  warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;
}

mod filters {
  use super::handlers;
  use super::models::{Credential, Db, ListOptions};
  use warp::Filter;

  /// The 4 CREDENTIALs filters combined.
  pub fn credentials(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    credentials_list(db.clone()).or(credentials_at(db.clone()))
                                .or(credentials_create(db.clone()))
                                .or(credentials_update(db.clone()))
                                .or(credentials_delete(db))
  }

  /// GET /credentials?offset=3&limit=5
  pub fn credentials_list(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("credentials").and(warp::get())
                              .and(warp::query::<ListOptions>())
                              .and(with_db(db))
                              .and_then(handlers::list_credentials)
  }

  /// GET /credentials/:id
  pub fn credentials_at(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("credentials" / String).and(warp::get())
                                       .and(with_db(db))
                                       .and_then(handlers::access_credential)
  }

  /// POST /credentials with JSON body
  pub fn credentials_create(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("credentials").and(warp::post())
                              .and(json_body())
                              .and(with_db(db))
                              .and_then(handlers::create_credential)
  }

  /// PUT /credentials/:id with JSON body
  pub fn credentials_update(
    db: Db)
    -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("credentials" / String).and(warp::put())
                                       .and(json_body())
                                       .and(with_db(db))
                                       .and_then(handlers::update_credential)
  }

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

  fn with_db(db: Db) -> impl Filter<Extract = (Db,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
  }

  fn json_body() -> impl Filter<Extract = (Credential,), Error = warp::Rejection> + Clone {
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
  use super::models::{Credential, Db, ListOptions};
  use std::convert::Infallible;
  use warp::http::StatusCode;

  pub async fn list_credentials(opts: ListOptions, db: Db) -> Result<impl warp::Reply, Infallible> {
    // Just return a JSON array of credentials, applying the limit and offset.
    let credentials = db.lock().await;
    let credentials: Vec<Credential> = credentials.clone()
                                                  .into_iter()
                                                  .skip(opts.offset.unwrap_or(0))
                                                  .take(opts.limit.unwrap_or(std::usize::MAX))
                                                  .collect();
    Ok(warp::reply::json(&credentials))
  }

  pub async fn access_credential(id: String, db: Db) -> Result<impl warp::Reply, Infallible> {
    // Just return a JSON array of credentials, applying the limit and offset.
    let decoded_id = urldecode::decode(id); // Why can't I use it and call it as decode? (brian)
    log::debug!("access_credential: id={}", decoded_id);
    let credentials = db.lock().await;
    // Look for the specified Credential...
    for credential in credentials.iter() {
      if credential.id == decoded_id {
        return Ok(warp::reply::json(&credential));
      }
    }

    Ok(warp::reply::json(&"{}"))
  }

  pub async fn create_credential(create: Credential,
                                 db: Db)
                                 -> Result<impl warp::Reply, Infallible> {
    log::debug!("create_credential: {:?}", create);

    let mut vec = db.lock().await;

    for credential in vec.iter() {
      if credential.id == create.id {
        log::debug!("    -> id already exists: {}", &create.id);
        // Credential with id already exists, return `400 BadRequest`.
        return Ok(StatusCode::BAD_REQUEST);
      }
    }

    // No existing Credential with id, so insert and return `201 Created`.
    vec.push(create);

    Ok(StatusCode::CREATED)
  }

  pub async fn update_credential(id: String,
                                 update: Credential,
                                 db: Db)
                                 -> Result<impl warp::Reply, Infallible> {
    let decoded_id = urldecode::decode(id);
    log::debug!("update_credential: id={}, credential={:?}",
                decoded_id,
                update);
    let mut vec = db.lock().await;

    // Look for the specified Credential...
    for credential in vec.iter_mut() {
      if credential.id == decoded_id {
        *credential = update;
        return Ok(StatusCode::OK);
      }
    }

    log::debug!("    -> credential id not found!");

    // If the for loop didn't return OK, then the ID doesn't exist...
    Ok(StatusCode::NOT_FOUND)
  }

  pub async fn delete_credential(id: String, db: Db) -> Result<impl warp::Reply, Infallible> {
    log::debug!("delete_credential: id={}", id);

    let mut vec = db.lock().await;

    let len = vec.len();
    vec.retain(|credential| {
         // Retain all Credentials that aren't this id...
         // In other words, remove all that *are* this id...
         credential.id != id
       });

    // If the vec is smaller, we found and deleted a Credential!
    let deleted = vec.len() != len;

    if deleted {
      // respond with a `204 No Content`, which means successful,
      // yet no body expected...
      Ok(StatusCode::NO_CONTENT)
    } else {
      log::debug!("    -> credential id not found!");
      Ok(StatusCode::NOT_FOUND)
    }
  }
}

mod models {
  use serde_derive::{Deserialize, Serialize};
  use std::sync::Arc;
  use tokio::sync::Mutex;

  /// So we don't have to tackle how different database work, we'll just use
  /// a simple in-memory DB, a vector synchronized by a mutex.
  pub type Db = Arc<Mutex<Vec<Credential>>>;

  pub fn blank_db() -> Db {
    Arc::new(Mutex::new(Vec::new()))
  }

  #[derive(Debug, Deserialize, Serialize, Clone)]
  pub struct Credential {
    pub id: String,
    pub text: String,
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
  use warp::http::StatusCode;
  use warp::test::request;

  use super::{
    filters,
    models::{self, Credential},
  };

  #[tokio::test]
  async fn test_post() {
    let db = models::blank_db();
    let api = filters::credentials(db);

    let resp = request().method("POST")
                        .path("/credentials")
                        .json(&Credential { id: "666".into(),
                                            text: "Asmodeus".into() })
                        .reply(&api)
                        .await;

    assert_eq!(resp.status(), StatusCode::CREATED);
  }

  #[tokio::test]
  async fn test_post_conflict() {
    let db = models::blank_db();
    db.lock().await.push(credential1());
    let api = filters::credentials(db);

    let resp = request().method("POST")
                        .path("/credentials")
                        .json(&credential1())
                        .reply(&api)
                        .await;

    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
  }

  #[tokio::test]
  async fn test_put_unknown() {
    let _ = pretty_env_logger::try_init();
    let db = models::blank_db();
    let api = filters::credentials(db);

    let resp = request().method("PUT")
                        .path("/credentials/666")
                        .header("authorization", "Bearer admin")
                        .json(&credential1())
                        .reply(&api)
                        .await;

    assert_eq!(resp.status(), StatusCode::NOT_FOUND);
  }

  fn credential1() -> Credential {
    Credential { id: "666".into(),
                 text: "Asmodeus".into() }
  }
}
