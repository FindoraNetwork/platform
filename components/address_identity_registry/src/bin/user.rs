#![allow(warnings)]

mod shared;

use ledger::data_model::errors::PlatformError;
use percent_encoding::{AsciiSet, CONTROLS, utf8_percent_encode };
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use shared::{Bitmap, PubCreds, UserCreds};
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use warp::Filter;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::api::anon_creds::{ac_commit, ac_keygen_user, ACCommitmentKey, ACPoK, ACSignature, Credential};

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

fn urlencode(input: &str) -> String {
  let iter = utf8_percent_encode(input, FRAGMENT);
  iter.collect()
}

const PROTOCOL: &str = "http";
const SERVER_HOST: &str = "localhost";

/// Port for querying values.
const QUERY_PORT: &str = "8668";
/// Port for submitting transactions.
const SUBMIT_PORT: &str = "8669";

// From txn_builder_cli: need a working key pair String
const KEY_PAIR_STR: &str = "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc720fdbac9b10b7587bba7b5bc163bce69e796d71e4ed44c10fcb4488689f7a144";

fn air_assign(issuer_id: u64,
              address: &str,
              data: &str)
              -> Result<(), PlatformError> {
  Ok(())
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct InfallibleFailure {
  pub msg: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Setup: At the outset, the User is a client of the Issuer
  // Step 1: Get the issuer_pk for the credential of interest
  let credname = urlencode("passport");
  let resp1 =
    reqwest::get(&format!("http://localhost:3030/issuer_pk/{}", &credname)).await?
                                                                           .json::<PubCreds>()
                                                                           .await?;
  println!("Response from issuer for public key is:\n{}", serde_json::to_string(&resp1).unwrap());

  // Step 2: generate user key pair for this credential
  let mut prng = ChaChaRng::from_entropy();
  let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &resp1.issuer_pk);
  let attrs: Vec<String> = vec![String::from("dob:08221964"),
                                String::from("ss:666666666"),
                                String::from("photo:https://bit.ly/gotohell"),
                                String::from("dl:123456")];
  let user_creds = UserCreds { credname: credname.to_string(),
                               user_pk: user_pk.clone(),
                               attrs: attrs.clone() };
  let client = reqwest::Client::new();
  let resp2 = client.put("http://localhost:3030/sign/passport")
                    .json::<UserCreds>(&user_creds)
                    .send()
                    .await?;

  let resp_text = &resp2.bytes().await?;

  println!("Response from issuer for signature is:\n{:?}", &resp_text);
  let sig: ACSignature = serde_json::from_str(std::str::from_utf8(resp_text).unwrap()).unwrap();

  let credential: Credential<String> =
    Credential { signature: sig.clone(),
                 attributes: attrs,
                 issuer_pk: resp1.issuer_pk.clone() };

  if let Ok((commitment, _proof, key)) =
    ac_commit::<ChaChaRng, String>(&mut prng, &user_sk, &credential, b"random message")
  {
    // Now we store the commitment to this credential at the AIR
    // Build the transaction
    let issuer_key_pair = XfrKeyPair::zei_from_bytes(&hex::decode(KEY_PAIR_STR)?);
    
    let mut txn_builder = TransactionBuilder::default();
    let data = serde_json::to_string(&commitment).unwrap();
    let address = serde_json::to_string(&user_creds.user_pk).unwrap();
    txn_builder.add_operation_air_assign(&issuer_key_pair, &address, &data)?;

    // Submit to ledger
    let txn = txn_builder.transaction();
    let mut res =
      client.post(&format!("{}://{}:{}/{}",
                          PROTOCOL, SERVER_HOST, SUBMIT_PORT, "submit_transaction"))
            .json(&txn)
            .send()
            .await?;

    // For the rest of the session, the User process behaves as a server w.r.t. the Verifier

    let db = models::make_db(prng, user_sk, user_pk, credential, key);
    let api = filters::user(db);

    println!("User: waiting for verifier on 3031");

    // View access logs by setting `RUST_LOG=user`.
    let routes = api.with(warp::log("user"));
    // Start up the server...
    warp::serve(routes).run(([127, 0, 0, 1], 3031)).await;

    Ok(())
  } else {
    Err("Commitment fails")?
  }
}

mod filters {
  use crate::shared::Bitmap;
  use super::handlers;
  use super::models::Db;
  use warp::Filter;

  /// The User filters combined.
  pub fn user(db: Db)
                -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    reveal(db.clone()).or(ping(db))
  }

  pub fn reveal(db: Db) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("reveal" / String ).and(warp::post())
                                   .and(json_body())
                                   .and(with_db(db))
                                   .and_then(handlers::reveal)
  }

  pub fn ping(db: Db) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("ping").and(warp::get())
                       .and(with_db(db))
                       .and_then(handlers::ping)
  }

  fn with_db(db: Db) -> impl Filter<Extract = (Db,), Error = std::convert::Infallible> + Clone {
    warp::any().map(move || db.clone())
  }

  fn json_body() -> impl Filter<Extract = (Bitmap,), Error = warp::Rejection> + Clone {
    // When accepting a body, we want a JSON body
    // (and to reject huge payloads)...
    warp::body::content_length_limit(1024 * 16).and(warp::body::json())
  }
}

mod handlers {
  use crate::shared::{AIRAddressAndPoK, Bitmap};
  use super::models::{Db, GlobalState};
  use rand_chacha::ChaChaRng;
  use std::convert::Infallible;
  use warp::Filter;
  use warp::http::StatusCode;
  use zei::api::anon_creds::{ac_open_commitment};

  /// POST //reveal/:credname/:bitmap
  pub async fn reveal(credname: String,
                      bitmap: Bitmap,
                      db: Db)
                      -> Result<impl warp::Reply, Infallible> {
    println!("User:reveal credname = {}, bitmap = {:?}", &credname, &bitmap);

    let mut global_state = db.lock().await;
    let cred = global_state.cred.clone();
    let user_sk = global_state.user_sk.clone();
    let user_pk = global_state.user_pk.clone();
    let key = global_state.key.clone();


    if let Ok(pok) = ac_open_commitment::<ChaChaRng, String>(&mut global_state.prng,
                                                             &user_sk,
                                                             &cred,
                                                             &key,
                                                             &bitmap.bits) {
      let address = serde_json::to_string(&user_pk).unwrap();
      let result = AIRAddressAndPoK { addr: address, pok };
      println!("User: reveal success with result = {:?}", &result);

      Ok(warp::reply::json(&result))
    } else {
      let result = super::InfallibleFailure { msg: String::from("reveal: open commitment failed") };
      Ok(warp::reply::json(&result))
    }
  }

  /// GET //ping
  pub async fn ping(db: Db) -> Result<impl warp::Reply, Infallible> {
    println!("User.ping: call received");
    let result = super::InfallibleFailure { msg: String::from("PONG") };
    Ok(warp::reply::json(&result))
  }
}

mod models {
  use crate::shared::{};
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use serde_derive::{Deserialize, Serialize};
  use std::collections::HashMap;
  use std::sync::Arc;
  use tokio::sync::Mutex;
  use zei::api::anon_creds::{
    ACCommitmentKey, ACPoK, ACUserPublicKey, ACUserSecretKey, Credential,
  };
  /// So we don't have to tackle how different database work, we'll just use
  /// a simple in-memory DB, a HashMap synchronized by a mutex.
  pub type Db = Arc<Mutex<GlobalState>>;

  pub struct GlobalState {
    pub prng: ChaChaRng,
    pub user_sk: ACUserSecretKey,
    pub user_pk: ACUserPublicKey,
    pub cred: Credential<String>,
    pub key: ACCommitmentKey,
  }

  pub fn make_db(prng: ChaChaRng,
                 user_sk: ACUserSecretKey,
                 user_pk: ACUserPublicKey,
                 cred: Credential<String>,
                 key: ACCommitmentKey) -> Db {
    Arc::new(Mutex::new(GlobalState { prng, user_sk, user_pk, cred, key }))
  }
}