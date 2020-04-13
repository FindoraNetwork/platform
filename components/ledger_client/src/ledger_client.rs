#![deny(warnings)]
use ledger::data_model::{AssetRules, AssetTypeCode, Operation, Transaction};
use ledger::store::helpers::*;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let mut prng = ChaChaRng::from_entropy();
  let mut tx = Transaction::default();

  let token_code1 = AssetTypeCode { val: [1; 16] };
  let (public_key, secret_key) = build_keys(&mut prng);

  let asset_body =
    asset_creation_body(&token_code1, &public_key, AssetRules::default(), None, None);
  let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
  tx.operations.push(Operation::DefineAsset(asset_create));

  // env_logger::init();

  let client = reqwest::Client::new();

  let token_code_b64 = token_code1.to_base64();
  println!("\n\nQuery asset_token {:?}", &token_code1);

  let host = "localhost";
  let port = "8668";

  let mut res = reqwest::get(&format!("http://{}:{}/{}/{}",
                                      &host, &port, "asset_token", &token_code_b64))?;

  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  // copy the response body directly to stdout
  std::io::copy(&mut res, &mut std::io::stdout())?;

  println!("\n\nSubmit transaction");

  res = client.post(&format!("http://{}:{}/{}", &host, &port, "submit_transaction"))
              .json(&tx)
              .send()?;
  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  // copy the response body directly to stdout
  std::io::copy(&mut res, &mut std::io::stdout())?;

  println!("\n\nQuery global_state {:?} again", &token_code1);
  res = reqwest::get(&format!("http://{}:{}/{}/{}", &host, &port, "global_state", 0))?;

  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  let mut res = reqwest::get(&format!("http://{}:{}/{}/{}",
                                      &host, &port, "asset_token", &token_code_b64))?;

  println!("Status: {}", res.status());
  println!("Headers:\n{:?}", res.headers());

  // copy the response body directly to stdout
  std::io::copy(&mut res, &mut std::io::stdout())?;

  println!("\n\nDone.");

  Ok(())
}
