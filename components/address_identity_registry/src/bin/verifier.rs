#![allow(warnings)]

mod shared;

use air::{check_merkle_proof, AIRResult, AIR};
use credentials::{credential_user_key_gen, credential_verify, CredCommitment};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use shared::{protocol_host, urlencode, AIRAddressAndPoK, PubCreds, RevealFields, QUERY_PORT};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Step 1: Retrieve the address and pok from user
  let credname = urlencode("passport");
  let issuer_resp =
    reqwest::get(&format!("http://localhost:3030/issuer_pk/{}", &credname)).await?
                                                                           .json::<PubCreds>()
                                                                           .await?;
  println!("{}", serde_json::to_string(&issuer_resp).unwrap());
  // Step 2: generate user key pair for this credential
  let mut prng = ChaChaRng::from_entropy();
  let (user_pk, _user_sk) = credential_user_key_gen(&mut prng, &issuer_resp.issuer_pk);
  let attrs = vec![(String::from("08221964"), String::from("08221964")),
                   (String::from("ss"), String::from("666666666")),
                   (String::from("photo"), String::from("photo:https://bit.ly/gotohell")),
                   (String::from("dl"), String::from("dl:123456"))];
  println!("Response from issuer is:\n{:?}", &issuer_resp);

  let attr_map = vec![(String::from("sex"), "M".as_bytes()),
                      (String::from("pob"), "666".as_bytes())];
  let reveal_fields = RevealFields { fields: attr_map.clone()
                                                     .into_iter()
                                                     .map(|(f, a)| f.clone())
                                                     .collect() };
  let req_string = format!("http://127.0.0.1:3031/reveal/{}/", &credname);

  let client = reqwest::Client::new();
  let addr_and_pok: AIRAddressAndPoK = client.post(&req_string)
                                             .json::<RevealFields>(&reveal_fields)
                                             .send()
                                             .await?
                                             .json::<AIRAddressAndPoK>()
                                             .await?;

  println!("Response from user is:\n{:?}", &addr_and_pok);

  // Step 2: Get value and Merkle proof from AIR address

  let (protocol, host) = protocol_host();
  println!("Looking up value at {}://{}:{}/air_address/{}",
           protocol, host, QUERY_PORT, &addr_and_pok.addr);
  let air_result: AIRResult =
    reqwest::get(&format!("{}://{}:{}/air_address/{}",
                          protocol, host, QUERY_PORT, &addr_and_pok.addr)).await?
                                                                          .json::<AIRResult>()
                                                                          .await?;

  println!("Response from ledger is:\n{:?}", &air_result);

  let air_entry = air_result.value.clone();
  if let Some(commitment_string) = air_entry {
    let commitment: CredCommitment = serde_json::from_str(&commitment_string[..]).unwrap();
    if check_merkle_proof(&air_result.merkle_root,
                          &air_result.key,
                          air_result.value.as_ref(),
                          &air_result.merkle_proof)
    {
      if let Err(e) = credential_verify(&issuer_resp.issuer_pk,
                                        &attr_map[..],
                                        &commitment,
                                        &addr_and_pok.pok)
      {
        Err(format!("smt merkle proof succeeds but ac_verify fails with {}", e).into())
      } else {
        println!("smt merkle proof and ac_verify succeed");
        Ok(())
      }
    } else {
      Err(format!("AIR merkle proof failed").into())
    }
  } else {
    Err(format!("No AIR entry at {}", &addr_and_pok.addr).into())
  }
}
