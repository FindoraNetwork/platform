#![allow(warnings)]

mod shared;

use air::{check_merkle_proof, AIRResult, AIR};
use credentials::{credential_user_key_gen, credential_verify, CredCommitment};
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use shared::{AIRAddressAndPoK, Bitmap};
use shared::{PubCreds, UserCreds};

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');
/*
fn urldecode(s: &str) -> String {
  let iter = percent_decode(s.as_bytes());
  iter.decode_utf8().unwrap().to_string()
}
*/
fn urlencode(input: &str) -> String {
  let iter = utf8_percent_encode(input, FRAGMENT);
  iter.collect()
}

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
  let user_creds = UserCreds { credname: credname.to_string(),
                               user_pk,
                               attrs };
  println!("Response from issuer is:\n{:?}", &issuer_resp);
  let dob = b"08221964";
  let ss = b"666666666";
  let attributes = vec![(String::from("dob"), &dob[..]),
                        (String::from("ss"), &ss[..])];
  let bitmap = Bitmap { bits: vec![true, true, false, false] };
  let req_string = format!("http://127.0.0.1:3031/reveal/{}/", &credname);


  let client = reqwest::Client::new();
  let addr_and_pok: AIRAddressAndPoK = client.post(&req_string)
                                             .json::<Bitmap>(&bitmap)
                                             .send()
                                             .await?
                                             .json::<AIRAddressAndPoK>()
                                             .await?;

  println!("Response from user is:\n{:?}", &addr_and_pok);

  // Step 2: Get value and Merkle proof from AIR address

  let air_result: AIRResult = reqwest::get(&format!("http://localhost:8668/air_address/{}",
                                                    &addr_and_pok.addr)).await?
                                                                        .json::<AIRResult>()
                                                                        .await?;

  println!("Response from ledger is:\n{:?}", &air_result);

  let air_entry = air_result.value.clone();
  // Ok(())
  if let Some(commitment_string) = air_entry {
    let commitment: CredCommitment = serde_json::from_str(&commitment_string[..]).unwrap();
    if check_merkle_proof(&air_result.merkle_root,
                          &air_result.key,
                          air_result.value.as_ref(),
                          &air_result.merkle_proof)
    {
      if let Err(e) = credential_verify(&issuer_resp.issuer_pk,
                                        &attributes[..],
                                        &commitment,
                                        &addr_and_pok.pok)
      {
        panic!(format!("smt merkle proof succeeds but ac_verify fails with {}", e));
      } else {
        println!("smt merkle proof and ac_verify succeed");
      }
    } else {
      panic!("AIR merkle proof failed");
    }
  } else {
    panic!(format!("No AIR entry at {}", &addr_and_pok.addr));
  }

  Ok(())
}
