#![allow(warnings)]

mod shared;

//use rand_chacha::ChaChaRng;
//use rand_core::SeedableRng;
use air::{check_merkle_proof, AIRResult, AIR};
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use shared::{protocol_host, AIRAddressAndPoK, Bitmap, PubCreds, QUERY_PORT};
use zei::api::anon_creds::{ac_verify, ACCommitment};

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
  println!("Response from issuer is:\n{:?}", &issuer_resp);
  let attributes = vec![Some(String::from("dob:08221964")),
                        Some(String::from("ss:666666666")),
                        None,
                        None];
  let bitmap = Bitmap { bits: vec![true, true, false, false] };
  let bitmap_str = urlencode(&serde_json::to_string(&bitmap)?);
  let req_string = format!("http://127.0.0.1:3031/reveal/{}/", &credname);

  println!("req string is:\n{}, urlencoded bitmap string is {}",
           &req_string, bitmap_str);

  let client = reqwest::Client::new();
  let addr_and_pok: AIRAddressAndPoK = client.post(&req_string)
                                             .json::<Bitmap>(&bitmap)
                                             .send()
                                             .await?
                                             .json::<AIRAddressAndPoK>()
                                             .await?;

  println!("Response from user is:\n{:?}", &addr_and_pok);

  // Step 2: Get value and Merkle proof from AIR address

  let (protocol, host) = protocol_host();
  //  let air_result: AIRResult = reqwest::get(&format!("http://localhost:8668/air_address/{}", &addr_and_pok.addr))
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
    let commitment: ACCommitment = serde_json::from_str(&commitment_string[..]).unwrap();
    if check_merkle_proof(&air_result.merkle_root,
                          &air_result.key,
                          air_result.value.as_ref(),
                          &air_result.merkle_proof)
    {
      if let Err(e) = ac_verify::<String>(&issuer_resp.issuer_pk,
                                          &attributes[..],
                                          &commitment,
                                          &addr_and_pok.pok)
      {
        Err(format!("smt merkle proof succeeds but ac_verify fails with {}", e).into())
      } else {
        println!("smt merkle proof and ac_verify both succeed");
        Ok(())
      }
    } else {
      Err(format!("AIR merkle proof failed").into())
    }
  } else {
    // panic!(format!("No AIR entry at {}", &addr_and_pok.addr));
    Err(format!("No AIR entry at {}", &addr_and_pok.addr).into())
  }
}
