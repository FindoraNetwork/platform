#![allow(warnings)]

mod shared;

use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use shared::{PubCreds, UserCreds};
use zei::api::anon_creds::{ ac_keygen_user, ac_commit, ACSignature, Credential };

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Setup
  // Step 1: Get the issuer_pk for the credential of interest
  let credname = "passport";
  let resp1 = reqwest::get(&format!("http://localhost:3030/issuer_pk/{}", &credname))
      .await?
      .json::<PubCreds>()
      .await?;
  println!("{}", serde_json::to_string(&resp1).unwrap());
  // Step 2: generate user key pair for this credential
  let mut prng = ChaChaRng::from_entropy();
  let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &resp1.issuer_pk);
  let attrs: Vec<String> = vec![String::from("dob:08221964"),
                                String::from("ss:666666666"),
                                String::from("photo:https://bit.ly/gotohell"),
                                String::from("dl:123456")];
  let user_creds = UserCreds {
    credname: credname.to_string(),
    user_pk,
    attrs: attrs.clone(),
  };
  let client = reqwest::Client::new();
  let resp2 = client.put("http://localhost:3030/sign/passport")
      .json::<UserCreds>(&user_creds)
      .send()
      .await?;

  let resp_text = &resp2.bytes().await?;

  println!("Response is {:?}", &resp_text);
  let sig: ACSignature = serde_json::from_str(std::str::from_utf8(resp_text).unwrap()).unwrap();

  let credential: Credential<&[u8]> = Credential { signature: sig.clone(),
                                                   attributes: attrs.iter().map(|s| s.as_bytes()).collect(),
                                                   issuer_pk: resp1.issuer_pk.clone() };

  if let Ok((commitment, _proof, key)) = ac_commit::<ChaChaRng, &[u8]>(&mut prng,
                                                                       &user_sk,
                                                                       &credential,
                                                                       b"random message") {
    println!("All good");
    Ok(())
  } else {
    println!("Houston, we have a problem");
    Err("Commitment fails")?
  }
}