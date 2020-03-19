#![deny(warnings)]

mod shared;

use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use shared::{PubCreds, UserCreds};
use credentials::credential_user_key_gen;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Setup
  // Step 1: Get the issuer_pk for the credential of interest
  let credname = "passport";
  let resp =
    reqwest::get(&format!("http://localhost:3030/issuer_pk/{}", &credname)).await?
                                                                           .json::<PubCreds>()
                                                                           .await?;
  println!("{}", serde_json::to_string(&resp).unwrap());
  // Step 2: generate user key pair for this credential
  let mut prng = ChaChaRng::from_entropy();
  let (user_pk, _user_sk) = credential_user_key_gen(&mut prng, &resp.issuer_pk);
  let attrs = vec![(String::from("08221964"), String::from("08221964")),
                                (String::from("ss"), String::from("666666666")),
                                (String::from("photo"), String::from("photo:https://bit.ly/gotohell")),
                                (String::from("dl"), String::from("dl:123456"))];
  let user_creds = UserCreds { credname: credname.to_string(),
                               user_pk,
                               attrs };
  let client = reqwest::Client::new();
  let resp = client.put("http://localhost:3030/sign/passport")
                   .json::<UserCreds>(&user_creds)
                   .send()
                   .await?;

  let resp_bytes = &resp.bytes().await?;
  println!("{:#?}", &resp_bytes);

  Ok(())
}
