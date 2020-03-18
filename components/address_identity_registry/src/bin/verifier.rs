#![deny(warnings)]

mod shared;

//use rand_chacha::ChaChaRng;
//use rand_core::SeedableRng;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use shared::{ AIRAddressAndPoK, Bitmap };
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
  // Step 1: Get the issuer_pk for the credential of interest
  let credname = urlencode("passport");
  let bitmap = Bitmap {
    bits: vec![true, true, false, false],
  };
  let bitmap_str = urlencode(&serde_json::to_string(&bitmap)?);
  let req_string = format!("http://localhost:3031/reveal/{}/{}",
                           &credname,
                           bitmap_str);
  
  println!("req string is:\n{}", &req_string);
  
  let resp = reqwest::get(&req_string).await?.json::<AIRAddressAndPoK>().await?;
  println!("Response from user is:\n{}", serde_json::to_string(&resp).unwrap());
  // Step 2: generate user key pair for this credential
  // let mut prng = ChaChaRng::from_entropy();

  Ok(())
}
