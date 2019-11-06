// Credentialing interface, with Issuer, User and Prover components
// Currently support verifying the lower bound of the credit score
//
// To compile, run wasm-pack build in the wasm_credential directory
// To test, run wasm-pack test --node in the wasm_credential directory

#![deny(warnings)]
extern crate rand;
extern crate rand_chacha;
extern crate serde;
extern crate wasm_bindgen;
extern crate zei;

use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use serde::{Deserialize, Serialize};
use std::str;
use wasm_bindgen::prelude::*;
use zei::algebra::bls12_381::{BLSGt, BLSScalar, BLSG1, BLSG2};
use zei::algebra::groups::Scalar;
use zei::crypto::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey,
  ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Issuer {
  public_key: ACIssuerPublicKey<BLSG1, BLSG2>,
  secret_key: ACIssuerSecretKey<BLSG1, BLSScalar>,
}

#[wasm_bindgen]
impl Issuer {
  // Create a new issuer, generating the key pair with the knowledge of the number of attributes
  // TODO (Keyao):
  //  Make sure we can tell which attribute is which, possibly by fixing the order of attributes
  //  After this is done, pass all the attributes to sign_credit_score and sign the credit score only
  pub fn new(num_attr: usize) -> Issuer {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_, BLSGt>(&mut prng, num_attr);

    Issuer { public_key: issuer_pk,
             secret_key: issuer_sk }
  }

  #[wasm_bindgen]
  // Convert an Issuer to JsValue
  pub fn to_jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  #[wasm_bindgen]
  // Sign the low bound of the credit score
  pub fn sign_min_credit_score(&self, user_jsvalue: &JsValue, min_credit_score: u64) -> JsValue {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);
    let user: User = user_jsvalue.into_serde().unwrap();

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let sig = ac_sign::<_, BLSGt>(&mut prng, &self.secret_key, &user.public_key, &attrs);

    JsValue::from_serde(&sig).unwrap()
  }
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct User {
  public_key: ACUserPublicKey<BLSG1>,
  secret_key: ACUserSecretKey<BLSScalar>,
}

#[wasm_bindgen]
impl User {
  // Create a new user, generating the key pair using the issuer's public key
  pub fn new(issuer: &Issuer, rand_seed: &str) -> User {
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([rand_seed.as_bytes()[0]; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_, BLSGt>(&mut prng, &issuer.public_key);

    User { public_key: user_pk,
           secret_key: user_sk }
  }

  #[wasm_bindgen]
  // Convert a User to JsValue
  pub fn to_jsvalue(&mut self) -> JsValue {
    JsValue::from_serde(&self).unwrap()
  }

  #[wasm_bindgen]
  // Commit the lower bound of the credit score with the issuer's signature
  pub fn commit_min_credit_score(&self,
                                 issuer_jsvalue: &JsValue,
                                 sig: &JsValue,
                                 min_credit_score: u64,
                                 reveal_credit_score: bool)
                                 -> JsValue {
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();
    let sig: ACSignature<BLSG1> = sig.into_serde().unwrap();
    let mut prng = ChaChaRng::from_seed([0u8; 32]);

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let bitmap = [reveal_credit_score];

    let proof = ac_reveal::<_, BLSGt>(&mut prng,
                                      &self.secret_key,
                                      &issuer.public_key,
                                      &sig,
                                      &attrs,
                                      &bitmap).unwrap();

    JsValue::from_serde(&proof).unwrap()
  }
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Prover;

#[wasm_bindgen]
impl Prover {
  #[wasm_bindgen]
  // Verify the lower bound of credit score
  pub fn verify_min_credit_score(proof_jsvalue: &JsValue,
                                 issuer_jsvalue: &JsValue,
                                 min_credit_score: u64,
                                 reveal_min_credit_score: bool)
                                 -> bool {
    let proof: ACRevealSig<BLSG1, BLSG2, BLSScalar> = proof_jsvalue.into_serde().unwrap();
    let issuer: Issuer = issuer_jsvalue.into_serde().unwrap();

    let attrs = [BLSScalar::from_u64(min_credit_score)];
    let bitmap = [reveal_min_credit_score];
    ac_verify::<BLSGt>(&issuer.public_key, &attrs, &bitmap, &proof).is_ok()
  }

  #[wasm_bindgen]
  // Prove that the credit score meets the requirement
  pub fn prove_min_credit_score(proof_jsvalue: &JsValue,
                                issuer_jsvalue: &JsValue,
                                min_credit_score: u64,
                                reveal_min_credit_score: bool,
                                min_requirement: u64)
                                -> bool {
    if min_credit_score < min_requirement {
      return false;
    }
    Prover::verify_min_credit_score(proof_jsvalue,
                                    issuer_jsvalue,
                                    min_credit_score,
                                    reveal_min_credit_score)
  }
}

// wasm-bindgen-test must be placed in the root of the crate or in a pub mod
extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;

#[wasm_bindgen_test]
// Test to ensure that credit score is checked correctly
fn test_credit_score_proof() {
  let mut issuer = Issuer::new(10);
  let issuer_jsvalue = issuer.to_jsvalue();
  let mut user = User::new(&issuer, "user");
  let user_jsvalue = user.to_jsvalue();

  let min_credit_score = 520;
  let fake_credit_score = 620;

  let sig_jsvalue = issuer.sign_min_credit_score(&user_jsvalue, min_credit_score);
  let proof_jsvalue =
    user.commit_min_credit_score(&issuer_jsvalue, &sig_jsvalue, min_credit_score, true);
  let fake_proof_jsvalue =
    user.commit_min_credit_score(&issuer_jsvalue, &sig_jsvalue, fake_credit_score, true);

  let requirement_low = 500;
  let requirement_high = 600;
  let incorrect_credit_score = 700;

  // Verify that prove_min_credit_score succeedes
  assert!(Prover::prove_min_credit_score(&proof_jsvalue,
                                         &issuer_jsvalue,
                                         min_credit_score,
                                         true,
                                         requirement_low));

  // Verify that prove_min_credit_score fails if:
  //  1. The lower bound of the credit score doesn't meet the requirement
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          true,
                                          requirement_high));

  // 2. The prover uses the incorrect credit score for the verification
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          incorrect_credit_score,
                                          true,
                                          requirement_high));

  // 3. The user provides a fake proof
  assert!(!Prover::prove_min_credit_score(&fake_proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          true,
                                          requirement_high));

  // 4. reveal_min_credit_score isn't consistant
  assert!(!Prover::prove_min_credit_score(&proof_jsvalue,
                                          &issuer_jsvalue,
                                          min_credit_score,
                                          false,
                                          requirement_low));
}
