use linear_map::LinearMap;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use zei::api::anon_creds::{
  ac_commit, ac_commit_with_key, ac_keygen_commitment, ac_keygen_issuer, ac_keygen_user,
  ac_open_commitment, ac_reveal, ac_sign, ac_verify, ac_verify_commitment, ACCommitment,
  ACCommitmentKey, ACIssuerPublicKey, ACIssuerSecretKey, ACPoK, ACRevealProof, ACRevealSig,
  ACSignature, ACUserPublicKey, ACUserSecretKey, Credential as ZeiCredential,
};
use zei::errors::ZeiError;

#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerSecretKey {
  ac_sec_key: ACIssuerSecretKey,
  map: LinearMap<String, ((usize, usize), usize)>, // key: (index, len in u32), len in bytes
  num_internal_attrs: usize,
}

#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerPublicKey {
  ac_pub_key: ACIssuerPublicKey,
  map: LinearMap<String, ((usize, usize), usize)>, // key: (index, len in u32), len in bytes
  num_internal_attrs: usize,
}

pub type CredSignature = ACSignature;
pub type CredCommitmentKey = ACCommitmentKey;
pub type CredCommitment = ACCommitment;

#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredUserPublicKey(ACUserPublicKey);

#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredUserSecretKey(ACUserSecretKey);

impl CredIssuerSecretKey {
  pub fn get_sk_ref(&self) -> &ACIssuerSecretKey {
    &self.ac_sec_key
  }
}

impl CredUserPublicKey {
  pub fn get_ref(&self) -> &ACUserPublicKey {
    &self.0
  }
}

impl CredUserSecretKey {
  pub fn get_ref(&self) -> &ACUserSecretKey {
    &self.0
  }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Credential {
  pub attributes: Vec<(String, Vec<u8>)>,
  pub issuer_pub_key: CredIssuerPublicKey,
  pub signature: CredSignature,
}

impl Credential {
  pub fn to_ac_credential(&self) -> Result<ZeiCredential, ZeiError> {
    let mut u32_attrs = vec![0u32; self.issuer_pub_key.num_internal_attrs];
    for (key, attr) in &self.attributes {
      let ((pos, len), _) = self.issuer_pub_key
                                .map
                                .get(key)
                                .ok_or(ZeiError::ParameterError)?;
      let u32_vec = u8_slice_to_u32_vec(attr, *len);
      u32_attrs[*pos..*pos + *len].clone_from_slice(&u32_vec);
    }
    Ok(ZeiCredential { signature: self.signature.clone(),
                       attributes: u32_attrs,
                       issuer_pub_key: self.issuer_pub_key.ac_pub_key.clone() })
  }
}
pub type CredPoK = ACPoK;
pub type CredRevealSig = ACRevealSig;
pub type CredRevealProof = ACRevealProof;

pub fn credential_issuer_key_gen<R: CryptoRng + RngCore>(
  prng: &mut R,
  attributes: &[(String, usize)])
  -> (CredIssuerPublicKey, CredIssuerSecretKey) {
  let mut map = LinearMap::new();
  let mut num_attrs = 0usize;
  for (key, len) in attributes {
    let num_sub_attr = num_u32_per_u8(*len);
    map.insert(key.clone(), ((num_attrs, num_sub_attr), *len));
    num_attrs += num_sub_attr;
  }
  let (issuer_pub_key, issuer_sec_key) = ac_keygen_issuer(prng, num_attrs);

  (CredIssuerPublicKey { ac_pub_key: issuer_pub_key,
                         map: map.clone(),
                         num_internal_attrs: num_attrs },
   CredIssuerSecretKey { ac_sec_key: issuer_sec_key,
                         map,
                         num_internal_attrs: num_attrs })
}

pub fn credential_user_key_gen<R: CryptoRng + RngCore>(
  prng: &mut R,
  issuer_pub_key: &CredIssuerPublicKey)
  -> (CredUserPublicKey, CredUserSecretKey) {
  let (pk, sk) = ac_keygen_user(prng, &issuer_pub_key.ac_pub_key);
  (CredUserPublicKey(pk), CredUserSecretKey(sk))
}

pub fn credential_sign<R: CryptoRng + RngCore>(prng: &mut R,
                                               issuer_sec_key: &CredIssuerSecretKey,
                                               user_pub_key: &CredUserPublicKey,
                                               attributes: &[(String, &[u8])])
                                               -> Result<CredSignature, ZeiError> {
  // A. check that attributes fields to sign matches the attribute list
  // A.1 lengths matches
  let keys: Vec<&String> = attributes.iter().map(|(x, _)| x).collect();
  if keys.len() != issuer_sec_key.map.len() {
    return Err(ZeiError::ParameterError);
  }

  // B build list of u32 parameters
  let n_u32_attrs = issuer_sec_key.num_internal_attrs;
  let mut attrs = vec![0u32; n_u32_attrs];
  for (attr_key, attr_value) in attributes {
    let ((index, u32_len), byte_len) = issuer_sec_key.map
                                                     .get(attr_key)
                                                     .ok_or(ZeiError::ParameterError)?; // A.2 field is contained in secret key
                                                                                        // C. check that attribute length matches secret key parameters
    if attr_value.len() != *byte_len {
      return Err(ZeiError::ParameterError);
    }
    let u32_attrs = u8_slice_to_u32_vec(attr_value, *u32_len); // attr_to_u32_array(*attr, *len);
    attrs[*index..*index + *u32_len].clone_from_slice(&u32_attrs);
  }
  ac_sign(prng,
          &issuer_sec_key.ac_sec_key,
          &user_pub_key.get_ref(),
          &attrs)
}

pub fn credential_keygen_commitment<R: CryptoRng + RngCore>(prng: &mut R) -> CredCommitmentKey {
  ac_keygen_commitment(prng)
}

pub fn credential_commit<R: CryptoRng + RngCore>(
  prng: &mut R,
  user_sec_key: &CredUserSecretKey,
  credential: &Credential,
  msg: &[u8])
  -> Result<(CredCommitment, CredPoK, CredCommitmentKey), ZeiError> {
  let ac_credential = credential.to_ac_credential()?;
  ac_commit(prng, user_sec_key.get_ref(), &ac_credential, msg)
}

pub fn credential_commit_with_key<R: CryptoRng + RngCore>(
  prng: &mut R,
  user_sk: &CredUserSecretKey,
  credential: &Credential,
  key: &CredCommitmentKey,
  msg: &[u8])
  -> Result<(CredCommitment, CredPoK), ZeiError> {
  let ac_credential = credential.to_ac_credential()?;
  ac_commit_with_key(prng, user_sk.get_ref(), &ac_credential, key, msg)
}

pub fn credential_verify_commitment(issuer_pub_key: &CredIssuerPublicKey,
                                    sig_commitment: &CredCommitment,
                                    sok: &CredPoK,
                                    msg: &[u8])
                                    -> Result<(), ZeiError> {
  ac_verify_commitment(&issuer_pub_key.ac_pub_key, sig_commitment, sok, msg)
}

pub fn credential_open_commitment<R: CryptoRng + RngCore>(prng: &mut R,
                                                          user_sk: &CredUserSecretKey,
                                                          credential: &Credential,
                                                          key: &CredCommitmentKey,
                                                          reveal_fields: &[String])
                                                          -> Result<CredPoK, ZeiError> {
  let reveal_map = reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields)?;
  let ac_credential = credential.to_ac_credential()?;
  ac_open_commitment(prng, user_sk.get_ref(), &ac_credential, key, &reveal_map)
}

pub fn credential_reveal<R: CryptoRng + RngCore>(prng: &mut R,
                                                 user_sk: &CredUserSecretKey,
                                                 credential: &Credential,
                                                 reveal_fields: &[String])
                                                 -> Result<CredRevealSig, ZeiError> {
  let reveal_map = reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields)?;
  let ac_credential = credential.to_ac_credential()?;
  ac_reveal(prng, user_sk.get_ref(), &ac_credential, &reveal_map)
}

pub fn credential_verify(issuer_pub_key: &CredIssuerPublicKey,
                         attrs: &[(String, &[u8])],
                         sig_commitment: &CredCommitment,
                         reveal_proof: &ACRevealProof)
                         -> Result<(), ZeiError> {
  let mut u32_attrs = vec![None; issuer_pub_key.num_internal_attrs];
  for (field, attr) in attrs {
    let ((pos, len), _byte_len) = issuer_pub_key.map
                                                .get(field)
                                                .ok_or(ZeiError::ParameterError)?;
    let u32_vec = u8_slice_to_u32_vec(attr, *len);
    let u32_vec_option: Vec<Option<u32>> = u32_vec.iter().map(|x| Some(*x)).collect();
    u32_attrs[*pos..pos + len].clone_from_slice(&u32_vec_option);
  }
  ac_verify(&issuer_pub_key.ac_pub_key,
            &u32_attrs,
            sig_commitment,
            reveal_proof)
}

fn reveal_field_to_bitmap(issuer_pub_key: &CredIssuerPublicKey,
                          reveal_fields: &[String])
                          -> Result<Vec<bool>, ZeiError> {
  // 1. check fields are in public key
  for field in reveal_fields {
    if !issuer_pub_key.map.contains_key(field) {
      return Err(ZeiError::ParameterError);
    }
  }
  let mut reveal_map = vec![false; issuer_pub_key.num_internal_attrs];
  for (key, ((pos, len), _byte_len)) in &issuer_pub_key.map {
    let b = reveal_fields.contains(key);
    for x in reveal_map[*pos..pos + len].iter_mut() {
      *x = b
    }
  }
  Ok(reveal_map)
}

// How many u32 are required to hold num_u8 u8s?
fn num_u32_per_u8(num_u8: usize) -> usize {
  if num_u8 == 0 {
    0usize
  } else {
    1 + (num_u8 - 1) / 4
  }
}

/* Use the contents of u8 slice to fill a vector of u32 */
fn u8_slice_to_u32_vec(attr: &[u8], len: usize) -> Vec<u32> {
  assert!(len >= num_u32_per_u8(attr.len()));
  let mut res = vec![0u32; len];
  for (i, byte) in attr.iter().enumerate() {
    res[i / 4] |= (*byte as u32) << (8 * (i as u32 % 4));
  }
  res
}
