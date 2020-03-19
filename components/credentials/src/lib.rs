use linear_map::LinearMap;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use zei::api::anon_creds::{
  ac_commit, ac_commit_with_key, ac_keygen_commitment, ac_keygen_issuer, ac_keygen_user,
  ac_open_commitment, ac_reveal, ac_sign, ac_verify, ac_verify_commitment, ACCommitment,
  ACCommitmentKey, ACIssuerPublicKey, ACIssuerSecretKey, ACPoK, ACRevealProof, ACRevealSig,
  ACSignature, ACUserPublicKey, ACUserSecretKey, Credential as ZeiCredential,
};
use zei::errors::ZeiError;

#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerSecretKey {
  ac_sec_key: ACIssuerSecretKey,
  map: LinearMap<String, (usize, usize)>,
  num_attrs: usize,
}

#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerPublicKey {
  ac_pub_key: ACIssuerPublicKey,
  map: LinearMap<String, (usize, usize)>,
  num_attrs: usize,
}

pub type CredUserPublicKey = ACUserPublicKey;
pub type CredUserSecretKey = ACUserSecretKey;
pub type CredSignature = ACSignature;
pub type CredCommitmentKey = ACCommitmentKey;
pub type CredCommitment = ACCommitment;
pub struct Credential<'a> {
  attributes: LinearMap<String, &'a [u8]>,
  issuer_pub_key: CredIssuerPublicKey,
  signature: CredSignature,
}

impl<'a> Credential<'a> {
  pub fn to_ac_credential(&self) -> Result<ZeiCredential, ZeiError> {
    let mut u32_attrs = vec![0u32; self.issuer_pub_key.num_attrs];
    for (key, attr) in &self.attributes {
      let (pos, len) = self.issuer_pub_key
                           .map
                           .get(key)
                           .ok_or(ZeiError::ParameterError)?;
      let u32_vec = attr_to_u32_array(*attr, *len);
      for (i, u32_attr) in u32_vec.iter().enumerate() {
        u32_attrs[pos + i] = *u32_attr;
      }
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
    let mut num_sub_attr = len / 4;
    if len % 4 != 0 {
      num_sub_attr += 1;
    }
    map.insert(key.clone(), (num_attrs, num_sub_attr));
    num_attrs += num_sub_attr;
  }
  let (issuer_pub_key, issuer_sec_key) = ac_keygen_issuer(prng, num_attrs);

  (CredIssuerPublicKey { ac_pub_key: issuer_pub_key,
    map: map.clone(),
    num_attrs },
    CredIssuerSecretKey { ac_sec_key: issuer_sec_key,
                         map,
                         num_attrs },
   )
}

pub fn credential_user_key_gen<R: CryptoRng + RngCore>(
  prng: &mut R,
  issuer_pub_key: &CredIssuerPublicKey)
  -> (CredUserPublicKey, CredUserSecretKey) {
  ac_keygen_user(prng, &issuer_pub_key.ac_pub_key)
}

pub fn credential_sign<R: CryptoRng + RngCore>(prng: &mut R,
                                               issuer_sec_key: &CredIssuerSecretKey,
                                               user_pub_key: &CredUserPublicKey,
                                               attributes: &[(String, &[u8])])
                                               -> Result<CredSignature, ZeiError> {
  let n_attrs = issuer_sec_key.num_attrs;
  let mut attrs = vec![0u32; n_attrs];
  for (key, attr) in attributes {
    let (index, len) = issuer_sec_key.map
                                     .get(key)
                                     .ok_or(ZeiError::ParameterError)?;
    let u32_attrs = attr_to_u32_array(*attr, *len);
    for (i, attr) in u32_attrs.iter().enumerate() {
      attrs[index + i] = *attr;
    }
  }
  ac_sign(prng, &issuer_sec_key.ac_sec_key, &user_pub_key, &attrs)
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
  ac_commit(prng, user_sec_key, &ac_credential, msg)
}

pub fn credential_commit_with_key<R: CryptoRng + RngCore>(
  prng: &mut R,
  user_sk: &CredUserSecretKey,
  credential: &Credential,
  key: &CredCommitmentKey,
  msg: &[u8])
  -> Result<(CredCommitment, CredPoK), ZeiError> {
  let ac_credential = credential.to_ac_credential()?;
  ac_commit_with_key(prng, user_sk, &ac_credential, key, msg)
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
  let reveal_map = reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields);
  let ac_credential = credential.to_ac_credential()?;
  ac_open_commitment(prng, user_sk, &ac_credential, key, &reveal_map)
}

pub fn credential_reveal<R: CryptoRng + RngCore>(prng: &mut R,
                                                 user_sk: &CredUserSecretKey,
                                                 credential: &Credential,
                                                 reveal_fields: &[String])
                                                 -> Result<CredRevealSig, ZeiError> {
  let reveal_map = reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields);
  let ac_credential = credential.to_ac_credential()?;
  ac_reveal(prng, user_sk, &ac_credential, &reveal_map)
}

pub fn credential_verify(issuer_pub_key: &CredIssuerPublicKey,
                         attrs: &[(String, &[u8])],
                         sig_commitment: &CredCommitment,
                         reveal_proof: &ACRevealProof)
                         -> Result<(), ZeiError> {
  let mut u32_attrs = vec![None; issuer_pub_key.num_attrs];
  for (field, attr) in attrs {
    let (pos, len) = issuer_pub_key.map
                                   .get(field)
                                   .ok_or(ZeiError::ParameterError)?;
    let u32_vec = attr_to_u32_array(*attr, *len);
    for (i, u32_attr) in u32_vec.iter().enumerate() {
      u32_attrs[pos + i] = Some(*u32_attr);
    }
  }
  ac_verify(&issuer_pub_key.ac_pub_key,
            &u32_attrs,
            sig_commitment,
            reveal_proof)
}

fn reveal_field_to_bitmap(issuer_pub_key: &CredIssuerPublicKey,
                          reveal_fields: &[String])
                          -> Vec<bool> {
  let mut reveal_map = vec![false; issuer_pub_key.num_attrs];
  for (key, (pos, len)) in &issuer_pub_key.map {
    let b = reveal_fields.contains(key);
    for i in *pos..*pos + *len {
      reveal_map[i] = b;
    }
  }
  reveal_map
}

fn attr_to_u32_array(attr: &[u8], len: usize) -> Vec<u32> {
  let mut i = 0;
  let mut r = vec![0u32; len];
  let mut k = 0;
  while i + 3 < attr.len() {
    r[k] = (attr[i] as u32)
           + ((attr[i + 1] as u32) << 8)
           + ((attr[i + 2] as u32) << 16)
           + ((attr[i + 3] as u32) << 24);
    i += 4;
    k += 1;
  }
  if i < attr.len() {
    let mut v = 0;
    let mut j = 0;
    while i < attr.len() {
      v += (attr[i] as u32) << (8 * j);
      j += 1;
      i += 1;
    }
    r[k] = v;
  }
  r
}
