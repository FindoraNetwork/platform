//!
//! # A Credentials Implementation
//!

#![deny(warnings)]
#![deny(missing_docs)]

use {
    linear_map::LinearMap,
    rand_core::{CryptoRng, RngCore},
    ruc::*,
    serde::{Deserialize, Serialize},
    wasm_bindgen::prelude::*,
    noah::{
        anon_creds::{
            ac_commit, ac_commit_with_key, ac_keygen_commitment, ac_keygen_issuer,
            ac_keygen_user, ac_open_commitment, ac_reveal, ac_sign, ac_verify,
            ac_verify_commitment, ACCommitment, ACCommitmentKey, ACIssuerPublicKey,
            ACIssuerSecretKey, ACPoK, ACRevealProof, ACRevealSig, ACSignature,
            ACUserPublicKey, ACUserSecretKey, Credential as NoahCredential,
        },
        errors::NoahError,
    },
};

/// Secret key of a credential issuer.
#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerSecretKey {
    ac_sec_key: ACIssuerSecretKey,
    map: LinearMap<String, ((usize, usize), usize)>, // key: (index, len in u32), len in bytes
    num_internal_attrs: usize,
}

/// Public key of a credential issuer.
#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredIssuerPublicKey {
    ac_pub_key: ACIssuerPublicKey,
    map: LinearMap<String, ((usize, usize), usize)>, // key: (index, len in u32), len in bytes
    num_internal_attrs: usize,
}

#[allow(missing_docs)]
pub type CredSignature = ACSignature;
#[allow(missing_docs)]
pub type CredCommitmentKey = ACCommitmentKey;
#[allow(missing_docs)]
pub type CredCommitment = ACCommitment;

/// Public key of a credential user.
#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredUserPublicKey(ACUserPublicKey);

/// Secret key of a credential user.
#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct CredUserSecretKey(ACUserSecretKey);

impl CredIssuerPublicKey {
    /// Return a reference of inner data structure
    #[inline(always)]
    pub fn get_ref(&self) -> &ACIssuerPublicKey {
        &self.ac_pub_key
    }

    /// Calculate data size
    #[inline(always)]
    pub fn get_len(&self, key: &str) -> Result<usize> {
        match self.map.get(key) {
            Some(((_, len), _)) => Ok(*len),
            None => Err(eg!(NoahError::ParameterError)),
        }
    }
}

impl CredIssuerSecretKey {
    /// Return a reference of inner data structure
    #[inline(always)]
    pub fn get_ref(&self) -> &ACIssuerSecretKey {
        &self.ac_sec_key
    }
}

impl CredUserPublicKey {
    /// Return a reference of inner data structure
    #[inline(always)]
    pub fn get_ref(&self) -> &ACUserPublicKey {
        &self.0
    }
}

impl CredUserSecretKey {
    /// Return a reference of inner data structure
    #[inline(always)]
    pub fn get_ref(&self) -> &ACUserSecretKey {
        &self.0
    }
}

#[allow(missing_docs)]
#[derive(Clone, Serialize, Deserialize)]
pub struct Credential {
    pub attributes: Vec<(String, Vec<u8>)>,
    pub issuer_pub_key: CredIssuerPublicKey,
    pub signature: CredSignature,
}

impl Credential {
    #[allow(missing_docs)]
    pub fn to_ac_credential(&self) -> Result<NoahCredential> {
        let mut u32_attrs = vec![0u32; self.issuer_pub_key.num_internal_attrs];
        for (key, attr) in &self.attributes {
            let ((pos, len), _) = self
                .issuer_pub_key
                .map
                .get(key)
                .c(d!(NoahError::ParameterError))?;
            let u32_vec = u8_slice_to_u32_vec(attr, *len);
            u32_attrs[*pos..*pos + *len].clone_from_slice(&u32_vec);
        }
        Ok(NoahCredential {
            sig: self.signature.clone(),
            attrs: u32_attrs,
            ipk: self.issuer_pub_key.ac_pub_key.clone(),
        })
    }
}
#[allow(missing_docs)]
pub type CredPoK = ACPoK;
#[allow(missing_docs)]
pub type CredRevealSig = ACRevealSig;
#[allow(missing_docs)]
pub type CredRevealProof = ACRevealProof;

#[allow(missing_docs)]
pub fn credential_issuer_key_gen<R: CryptoRng + RngCore>(
    prng: &mut R,
    attributes: &[(String, usize)],
) -> (CredIssuerPublicKey, CredIssuerSecretKey) {
    let mut map = LinearMap::new();
    let mut num_attrs = 0usize;
    for (key, len) in attributes {
        let num_sub_attr = num_u32_per_u8(*len);
        map.insert(key.clone(), ((num_attrs, num_sub_attr), *len));
        num_attrs += num_sub_attr;
    }
    let (issuer_sec_key, issuer_pub_key) = ac_keygen_issuer(prng, num_attrs);

    (
        CredIssuerPublicKey {
            ac_pub_key: issuer_pub_key,
            map: map.clone(),
            num_internal_attrs: num_attrs,
        },
        CredIssuerSecretKey {
            ac_sec_key: issuer_sec_key,
            map,
            num_internal_attrs: num_attrs,
        },
    )
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_user_key_gen<R: CryptoRng + RngCore>(
    prng: &mut R,
    issuer_pub_key: &CredIssuerPublicKey,
) -> (CredUserPublicKey, CredUserSecretKey) {
    let (sk, pk) = ac_keygen_user(prng, &issuer_pub_key.ac_pub_key);
    (CredUserPublicKey(pk), CredUserSecretKey(sk))
}

#[allow(missing_docs)]
pub fn credential_sign<R: CryptoRng + RngCore>(
    prng: &mut R,
    issuer_sec_key: &CredIssuerSecretKey,
    user_pub_key: &CredUserPublicKey,
    attributes: &[(String, &[u8])],
) -> Result<CredSignature> {
    // A. check that attributes fields to sign matches the attribute list
    // A.1 lengths matches
    let keys = attributes.iter().map(|(x, _)| x);
    if keys.len() != issuer_sec_key.map.len() {
        return Err(eg!(NoahError::ParameterError));
    }

    // B build list of u32 parameters
    let n_u32_attrs = issuer_sec_key.num_internal_attrs;
    let mut attrs = vec![0u32; n_u32_attrs];
    for (attr_key, attr_value) in attributes {
        let ((index, u32_len), byte_len) = issuer_sec_key
            .map
            .get(attr_key)
            .c(d!(NoahError::ParameterError))?; // A.2 field is contained in secret key
                                               // C. check that attribute length matches secret key parameters
        if attr_value.len() != *byte_len {
            return Err(eg!(NoahError::ParameterError));
        }

        let u32_attrs = u8_slice_to_u32_vec(attr_value, *u32_len); // attr_to_u32_array(*attr, *len);
        attrs[*index..*index + *u32_len].clone_from_slice(&u32_attrs);
    }
    ac_sign(
        prng,
        &issuer_sec_key.ac_sec_key,
        user_pub_key.get_ref(),
        &attrs,
    )
    .c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_keygen_commitment<R: CryptoRng + RngCore>(
    prng: &mut R,
) -> CredCommitmentKey {
    ac_keygen_commitment(prng)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_commit<R: CryptoRng + RngCore>(
    prng: &mut R,
    user_sec_key: &CredUserSecretKey,
    credential: &Credential,
    msg: &[u8],
) -> Result<(CredCommitment, CredPoK, CredCommitmentKey)> {
    let ac_credential = credential.to_ac_credential().c(d!())?;
    match ac_commit(prng, user_sec_key.get_ref(), &ac_credential, msg).c(d!())? {
        (acc, acp, Some(ack)) => Ok((acc, acp, ack)),
        _ => Err(eg!(NoahError::ParameterError)),
    }
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_commit_with_key<R: CryptoRng + RngCore>(
    prng: &mut R,
    user_sk: &CredUserSecretKey,
    credential: &Credential,
    key: &CredCommitmentKey,
    msg: &[u8],
) -> Result<(CredCommitment, CredPoK)> {
    let ac_credential = credential.to_ac_credential().c(d!())?;
    ac_commit_with_key(prng, user_sk.get_ref(), &ac_credential, key, msg)
        .c(d!())
        .map(|(acc, acp, _)| (acc, acp))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_verify_commitment(
    issuer_pub_key: &CredIssuerPublicKey,
    sig_commitment: &CredCommitment,
    sok: &CredPoK,
    msg: &[u8],
) -> Result<()> {
    ac_verify_commitment(&issuer_pub_key.ac_pub_key, sig_commitment, sok, msg).c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_open_commitment<R: CryptoRng + RngCore>(
    prng: &mut R,
    user_sk: &CredUserSecretKey,
    credential: &Credential,
    key: &CredCommitmentKey,
    reveal_fields: &[String],
) -> Result<CredPoK> {
    let reveal_map =
        reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields).c(d!())?;
    let ac_credential = credential.to_ac_credential().c(d!())?;
    ac_open_commitment(prng, user_sk.get_ref(), &ac_credential, key, &reveal_map).c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn credential_reveal<R: CryptoRng + RngCore>(
    prng: &mut R,
    user_sk: &CredUserSecretKey,
    credential: &Credential,
    reveal_fields: &[String],
) -> Result<CredRevealSig> {
    let reveal_map =
        reveal_field_to_bitmap(&credential.issuer_pub_key, reveal_fields).c(d!())?;
    let ac_credential = credential.to_ac_credential().c(d!())?;
    ac_reveal(prng, user_sk.get_ref(), &ac_credential, &reveal_map).c(d!())
}

#[allow(missing_docs)]
pub fn credential_verify(
    issuer_pub_key: &CredIssuerPublicKey,
    attrs: &[(String, &[u8])],
    sig_commitment: &CredCommitment,
    reveal_proof: &ACRevealProof,
) -> Result<()> {
    let mut u32_attrs = vec![None; issuer_pub_key.num_internal_attrs];
    for (field, attr) in attrs {
        let ((pos, len), _byte_len) = issuer_pub_key
            .map
            .get(field)
            .c(d!(NoahError::ParameterError))?;
        let u32_vec = u8_slice_to_u32_vec(attr, *len);
        let u32_vec_option: Vec<Option<u32>> =
            u32_vec.iter().map(|x| Some(*x)).collect();
        u32_attrs[*pos..pos + len].clone_from_slice(&u32_vec_option);
    }
    ac_verify(
        &issuer_pub_key.ac_pub_key,
        &u32_attrs,
        sig_commitment,
        reveal_proof,
    )
    .c(d!())
}

fn reveal_field_to_bitmap(
    issuer_pub_key: &CredIssuerPublicKey,
    reveal_fields: &[String],
) -> Result<Vec<bool>> {
    // 1. check fields are in public key
    for field in reveal_fields {
        if !issuer_pub_key.map.contains_key(field) {
            return Err(eg!(NoahError::ParameterError));
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
#[inline(always)]
fn num_u32_per_u8(num_u8: usize) -> usize {
    if num_u8 == 0 {
        0usize
    } else {
        1 + (num_u8 - 1) / 4
    }
}

// Use the contents of u8 slice to fill a vector of u32
#[inline(always)]
fn u8_slice_to_u32_vec(attr: &[u8], len: usize) -> Vec<u32> {
    let mut res = vec![0u32; len];
    for (i, byte) in attr.iter().enumerate() {
        res[i / 4] |= (*byte as u32) << (8 * (i as u32 % 4));
    }
    res
}
