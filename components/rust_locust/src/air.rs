// #![deny(warnings)]
use credentials::{
  credential_commit, credential_issuer_key_gen, credential_open_commitment, credential_sign,
  credential_user_key_gen, credential_verify, CredCommitment, CredCommitmentKey,
  CredIssuerPublicKey, CredIssuerSecretKey, CredPoK, CredSignature, CredUserPublicKey,
  CredUserSecretKey, Credential,
};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use txn_builder::{BuildsTransactions, TransactionBuilder};
// use utils::{protocol_host, urlencode, SUBMIT_PORT};
use pyo3::prelude::*;
use pyo3::exceptions;
use std::collections::HashMap;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrKeyPair;

// From txn_cli: need a working key pair String
const KEY_PAIR_STR: &str = "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc720fdbac9b10b7587bba7b5bc163bce69e796d71e4ed44c10fcb4488689f7a144";

/// Represents a file that can be searched
#[pyclass]
struct AIR {
  prng: ChaChaRng,
  issuer_pk: CredIssuerPublicKey,
  issuer_sk: CredIssuerSecretKey,
  user_commitment : HashMap<String, (CredCommitment, CredCommitmentKey)>,
}

#[pymethods]
impl AIR {
  #[new]
  fn new() -> Self {
    let mut prng =ChaChaRng::from_entropy();
    let attrs = vec![(String::from("dob"), 8),
                     (String::from("pob"), 3),
                     (String::from("sex"), 1)]; 
    let (issuer_pk, issuer_sk) = credential_issuer_key_gen(&mut prng, &attrs);
    AIR {
      prng,
      issuer_pk,
      issuer_sk,
      user_commitment: HashMap::new(),
    }
  }

  /// Return a JSON txn corresponding to this request
  pub fn make_assign_txn(&mut self) -> PyResult<String> {
    let (user_pk, user_sk) = credential_user_key_gen(&mut self.prng, &self.issuer_pk);
    let attr_vals: Vec<(String, &[u8])> = vec![(String::from("dob"), b"08221964"),
                                                (String::from("pob"), b"666"),
                                                (String::from("sex"), b"M")];
    let sig = credential_sign(&mut self.prng,
                              &self.issuer_sk,
                              &user_pk,
                              &attr_vals).unwrap();
    let credential = Credential { signature: sig.clone(),
                                  attributes: attr_vals.iter()
                                                       .map(|(k, v)| (k.clone(), v.to_vec()))
                                                       .collect(),
                                  issuer_pub_key: self.issuer_pk.clone() };
    let user_key_pair = XfrKeyPair::zei_from_bytes(&hex::decode(KEY_PAIR_STR).unwrap());

    let (commitment, proof, _key) =
      credential_commit(&mut self.prng, &user_sk, &credential, b"some addr").unwrap();
    let mut txn_builder = TransactionBuilder::default();
    if let Ok(_) = txn_builder.add_operation_air_assign(&user_key_pair,
                                                        user_pk,
                                                        commitment,
                                                        self.issuer_pk.clone(),
                                                        proof)
    {
      let txn = txn_builder.transaction();
      let result = serde_json::to_string(&txn).unwrap();
      Ok(result)
    } else {
      Err(exceptions::TypeError::py_err("add_operation_air_assign failed"))
    }
  }
}

#[pymodule]
pub fn air(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<AIR>()?;
    Ok(())
}
