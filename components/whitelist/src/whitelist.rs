#![deny(warnings)]
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::AssetTypeCode;
use ledger::error_location;
use serde::{Deserialize, Serialize};
use zei::crypto::whitelist::{prove_array_membership, verify_array_membership, WhitelistProof};
use zei::xfr::structs::asset_type_to_scalar;

/// Code of whitelisted assets
pub type WhiteListedCode = Scalar;

/// Asset whitelist
#[derive(Default, Deserialize, Serialize, Debug)]
pub struct Whitelist {
  /// List of whitelisted asset codes
  // TODO (Keyao): Redmine issue #45: Allow storing whitelist members in a Merkle tree
  pub members: Vec<WhiteListedCode>,
}

impl Whitelist {
  // Adds an asset code to the whitelist
  pub fn add_member(&mut self, code: AssetTypeCode) {
    self.members.push(asset_type_to_scalar(&code.val));
  }

  /// Proves the whitelist membership of a confidential asset transferred in a transaction.
  /// # Arguments
  /// * `index`: index in the whitelist.
  /// * `commitment`: asset type commitment.
  /// * `blind`: blinding factor for the asset type commitment.
  pub fn prove_membership(&mut self,
                          index: u64,
                          commitment: CompressedRistretto,
                          blind: Scalar)
                          -> Result<WhitelistProof, PlatformError> {
    prove_array_membership(&self.members, index as usize, &commitment, &blind).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }

  /// Verifies the whitelist membership of a confidential asset transferred in a transaction.
  /// # Arguments
  /// * `index`: index in the whitelist.
  /// * `commitment`: asset type commitment.
  /// * `proof`: whitelist proof.
  pub fn verify_membership(&mut self,
                           commitment: CompressedRistretto,
                           proof: WhitelistProof)
                           -> Result<(), PlatformError> {
    verify_array_membership(&self.members, &commitment, &proof).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ledger::data_model::AssetRules;
  use network::LedgerStandalone;
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;
  use txn_cli::txn_lib::{
    define_issue_transfer_and_get_utxo_and_blinds, query_utxo_and_get_type_commitment,
  };
  use zei::xfr::asset_record::AssetRecordType;
  use zei::xfr::sig::XfrKeyPair;

  #[should_panic(expected = "assertion failed: com_elem == *elem")]
  #[test]
  fn test_prove_membership_incorrect_index() {
    // Start the standalone ledger
    let mut ledger_standalone = LedgerStandalone::new_mock(1);

    // Generate asset codes and add codes to the whitelist
    let codes = vec![AssetTypeCode::gen_random(), AssetTypeCode::gen_random()];
    let whitelist = &mut Whitelist::default();
    for code in &codes {
      whitelist.add_member(*code);
    }

    // Transfer the second asset, and get the UTXO SID, asset type blind, and the asset type commitment
    let prng = &mut ChaChaRng::from_entropy();
    let (utxo_1, _, blind_1) = define_issue_transfer_and_get_utxo_and_blinds(&XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      &XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      10,
                                                                      codes[1],
                                                                      AssetRules::default(),
                                                                      AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                                      &mut ledger_standalone,
                                                                      prng).unwrap();
    let commitment_1 = query_utxo_and_get_type_commitment(utxo_1, &ledger_standalone).unwrap();

    // Prove the whitelist memberships of the second asset with the incorrect index
    // Should panic
    whitelist.prove_membership(0, commitment_1, blind_1)
             .unwrap();
  }

  #[should_panic(expected = "assertion failed: com_elem == *elem")]
  #[test]
  fn test_prove_membership_incorrect_utxo() {
    // Start the standalone ledger
    let mut ledger_standalone = LedgerStandalone::new_mock(1);

    // Generate asset codes and add codes to the whitelist
    let codes = vec![AssetTypeCode::gen_random(), AssetTypeCode::gen_random()];
    let whitelist = &mut Whitelist::default();
    for code in &codes {
      whitelist.add_member(*code);
    }

    // Transfer the assets
    // Get the UTXO SID and asset type commitment of the first asset, and asset type blind of the second asset
    let prng = &mut ChaChaRng::from_entropy();
    let (utxo_0, _, _) = define_issue_transfer_and_get_utxo_and_blinds(&XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
    &XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
    10,
    codes[0],
    AssetRules::default(),
    AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
    &mut ledger_standalone,
    prng).unwrap();
    let commitment_0 = query_utxo_and_get_type_commitment(utxo_0, &ledger_standalone).unwrap();
    let (_, _, blind_1) = define_issue_transfer_and_get_utxo_and_blinds(&XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      &XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      10,
                                                                      codes[1],
                                                                      AssetRules::default(),
                                                                      AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                                      &mut ledger_standalone,
                                                                      prng).unwrap();

    // Prove the whitelist memberships of the second asset with the incorrect UTXO SID
    // Should panic
    whitelist.prove_membership(1, commitment_0, blind_1)
             .unwrap();
  }

  #[should_panic(expected = "assertion failed: com_elem == *elem")]
  #[test]
  fn test_prove_membership_incorrect_blind() {
    // Start the standalone ledger
    let mut ledger_standalone = LedgerStandalone::new_mock(1);

    // Generate asset codes and add codes to the whitelist
    let codes = vec![AssetTypeCode::gen_random(), AssetTypeCode::gen_random()];
    let whitelist = &mut Whitelist::default();
    for code in &codes {
      whitelist.add_member(*code);
    }

    // Transfer assets
    // Get the UTXO SID of the first asset, and asset type blind and commitment of the second asset
    let prng = &mut ChaChaRng::from_entropy();
    let (_, _, blind_0) = define_issue_transfer_and_get_utxo_and_blinds(&XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
    &XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
    10,
    codes[0],
    AssetRules::default(),
    AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
    &mut ledger_standalone,
    prng).unwrap();
    let (utxo_1, _, _) = define_issue_transfer_and_get_utxo_and_blinds(&XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      &XfrKeyPair::generate(&mut ChaChaRng::from_entropy()),
                                                                      10,
                                                                      codes[1],
                                                                      AssetRules::default(),
                                                                      AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                                      &mut ledger_standalone,
                                                                      prng).unwrap();
    let commitment_1 = query_utxo_and_get_type_commitment(utxo_1, &ledger_standalone).unwrap();

    // Prove the whitelist memberships of the second asset with the incorrect UTXO SID
    // Should panic
    whitelist.prove_membership(1, commitment_1, blind_0)
             .unwrap();
  }

  #[test]
  fn test_prove_and_verify_membership() {
    // Start the standalone ledger
    let mut ledger_standalone = LedgerStandalone::new_mock(1);

    // Generate key pair and asset codes
    let issuer_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let recipient_key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = vec![AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random()];

    // Add codes to the whitelist
    let whitelist = &mut Whitelist::default();
    for code in &codes {
      whitelist.add_member(*code);
    }
    assert_eq!(whitelist.members.len(), 5);

    // Transfer the second and third assets
    // Get the UTXO SIDs, asset type blinds, and asset type commitments
    let prng = &mut ChaChaRng::from_entropy();
    let (utxo_1, _, blind_1) = define_issue_transfer_and_get_utxo_and_blinds(&issuer_key_pair,
                                                                     &recipient_key_pair,
                                                                     10,
                                                                     codes[1],
                                                                     AssetRules::default(),
                                                                     AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                                     &mut ledger_standalone,
                                                                     prng).unwrap();
    let commitment_1 = query_utxo_and_get_type_commitment(utxo_1, &ledger_standalone).unwrap();

    let (utxo_2, _, blind_2) = define_issue_transfer_and_get_utxo_and_blinds(&issuer_key_pair,
                                                                      &recipient_key_pair,
                                                                      10,
                                                                      codes[2],
                                                                      AssetRules::default(),
                                                                      AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
                                                                      &mut ledger_standalone,
                                                                      prng).unwrap();
    let commitment_2 = query_utxo_and_get_type_commitment(utxo_2, &ledger_standalone).unwrap();

    // Prove the whitelist memberships of the second and third assets
    let proof_1 = whitelist.prove_membership(1, commitment_1, blind_1);
    let proof_2 = whitelist.prove_membership(2, commitment_2, blind_2);
    assert!(proof_1.is_ok());
    assert!(proof_2.is_ok());

    // Verify the whitelist memberships of the second and third assets
    assert!(whitelist.verify_membership(commitment_1, proof_1.unwrap())
                     .is_ok());
    assert!(whitelist.verify_membership(commitment_2, proof_2.unwrap())
                     .is_ok());
  }
}
