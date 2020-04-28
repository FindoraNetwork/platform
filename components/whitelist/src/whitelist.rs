#![deny(warnings)]
use bulletproofs::PedersenGens;
use curve25519_dalek::ristretto::CompressedRistretto;
use curve25519_dalek::scalar::Scalar;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetRules, AssetTypeCode, TransferType, TxOutput, TxoRef};
use ledger::error_location;
use ledger_standalone::LedgerStandalone;
use rand_chacha::ChaChaRng;
use std::collections::HashMap;
use txn_builder::{BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder};
use txn_cli::txn_lib::query;
use zei::crypto::whitelist::{prove_array_membership, verify_array_membership, WhitelistProof};
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{asset_type_to_scalar, AssetRecordTemplate, BlindAssetRecord};

const PROTOCOL: &str = "http";
const HOST: &str = "localhost";
const QUERY_PORT: &str = "8668";

// Code of whitelisted assets
pub type WhiteListedCode = Scalar;

// Queries the UTXO SID and get the asset type commitment
pub fn query_utxo_and_get_commitment(utxo: u64,
                                     protocol: &str,
                                     host: &str)
                                     -> Result<CompressedRistretto, PlatformError> {
  let res = query(protocol, host, QUERY_PORT, "utxo_sid", &format!("{}", utxo))?;
  let blind_asset_record =
    serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                    Err(PlatformError::DeserializationError)
                                                  })?;
  let asset_type = blind_asset_record.asset_type;
  match asset_type.get_commitment() {
    Some(c) => Ok(c),
    None => {
      println!("Found nonconfidential asset. Asset should be confidential to be added to the whitelist.");
      Err(PlatformError::InputsError(error_location!()))
    }
  }
}

/// Asset whitelist
#[derive(Default)]
pub struct Whitelist {
  /// List of whitelisted asset codes
  // TODO (Keyao): Make this a merkle tree instead?
  pub members: Vec<WhiteListedCode>,

  /// Map from asset type commitments (as bytes of CompressedRistretto), to their associated whitelist proofs
  pub commitments_and_proofs: HashMap<[u8; 32], WhitelistProof>,
}

impl Whitelist {
  // Adds an asset code to the whitelist
  pub fn add_member(&mut self, code: AssetTypeCode) {
    self.members.push(asset_type_to_scalar(&code.val));
  }

  /// Proves whitelist membership of a confidential asset transferred in a transaction, and stores the proof.
  /// Must be used before `verify_membership`.
  /// # Arguments
  /// * `index`: index in the whitelist.
  /// * `utxo`: UTXO SID of the transaction.
  /// * `blind`: blinding factor for the asset type commitment.
  pub fn prove_membership_and_store(&mut self,
                                    index: u64,
                                    utxo: u64,
                                    blind: Scalar)
                                    -> Result<(), PlatformError> {
    let commitment = query_utxo_and_get_commitment(utxo, PROTOCOL, HOST)?;
    let proof = prove_array_membership(&self.members, index as usize, &commitment, &blind).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))?;
    self.commitments_and_proofs
        .insert(commitment.to_bytes(), proof);
    Ok(())
  }

  /// Verify the whitelist membership of an asset transferred in a transaction.
  /// Must not be used before `prove_membership`.
  /// # Arguments
  /// * `sid`: SID of the transaction.
  pub fn verify_membership(&self, utxo: u64) -> Result<(), PlatformError> {
    let commitment = query_utxo_and_get_commitment(utxo, PROTOCOL, HOST)?;
    let proof = match self.commitments_and_proofs.get(commitment.as_bytes()) {
      Some(p) => p,
      None => {
        println!("Whitelist proof not found.");
        return Err(PlatformError::InputsError(error_location!()));
      }
    };
    verify_array_membership(&self.members, &commitment, &proof).or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))
  }
}

/// For unit testing: issues and transfers a confidential asset, submits the transactions, and get the UTXO SID and asset type blind.
fn test_issue_transfer_and_get_utxo_and_blind(key_pair: &XfrKeyPair,
                                              code: AssetTypeCode,
                                              ledger_standalone: &LedgerStandalone,
                                              mut prng: &mut ChaChaRng)
                                              -> Result<(u64, Scalar), PlatformError> {
  // Issue and transfer the asset
  let pc_gens = PedersenGens::default();
  let amount = 10;
  let issue_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, key_pair.get_pk());
  let issue_blind_asset_record =
    build_blind_asset_record(&mut prng, &pc_gens, &issue_template, None).0;
  let transfer_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_ConfidentialAssetType, key_pair.get_pk());
  let mut txn_builder = TransferOperationBuilder::new();
  txn_builder.add_input(TxoRef::Relative(0),
    open_blind_asset_record(&issue_blind_asset_record,
                      &None,
                      key_pair.get_sk_ref())
    .map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
    amount)?;
  let blind = txn_builder.add_output_and_get_type_blind(&transfer_template, None, prng)?;
  let xfr_op = txn_builder.balance()?
                          .create(TransferType::Standard)?
                          .sign(key_pair)?
                          .transaction()?;

  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_issue_asset(key_pair,
                                                  &code,
                                                  1,
                                                  &[(TxOutput(issue_blind_asset_record), None)],
                                                  None)?
                       .add_operation(xfr_op)
                       .transaction();

  // Submit the transaction and get the UTXO
  Ok((ledger_standalone.submit_transaction_and_fetch_utxos(&txn)[0].0, blind))
}

/// For unit testing: defines, issues and transfers a confidential asset, submits the transactions, and get the UTXO SID and asset type blind.
pub fn test_init_asset_and_get_utxo_and_blind(key_pair: &XfrKeyPair,
                                              code: AssetTypeCode,
                                              ledger_standalone: &LedgerStandalone,
                                              prng: &mut ChaChaRng)
                                              -> Result<(u64, Scalar), PlatformError> {
  // Define the asset
  let mut txn_builder = TransactionBuilder::default();
  let txn = txn_builder.add_operation_create_asset(key_pair,
                                                   Some(code),
                                                   AssetRules::default(),
                                                   "",
                                                   PolicyChoice::Fungible())?
                       .transaction();
  ledger_standalone.submit_transaction(&txn);

  // Issues and transfers the asset, and get the UTXO SID and asset type blind.
  test_issue_transfer_and_get_utxo_and_blind(key_pair, code, ledger_standalone, prng)
}

#[cfg(test)]
mod tests {
  use super::*;
  use rand_core::SeedableRng;

  // TODO (Keyao): Add negative tests

  // Ignoring this test as it fails due to the below validation in ledger/src/store/effects.rs:
  //
  // if let Some(out_code) = out.asset_type.get_asset_type() {
  //   asset_types_involved.insert(AssetTypeCode { val: out_code });
  // }
  //
  // To test the functionalities of whitelist proof:
  // * Comment out the validation in ledger/src/store/effects.rs
  // * Run this test with -- --ignored
  // * Verify the test result
  // * Restore the validation in ledger/src/store/effects.rs
  #[test]
  #[ignore]
  fn test_prove_and_verify_membership() {
    // Start the standalone ledger
    let ledger_standalone = &LedgerStandalone::new();
    ledger_standalone.poll_until_ready().unwrap();

    // Generate key pair and asset codes
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let codes = vec![AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random(),
                     AssetTypeCode::gen_random()];

    // Add codes to the whitelist
    let mut whitelist = Whitelist::default();
    for code in &codes {
      whitelist.add_member(*code);
    }
    assert_eq!(whitelist.members.len(), 5);

    // Transfer the third asset and get the UTXO SID
    let prng = &mut ChaChaRng::from_entropy();
    let (utxo, blind) =
      test_init_asset_and_get_utxo_and_blind(&key_pair, codes[2], ledger_standalone, prng).unwrap();

    // Proves the whitelist membership of the second asset
    assert!(whitelist.prove_membership_and_store(2, utxo, blind).is_ok());

    // Verify the whitelist membership
    assert!(whitelist.verify_membership(utxo).is_ok());
  }
}
