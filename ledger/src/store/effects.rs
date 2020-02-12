#![deny(warnings)]
use crate::data_model::errors::PlatformError;
use crate::data_model::*;
use crate::policies::{compute_debt_swap_effect, DebtSwapEffect};
use crate::policy_script::run_txn_check;
use cryptohash::sha256;
use cryptohash::sha256::Digest as BitDigest;
use findora::HasInvariants;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use std::collections::{HashMap, HashSet};
use zei::serialization::ZeiFromToBytes;
use zei::xfr::lib::verify_xfr_body;
use zei::xfr::structs::BlindAssetRecord;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TxnEffect {
  // The Transaction object this represents
  pub txn: Transaction,
  // Internally-spent TXOs are None, UTXOs are Some(...)
  pub txos: Vec<Option<TxOutput>>,
  // Which TXOs this consumes
  pub input_txos: HashMap<TxoSID, BlindAssetRecord>,
  // Which new asset types this defines
  pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
  // Which new TXO issuance sequence numbers are used, in sorted order
  // The vec should be nonempty unless this asset code is being created in
  // this transaction.
  pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
  // Which public key is being used to issue each asset type
  pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
  // Debt swap information that must be externally validated
  pub debt_effects: HashMap<AssetTypeCode, DebtSwapEffect>,

  pub asset_types_involved: HashSet<AssetTypeCode>,
}

// Internally validates the transaction as well.
// If the transaction is invalid, it is dropped, so if you need to inspect
// the transaction in order to diagnose the error, clone it first!
impl TxnEffect {
  pub fn compute_effect<R: CryptoRng + RngCore>(prng: &mut R,
                                                txn: Transaction)
                                                -> Result<TxnEffect, PlatformError> {
    let mut txo_count: usize = 0;
    let mut txos: Vec<Option<TxOutput>> = Vec::new();
    let mut input_txos: HashMap<TxoSID, BlindAssetRecord> = HashMap::new();
    let mut new_asset_codes: HashMap<AssetTypeCode, AssetType> = HashMap::new();
    let mut new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>> = HashMap::new();
    let mut issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey> = HashMap::new();
    let mut debt_effects: HashMap<AssetTypeCode, DebtSwapEffect> = HashMap::new();
    let mut asset_types_involved = HashSet::<AssetTypeCode>::new();

    // Sequentially go through the operations, validating intrinsic or
    // local-to-the-transaction properties, then recording effects and
    // external properties.
    //
    // Incrementally recording operations in this way is necessary since
    // validity can depend upon earlier operations within a single
    // transaction (eg, a single transaction containing two Transfers which
    // consume the same TXO is invalid).
    //
    // This process should be a complete internal check of a transaction.
    // In particular, functions consuming a TxnEffect should be able to
    // assume that all internal consistency checks are valid, and that the
    // validity of the whole transaction now only depends on the
    // relationship between the outside world and the TxnEffect's fields
    // (eg, any input TXO SIDs of a Transfer should be recorded in
    // `input_txos` and that Transfer should be valid if all those TXO SIDs
    // exist unspent in the ledger and correspond to the correct
    // BlindAssetRecord).
    for op in txn.operations.iter() {
      assert!(txo_count == txos.len());

      match op {
        // An asset creation is valid iff:
        //     1) The signature is valid.
        //         - Fully checked here
        //     2) The token id is available.
        //         - Partially checked here
        //     3) The policy, if provided, passes its init check
        //         - Fully checked here
        Operation::DefineAsset(def) => {
          // (1)
          // TODO(joe?): like the note in data_model, should the public key
          // used here match `def.body.asset.issuer`?
          def.pubkey
             .key
             .verify(&serde_json::to_vec(&def.body).unwrap(), &def.signature)?;

          let code = def.body.asset.code;
          let token = AssetType { properties: def.body.asset.clone(),
                                  ..Default::default() };

          // (2), only within this transaction
          if new_asset_codes.contains_key(&code) || new_issuance_nums.contains_key(&code) {
            return Err(PlatformError::InputsError);
          }

          // (3)
          if let Some((ref pol, ref globals)) = def.body.asset.policy {
            let globals = globals.clone();
            run_txn_check(&pol.init_check,
                          globals.id_vars,
                          globals.rt_vars,
                          globals.amt_vars,
                          globals.frac_vars,
                          &Transaction::default())?;
          }

          issuance_keys.insert(code, token.properties.issuer.clone());
          new_asset_codes.insert(code, token);
          new_issuance_nums.insert(code, vec![]);
        }

        // The asset issuance is valid iff:
        //      1) The operation is unique (not a replay).
        //          - Partially checked here
        //      2) The signature is valid.
        //          - Fully checked here
        //      3) The signature belongs to the anchor (the issuer).
        //          - Either checked here or recorded in `new_issuance_keys`
        //      4) The assets in the TxOutputs are owned by the signatory.
        //          - Fully checked here
        //      5) The assets in the TxOutputs have a non-confidential
        //         asset type which agrees with the stated asset type.
        //          - Fully checked here
        //      TODO(joe): tracking!
        Operation::IssueAsset(iss) => {
          if iss.body.num_outputs != iss.body.records.len() {
            return Err(PlatformError::InputsError);
          }

          assert!(iss.body.num_outputs == iss.body.records.len());

          let code = iss.body.code;
          let seq_num = iss.body.seq_num;

          asset_types_involved.insert(code);

          // (1), within this transaction
          let iss_nums = new_issuance_nums.entry(code).or_insert_with(|| vec![]);

          if let Some(last_num) = iss_nums.last() {
            if seq_num <= *last_num {
              return Err(PlatformError::InputsError);
            }
          }
          iss_nums.push(seq_num);

          // (2)
          iss.pubkey
             .key
             .verify(&serde_json::to_vec(&iss.body).unwrap(), &iss.signature)?;

          // (3)
          if let Some(prior_key) = issuance_keys.get(&code) {
            if iss.pubkey != *prior_key {
              return Err(PlatformError::InputsError);
            }
          } else {
            issuance_keys.insert(code, iss.pubkey.clone());
          }

          txos.reserve(iss.body.records.len());
          for output in iss.body.records.iter() {
            // (4)
            if (output.0).public_key != iss.pubkey.key {
              return Err(PlatformError::InputsError);
            }

            // (5)
            if (output.0).asset_type != Some(code.val) {
              return Err(PlatformError::InputsError);
            }

            txos.push(Some(output.clone()));
            txo_count += 1;
          }
        }

        // An asset transfer is valid iff:
        //     1) The signatures on the body all are valid and there is a signature for each input
        //       key
        //          - Fully checked here
        //     2) The UTXOs (a) exist on the ledger and (b) match the zei transaction.
        //          - Partially checked here -- anything which hasn't
        //            been checked will appear in `input_txos`
        //     3) The zei transaction is valid.
        //          - Fully checked here
        Operation::TransferAsset(trn) => {
          if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
            return Err(PlatformError::InputsError);
          }
          if trn.body.num_outputs != trn.body.transfer.outputs.len() {
            return Err(PlatformError::InputsError);
          }
          assert!(trn.body.inputs.len() == trn.body.transfer.inputs.len());
          assert!(trn.body.num_outputs == trn.body.transfer.outputs.len());

          match trn.transfer_type {
            TransferType::DebtSwap => {
              let (debt_type, debt_swap_effect) = compute_debt_swap_effect(&trn.body.transfer)?;

              if debt_effects.contains_key(&debt_type) {
                return Err(PlatformError::InputsError);
              }
              debt_effects.insert(debt_type, debt_swap_effect);
            }
            TransferType::Standard => {
              // (1a) all body signatures are valid
              let mut sig_keys = HashSet::new();
              for sig in &trn.body_signatures {
                if !sig.verify(&serde_json::to_vec(&trn.body).unwrap()) {
                  return Err(PlatformError::InputsError);
                }
                sig_keys.insert(sig.address.key.zei_to_bytes());
              }

              // (1b) all input record owners have signed
              for record in &trn.body.transfer.inputs {
                if !sig_keys.contains(&record.public_key.zei_to_bytes()) {
                  return Err(PlatformError::InputsError);
                }
              }
            }
          }
          // (3)
          // TODO: implement real policies
          let null_policies = vec![];
          verify_xfr_body(prng, &trn.body.transfer, &null_policies)?;

          for (inp, record) in trn.body.inputs.iter().zip(trn.body.transfer.inputs.iter()) {
            if let Some(inp_code) = record.asset_type {
              asset_types_involved.insert(AssetTypeCode { val: inp_code });
            }

            // (2), checking within this transaction and recording
            // external UTXOs
            match *inp {
              TxoRef::Relative(offs) => {
                // (2).(a)
                if offs as usize >= txo_count {
                  return Err(PlatformError::InputsError);
                }
                let ix = (txo_count - 1) - (offs as usize);
                match &txos[ix] {
                  None => {
                    return Err(PlatformError::InputsError);
                  }
                  Some(TxOutput(inp_record)) => {
                    // (2).(b)
                    if inp_record != record {
                      return Err(PlatformError::InputsError);
                    }
                  }
                }
                txos[ix] = None;
              }
              TxoRef::Absolute(txo_sid) => {
                // (2).(a), partially
                if input_txos.contains_key(&txo_sid) {
                  return Err(PlatformError::InputsError);
                }

                input_txos.insert(txo_sid, record.clone());
              }
            }
          }

          txos.reserve(trn.body.transfer.outputs.len());
          for out in trn.body.transfer.outputs.iter() {
            if let Some(out_code) = out.asset_type {
              asset_types_involved.insert(AssetTypeCode { val: out_code });
            }
            txos.push(Some(TxOutput(out.clone())));
            txo_count += 1;
          }
        }
      }
    }

    Ok(TxnEffect { txn,
                   txos,
                   input_txos,
                   new_asset_codes,
                   new_issuance_nums,
                   issuance_keys,
                   debt_effects,
                   asset_types_involved })
  }
}

impl HasInvariants<PlatformError> for TxnEffect {
  fn fast_invariant_check(&self) -> Result<(), PlatformError> {
    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), PlatformError> {
    // Kinda messy, but the intention of this loop is to encode: For
    // every external input of a TxnEffect, there is exactly one
    // TransferAsset which consumes it.
    for (txo_sid, record) in self.input_txos.iter() {
      let mut found = false;
      for op in self.txn.operations.iter() {
        if let Operation::TransferAsset(trn) = op {
          if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
            return Err(PlatformError::InvariantError(None));
          }
          for (ix, inp_record) in trn.body.inputs.iter().zip(trn.body.transfer.inputs.iter()) {
            if let TxoRef::Absolute(input_tid) = ix {
              if input_tid == txo_sid {
                if inp_record != record {
                  return Err(PlatformError::InvariantError(None));
                }
                if found {
                  return Err(PlatformError::InvariantError(None));
                }
                found = true;
              }
            } else if inp_record == record {
              return Err(PlatformError::InvariantError(None));
            }
          }
        }
      }
      if !found {
        return Err(PlatformError::InvariantError(None));
      }
    }

    // TODO(joe): Every Utxo corresponds to exactly one TranferAsset or
    // IssueAsset, and does not appear in any inputs

    // TODO(joe): other checks?
    {
      // Slightly cheating
      let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);
      if TxnEffect::compute_effect(&mut prng, self.txn.clone())? != *self {
        return Err(PlatformError::InvariantError(None));
      }
    }

    Ok(())
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Default, Serialize)]
pub struct BlockEffect {
  // All Transaction objects validated in this block
  pub txns: Vec<Transaction>,
  // Identifiers within this block for each transaction
  // (currently just an index into `txns`)
  pub temp_sids: Vec<TxnTempSID>,
  // Internally-spent TXOs are None, UTXOs are Some(...)
  // Should line up element-wise with `txns`
  pub txos: Vec<Vec<Option<TxOutput>>>,
  // Which TXOs this consumes
  pub input_txos: HashMap<TxoSID, BlindAssetRecord>,
  // Which new asset types this defines
  pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
  // Which new TXO issuance sequence numbers are used, in sorted order
  // The vec should be nonempty unless this asset code is being created in
  // this transaction.
  pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
  // Which public key is being used to issue each asset type
  pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
}

impl BlockEffect {
  pub fn new() -> BlockEffect {
    Default::default()
  }

  // Combine a TxnEffect into this block.
  //
  // NOTE: this does not check the TxnEffect against the rest of the ledger
  // state, so each TxnEffect should be passed through
  // LedgerStatus::check_txn_effects *first*.
  //
  // Returns:
  //   if `txn` would not interfere with any transaction in the block, the
  //       new temp SID representing the transaction.
  //   Otherwise, Err(...)
  pub fn add_txn_effect(&mut self, txn: TxnEffect) -> Result<TxnTempSID, PlatformError> {
    // Check that no inputs are consumed twice
    for (input_sid, _) in txn.input_txos.iter() {
      if self.input_txos.contains_key(&input_sid) {
        return Err(PlatformError::InputsError);
      }
    }

    // Check that no AssetType is affected by both the block so far and
    // this transaction
    {
      for (type_code, _) in txn.new_asset_codes.iter() {
        if self.new_asset_codes.contains_key(&type_code)
           || self.new_issuance_nums.contains_key(&type_code)
        {
          return Err(PlatformError::InputsError);
        }
      }

      for (type_code, nums) in txn.new_issuance_nums.iter() {
        if self.new_asset_codes.contains_key(&type_code)
           || self.new_issuance_nums.contains_key(&type_code)
        {
          return Err(PlatformError::InputsError);
        }

        // Debug-check that issued assets are registered in `issuance_keys`
        if !nums.is_empty() {
          debug_assert!(txn.issuance_keys.contains_key(&type_code));
        }
      }
    }

    // == All validation done, apply `txn` to this block ==
    let temp_sid = TxnTempSID(self.txns.len());
    self.txns.push(txn.txn);
    self.temp_sids.push(temp_sid);
    self.txos.push(txn.txos);

    for (input_sid, record) in txn.input_txos {
      debug_assert!(!self.input_txos.contains_key(&input_sid));
      self.input_txos.insert(input_sid, record);
    }

    for (type_code, asset_type) in txn.new_asset_codes {
      debug_assert!(!self.new_asset_codes.contains_key(&type_code));
      self.new_asset_codes.insert(type_code, asset_type);
    }

    for (type_code, issuance_nums) in txn.new_issuance_nums {
      debug_assert!(!self.new_issuance_nums.contains_key(&type_code));
      self.new_issuance_nums.insert(type_code, issuance_nums);
    }

    for (type_code, issuer_key) in txn.issuance_keys {
      debug_assert!(!self.issuance_keys.contains_key(&type_code));
      self.issuance_keys.insert(type_code, issuer_key);
    }

    Ok(temp_sid)
  }

  pub fn compute_txns_in_block_hash(&self) -> BitDigest {
    let serialized = bincode::serialize(&self.txns).unwrap();

    sha256::hash(&serialized)
  }
}
