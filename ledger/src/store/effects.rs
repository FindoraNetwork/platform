#![deny(warnings)]
use crate::data_model::errors::PlatformError;
use crate::data_model::*;
use crate::policies::{compute_debt_swap_effect, DebtSwapEffect};
use crate::policy_script::{run_txn_check, TxnCheckInputs, TxnPolicyData};
use crate::{error_location, inp_fail, inv_fail, zei_fail};
use credentials::credential_verify_commitment;
use serde::Serialize;
use sparse_merkle_tree::Key;
use std::collections::{HashMap, HashSet};
use utils::{HasInvariants, HashOf, SignatureOf};
use zei::api::anon_creds::ACCommitment;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::{AssetTracingPolicy, BlindAssetRecord, XfrAmount, XfrAssetType, XfrBody};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TxnEffect {
  // The Transaction object this represents
  pub txn: Transaction,
  // Internally-spent TXOs are None, UTXOs are Some(...)
  pub txos: Vec<Option<TxOutput>>,
  // Which TXOs this consumes
  pub input_txos: HashMap<TxoSID, BlindAssetRecord>,
  // List of internally-spent TXOs. This does not include input txos;
  pub internally_spent_txos: Vec<BlindAssetRecord>,
  // Which new asset types this defines
  pub new_asset_codes: HashMap<AssetTypeCode, AssetType>,
  // Which tracing policy is being used for each new asset type
  pub new_tracing_policies: HashMap<AssetTypeCode, AssetTracingPolicy>,
  // Which new TXO issuance sequence numbers are used, in sorted order
  // The vec should be nonempty unless this asset code is being created in
  // this transaction.
  pub new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>>,
  // Which public key is being used to issue each asset type
  pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
  // New issuance amounts
  pub issuance_amounts: HashMap<AssetTypeCode, u64>,
  // Asset types that have issuances with confidential outputs. Issuances cannot be confidential
  // if there is an issuance cap
  pub confidential_issuance_types: HashSet<AssetTypeCode>,
  // Mapping of (op index, xfr input idx) tuples to set of valid signature keys
  // i.e. (2, 1) -> { AlicePk, BobPk } means that Alice and Bob both have valid signatures on the 2nd input of the 1st
  // operation
  pub cosig_keys: HashMap<(usize, usize), HashSet<Vec<u8>>>,
  // Identity tracing commitments of transfer inputs
  pub transfer_input_commitments: Vec<Option<ACCommitment>>,
  // Identity tracing commitments of transfer outputs
  pub transfer_output_commitments: Vec<Option<ACCommitment>>,
  // Encrypted transfer body
  pub transfer_body: Option<Box<XfrBody>>,
  // Debt swap information that must be externally validated
  pub debt_effects: HashMap<AssetTypeCode, DebtSwapEffect>,
  // Non-confidential asset types involved in confidential transfers
  pub confidential_transfer_inputs: HashSet<AssetTypeCode>,

  pub asset_types_involved: HashSet<AssetTypeCode>,
  pub custom_policy_asset_types: HashMap<AssetTypeCode, TxnCheckInputs>,
  // Updates to the AIR
  pub air_updates: HashMap<String, String>,
  // User-provided Key-Value store updates
  pub kv_updates: HashMap<Key, Vec<(KVEntrySignature, u64, Option<KVEntry>)>>,
  // Memo updates
  pub memo_updates: Vec<(AssetTypeCode, XfrPublicKey, Memo)>,
}

// Internally validates the transaction as well.
// If the transaction is invalid, it is dropped, so if you need to inspect
// the transaction in order to diagnose the error, clone it first!
#[allow(clippy::cognitive_complexity)]
impl TxnEffect {
  pub fn compute_effect(txn: Transaction) -> Result<TxnEffect, PlatformError> {
    let mut txo_count: usize = 0;
    let mut op_idx: usize = 0;
    let mut txos: Vec<Option<TxOutput>> = Vec::new();
    let mut internally_spent_txos = Vec::new();
    let mut input_txos: HashMap<TxoSID, BlindAssetRecord> = HashMap::new();
    let mut memo_updates = Vec::new();
    let mut new_asset_codes: HashMap<AssetTypeCode, AssetType> = HashMap::new();
    let mut cosig_keys = HashMap::new();
    let mut new_tracing_policies: HashMap<AssetTypeCode, AssetTracingPolicy> = HashMap::new();
    let mut new_issuance_nums: HashMap<AssetTypeCode, Vec<u64>> = HashMap::new();
    let mut issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey> = HashMap::new();
    let mut issuance_amounts = HashMap::new();
    let mut transfer_input_commitments = Vec::new();
    let mut transfer_output_commitments = Vec::new();
    let mut transfer_body: Option<Box<XfrBody>> = None;
    let mut debt_effects: HashMap<AssetTypeCode, DebtSwapEffect> = HashMap::new();
    let mut asset_types_involved: HashSet<AssetTypeCode> = HashSet::new();
    let mut confidential_issuance_types = HashSet::new();
    let mut kv_updates =
      HashMap::<Key, Vec<(SignatureOf<(Key, u64, Option<KVEntry>)>, u64, Option<KVEntry>)>>::new();
    let mut confidential_transfer_inputs = HashSet::new();

    let custom_policy_asset_types = txn.body
                                       .policy_options
                                       .clone()
                                       .unwrap_or_else(TxnPolicyData::default)
                                       .0
                                       .drain(..)
                                       .collect::<HashMap<_, _>>();

    let mut air_updates: HashMap<String, String> = HashMap::new();

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
    for op in txn.body.operations.iter() {
      assert!(txo_count == txos.len());

      match op {
        Operation::KVStoreUpdate(update) => {
          // If there is a prior update, this change must be signed by
          // the key associated with that update, and must have the
          // exactly-subsequent generation value.
          if let Some((_, gen, Some(ent))) = kv_updates.get(&update.body.0).and_then(|x| x.last()) {
            if gen + 1 != update.body.1 {
              return Err(PlatformError::InputsError(error_location!()));
            }
            update.check_signature(&ent.0)?;
          }
          // When inserting a value, ensure that the owning key has
          // signed this update
          if let Some(ent) = &update.body.2 {
            update.check_signature(&ent.0)?;
          }
          kv_updates.entry(update.body.0)
                    .or_insert_with(std::vec::Vec::new)
                    .push((update.signature.clone(), update.body.1, update.body.2.clone()));
        }

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
          def.signature
             .verify(&def.pubkey.key, &def.body)
             .map_err(|e| zei_fail!(e))?;

          let code = def.body.asset.code;
          let token = AssetType { properties: def.body.asset.clone(),
                                  ..Default::default() };

          // (2), only within this transaction
          if new_asset_codes.contains_key(&code) || new_issuance_nums.contains_key(&code) {
            return Err(inp_fail!());
          }

          // (3)
          if let Some((ref pol, ref globals)) = def.body.asset.policy {
            let globals = globals.clone();
            run_txn_check(&pol.init_check,
                          globals.id_vars,
                          globals.rt_vars,
                          globals.amt_vars,
                          globals.frac_vars,
                          &Transaction::from_seq_id(txn.seq_id))?;
          }

          issuance_keys.insert(code, token.properties.issuer);
          new_asset_codes.insert(code, token);
          new_issuance_nums.insert(code, vec![]);
          match &def.body.asset_rules.tracing_policy {
            Some(policy) => {
              new_tracing_policies.insert(code, policy.clone());
            }
            None => {}
          }
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
        Operation::IssueAsset(iss) => {
          if iss.body.num_outputs != iss.body.records.len() {
            return Err(inp_fail!());
          }

          assert!(iss.body.num_outputs == iss.body.records.len());

          let code = iss.body.code;
          let seq_num = iss.body.seq_num;

          asset_types_involved.insert(code);

          // (1), within this transaction
          //let v = vec![];
          let iss_nums = new_issuance_nums.entry(code).or_insert_with(Vec::new);

          if let Some(last_num) = iss_nums.last() {
            if seq_num <= *last_num {
              return Err(inp_fail!());
            }
          }
          iss_nums.push(seq_num);

          // (2)
          iss.signature
             .verify(&iss.pubkey.key, &iss.body)
             .map_err(|e| zei_fail!(e))?;

          // (3)
          if let Some(prior_key) = issuance_keys.get(&code) {
            if iss.pubkey != *prior_key {
              return Err(inp_fail!());
            }
          } else {
            issuance_keys.insert(code, iss.pubkey);
          }
          // Increment amounts
          txos.reserve(iss.body.records.len());
          for output in iss.body.records.iter() {
            // (4)
            if (output.0).public_key != iss.pubkey.key {
              return Err(inp_fail!());
            }

            // (5)
            if (output.0).asset_type != XfrAssetType::NonConfidential(code.val) {
              return Err(inp_fail!());
            }

            if let XfrAmount::NonConfidential(amt) = (output.0).amount {
              let issuance_amount = issuance_amounts.entry(code).or_insert(0);
              *issuance_amount += amt;
            } else {
              confidential_issuance_types.insert(code);
            }

            txos.push(Some(output.clone()));
            txo_count += 1;
          }
        }

        // An asset transfer is valid iff:
        //     1) The signatures on the body (a) all are valid and (b)
        //        there is a signature for each non-custom-policy input key
        //          - Fully checked here
        //     2) The UTXOs (a) exist on the ledger and (b) match the zei transaction.
        //          - Partially checked here -- anything which hasn't
        //            been checked will appear in `input_txos`
        //     3) The zei transaction is valid.
        //          - Checked here and in check_txn_effects
        Operation::TransferAsset(trn) => {
          if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
            return Err(inp_fail!());
          }
          if trn.body.num_outputs != trn.body.transfer.outputs.len() {
            return Err(inp_fail!());
          }
          assert!(trn.body.inputs.len() == trn.body.transfer.inputs.len());
          assert!(trn.body.num_outputs == trn.body.transfer.outputs.len());

          match trn.body.transfer_type {
            TransferType::DebtSwap => {
              let (debt_type, debt_swap_effect) = compute_debt_swap_effect(&trn.body.transfer)?;

              if debt_effects.contains_key(&debt_type) {
                return Err(inp_fail!());
              }
              debt_effects.insert(debt_type, debt_swap_effect);
            }
            TransferType::Standard => {
              let mut input_keys = HashSet::new();
              // (1a) all body signatures are valid
              for sig in &trn.body_signatures {
                if !trn.body.verify_body_signature(sig) {
                  return Err(inp_fail!());
                }
                if let Some(input_idx) = sig.input_idx {
                  let sig_keys = cosig_keys.entry((op_idx, input_idx))
                                           .or_insert_with(HashSet::new);
                  (*sig_keys).insert(sig.address.key.zei_to_bytes());
                } else {
                  input_keys.insert(sig.address.key.zei_to_bytes());
                }
              }

              // (1b) all input record owners (for non-custom-policy
              //      assets) have signed
              for (input_idx, record) in trn.body.transfer.inputs.iter().enumerate() {
                // skip signature checking for custom-policy assets
                if let Some(inp_code) = record.asset_type.get_asset_type() {
                  if custom_policy_asset_types.get(&AssetTypeCode { val: inp_code })
                                              .is_some()
                  {
                    continue;
                  }
                }
                if !input_keys.contains(&record.public_key.zei_to_bytes()) {
                  return Err(inp_fail!());
                }
                cosig_keys.entry((op_idx, input_idx))
                          .or_insert_with(HashSet::new);
              }
            }
          }
          // (3)
          // TODO: implement real policies
          transfer_input_commitments = trn.body.input_identity_commitments.clone();
          transfer_output_commitments = trn.body.output_identity_commitments.clone();
          transfer_body = Some(trn.body.transfer.clone());

          let mut input_types = HashSet::new();
          for (inp, record) in trn.body.inputs.iter().zip(trn.body.transfer.inputs.iter()) {
            // NOTE: We assume that any confidential-type asset records
            // have no atypical transfer restrictions. Be careful!
            if let Some(inp_code) = record.asset_type.get_asset_type() {
              input_types.insert(AssetTypeCode { val: inp_code });
              //asset_types_involved.insert(AssetTypeCode { val: inp_code });
            }

            // (2), checking within this transaction and recording
            // external UTXOs
            match *inp {
              TxoRef::Relative(offs) => {
                // (2).(a)
                if offs as usize >= txo_count {
                  return Err(inp_fail!());
                }
                let ix = (txo_count - 1) - (offs as usize);
                match &txos[ix] {
                  None => {
                    return Err(inp_fail!());
                  }
                  Some(TxOutput(inp_record)) => {
                    // (2).(b)
                    if inp_record != record {
                      return Err(inp_fail!());
                    }
                    internally_spent_txos.push(inp_record.clone());
                  }
                }
                txos[ix] = None;
              }
              TxoRef::Absolute(txo_sid) => {
                // (2).(a), partially
                if input_txos.contains_key(&txo_sid) {
                  return Err(inp_fail!());
                }

                input_txos.insert(txo_sid, record.clone());
              }
            }
          }

          txos.reserve(trn.body.transfer.outputs.len());
          let mut conf_transfer = false;
          for out in trn.body.transfer.outputs.iter() {
            if let XfrAssetType::Confidential(_) = out.asset_type {
              conf_transfer = true;
            }
            if let Some(out_code) = out.asset_type.get_asset_type() {
              asset_types_involved.insert(AssetTypeCode { val: out_code });
            }
            txos.push(Some(TxOutput(out.clone())));
            txo_count += 1;
          }
          // Until we can distinguish assets that have policies that invoke transfer restrictions
          // from those that don't, make note of all non-confidential inputs of confidential
          // transfers
          asset_types_involved.extend(&input_types);
          if conf_transfer {
            confidential_transfer_inputs.extend(&input_types);
          }
        }

        // An AIR assignment is valid iff:
        //     1)  The body signature is valid.
        //     2)  The credential commitment is valid for the public key of the signer.
        Operation::AIRAssign(air_assign) => {
          let commitment = &air_assign.body.data;
          let issuer_pk = &air_assign.body.issuer_pk;
          let pok = &air_assign.body.pok;
          let pk = &air_assign.pubkey;
          // 1)
          air_assign.signature
                    .verify(&pk, &air_assign.body)
                    .map_err(|e| zei_fail!(e))?;
          // 2)
          credential_verify_commitment(issuer_pk, &commitment, pok, pk.as_bytes()).map_err(|e| {
                                                                                    zei_fail!(e)
                                                                                  })?;
          air_updates.insert(serde_json::to_string(&air_assign.body.addr)?,
                             serde_json::to_string(&commitment)?);
        }
        // A memo update is valid iff:
        // 1) The signature is valid.
        // 2) The asset type is updatable (checked later).
        // 3) The signing key is the asset issuer key (checked later).
        Operation::UpdateMemo(update_memo) => {
          let pk = update_memo.pubkey;
          // 1)
          update_memo.signature
                     .verify(&pk, &update_memo.body)
                     .map_err(|e| zei_fail!(e))?;

          memo_updates.push((update_memo.body.asset_type, pk, update_memo.body.new_memo.clone()));
        }
      } // end -- match op {
      op_idx += 1;
    } // end -- for op in txn.body.operations.iter() {

    Ok(TxnEffect { txn,
                   txos,
                   input_txos,
                   cosig_keys,
                   internally_spent_txos,
                   new_asset_codes,
                   new_tracing_policies,
                   new_issuance_nums,
                   memo_updates,
                   issuance_keys,
                   confidential_transfer_inputs,
                   issuance_amounts,
                   confidential_issuance_types,
                   transfer_input_commitments,
                   transfer_output_commitments,
                   transfer_body,
                   debt_effects,
                   asset_types_involved,
                   custom_policy_asset_types,
                   air_updates,
                   kv_updates })
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
      for op in self.txn.body.operations.iter() {
        if let Operation::TransferAsset(trn) = op {
          if trn.body.inputs.len() != trn.body.transfer.inputs.len() {
            return Err(inv_fail!());
          }
          for (ix, inp_record) in trn.body.inputs.iter().zip(trn.body.transfer.inputs.iter()) {
            if let TxoRef::Absolute(input_tid) = ix {
              if input_tid == txo_sid {
                if inp_record != record {
                  return Err(inv_fail!());
                }
                if found {
                  return Err(inv_fail!());
                }
                found = true;
              }
            } else if inp_record == record {
              return Err(inv_fail!());
            }
          }
        }
      }
      if !found {
        return Err(inv_fail!());
      }
    }

    // TODO(joe): Every Utxo corresponds to exactly one TranferAsset or
    // IssueAsset, and does not appear in any inputs

    // TODO(joe): other checks?
    {
      // Slightly cheating
      if TxnEffect::compute_effect(self.txn.clone())? != *self {
        return Err(inv_fail!());
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
  // New issuance amounts
  pub issuance_amounts: HashMap<AssetTypeCode, u64>,
  // Which public key is being used to issue each asset type
  pub issuance_keys: HashMap<AssetTypeCode, IssuerPublicKey>,
  // Which new tracing policies are being added
  pub new_tracing_policies: HashMap<AssetTypeCode, AssetTracingPolicy>,
  // Updates to the AIR
  pub air_updates: HashMap<String, String>,
  // User-provided Key-Value store updates
  pub kv_updates: HashMap<Key, Vec<(KVEntrySignature, u64, Option<KVEntry>)>>,
  // Memo updates
  pub memo_updates: HashMap<AssetTypeCode, Memo>,
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
  #[allow(clippy::cognitive_complexity)]
  pub fn add_txn_effect(&mut self, txn: TxnEffect) -> Result<TxnTempSID, PlatformError> {
    // Check that KV updates are independent
    for (k, _) in txn.kv_updates.iter() {
      if self.kv_updates.contains_key(&k) {
        return Err(PlatformError::InputsError(error_location!()));
      }
    }

    // Check that no inputs are consumed twice
    for (input_sid, _) in txn.input_txos.iter() {
      if self.input_txos.contains_key(&input_sid) {
        return Err(inp_fail!());
      }
    }

    // Check that no AssetType is affected by both the block so far and
    // this transaction
    {
      for (type_code, _) in txn.new_asset_codes.iter() {
        if self.new_asset_codes.contains_key(&type_code)
           || self.new_issuance_nums.contains_key(&type_code)
        {
          return Err(inp_fail!());
        }
      }

      for (type_code, nums) in txn.new_issuance_nums.iter() {
        if self.new_asset_codes.contains_key(&type_code)
           || self.new_issuance_nums.contains_key(&type_code)
        {
          return Err(inp_fail!());
        }

        // Debug-check that issued assets are registered in `issuance_keys`
        if !nums.is_empty() {
          debug_assert!(txn.issuance_keys.contains_key(&type_code));
        }
      }
      // Ensure that each asset's memo can only be updated once per block
      for (type_code, _, _) in txn.memo_updates.iter() {
        if self.memo_updates.contains_key(&type_code) {
          return Err(inp_fail!());
        }
      }
    }

    // == All validation done, apply `txn` to this block ==
    for (k, update) in txn.kv_updates {
      self.kv_updates.insert(k, update);
    }

    let temp_sid = TxnTempSID(self.txns.len());
    self.txns.push(txn.txn);
    self.temp_sids.push(temp_sid);
    self.txos.push(txn.txos);

    for (input_sid, record) in txn.input_txos {
      // dbg!(&input_sid);
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

    for (type_code, amount) in txn.issuance_amounts.iter() {
      let issuance_amount = self.issuance_amounts.entry(*type_code).or_insert(0);
      *issuance_amount += amount;
    }

    for (type_code, tracing_policy) in txn.new_tracing_policies.iter() {
      debug_assert!(!self.new_tracing_policies.contains_key(type_code));
      self.new_tracing_policies
          .insert(*type_code, tracing_policy.clone());
    }

    for (addr, data) in txn.air_updates {
      debug_assert!(!self.air_updates.contains_key(&addr));
      self.air_updates.insert(addr, data);
    }

    for (code, _, memo) in txn.memo_updates {
      self.memo_updates.insert(code, memo);
    }

    Ok(temp_sid)
  }

  pub fn compute_txns_in_block_hash(&self) -> HashOf<Vec<Transaction>> {
    HashOf::new(&self.txns)
  }
}
