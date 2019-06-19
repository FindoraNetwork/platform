use crate::data_model::errors::PlatformError;
use crate::data_model::{
  AssetCreation, AssetIssuance, AssetPolicyKey, AssetToken, AssetTokenCode, AssetTransfer,
  CustomAssetPolicy, Operation, SmartContract, SmartContractKey, Transaction, TxOutput, TxnSID,
  TxoSID, Utxo, TXN_SEQ_ID_PLACEHOLDER,
};
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use std::u64;
use zei::xfr::lib::verify_xfr_note;

pub mod append_only_merkle;
pub mod errors;

macro_rules! log {
  // ($c:tt, $($x:tt)+) => {};
  ($c:tt, $($x:tt)+) => { println!($($x)+); }
}

pub trait LedgerAccess {
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo>;
  fn get_issuance_num(&self, code: &AssetTokenCode) -> Option<u64>;
  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken>;
  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy>;
  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract>;
  fn check_txn_structure(&self, _txn: &Transaction) -> bool {
    true
  }
}

pub trait LedgerUpdate {
  fn apply_transaction(&mut self, txn: &mut Transaction) -> TxoSID;
}

pub trait LedgerValidate {
  fn validate_transaction(&mut self, txn: &Transaction) -> bool;
}

pub trait ArchiveUpdate {
  fn append_transaction(&mut self, txn: Transaction) -> ();
}

pub trait ArchiveAccess {
  fn get_transaction(&self, addr: TxnSID) -> Option<&Transaction>;
}

pub fn compute_sha256_hash<T>(msg: &T) -> [u8; 32]
  where T: std::convert::AsRef<[u8]>
{
  let mut hasher = Sha256::new();
  hasher.input(msg);
  let result = hasher.result();
  let hash = array_ref![&result, 0, 32];

  *hash
}

#[derive(Default)]
pub struct LedgerState {
  txs: Vec<Transaction>, //will need to be replaced by merkle tree...
  utxos: HashMap<TxoSID, Utxo>,
  contracts: HashMap<SmartContractKey, SmartContract>,
  policies: HashMap<AssetPolicyKey, CustomAssetPolicy>,
  tokens: HashMap<AssetTokenCode, AssetToken>,
  issuance_num: HashMap<AssetTokenCode, u64>,
  txn_base_sid: TxoSID,
  max_applied_sid: TxoSID,
}

impl LedgerState {
  pub fn begin_commit(&mut self) {
    self.txn_base_sid.index = self.max_applied_sid.index + 1;
  }

  pub fn end_commit(&mut self) {}

  fn add_txo(&mut self, txo: (&TxoSID, TxOutput)) {
    let mut utxo_addr = *txo.0;

    match utxo_addr.index {
      TXN_SEQ_ID_PLACEHOLDER..=u64::MAX => {
        utxo_addr.index -= TXN_SEQ_ID_PLACEHOLDER;
        utxo_addr.index += self.txn_base_sid.index;
      }
      _ => {
        // TODO:  Is this recoverable?
        panic!("The index {} is not a placeholder.", utxo_addr.index);
      }
    }

    let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(&txo.1).unwrap()),
                          output: txo.1 };
    self.utxos.insert(utxo_addr, utxo_ref);
    self.max_applied_sid = utxo_addr;
  }

  fn apply_asset_transfer(&mut self, transfer: &AssetTransfer) {
    for utxo in &transfer.body.inputs {
      let mut rectified_txo = *utxo;
      if let TXN_SEQ_ID_PLACEHOLDER..=u64::MAX = rectified_txo.index {
        rectified_txo.index -= TXN_SEQ_ID_PLACEHOLDER;
        rectified_txo.index += self.txn_base_sid.index;
      }
      self.utxos.remove(&rectified_txo);
    }

    for out in
      transfer.body.outputs.iter().zip(transfer.body
                                               .transfer
                                               .outputs_iter()
                                               .map(|o| TxOutput::BlindAssetRecord(o.clone())))
    {
      self.add_txo(out);
    }
  }

  fn apply_asset_issuance(&mut self, issue: &AssetIssuance) {
    log!(ledger, "apply asset issue {:?}", issue.body.seq_num);
    for out in issue.body
                    .outputs
                    .iter()
                    .zip(issue.body.records.iter().map(|ref o| (*o).clone()))
    {
      log!(ledger, "add txo {:?}", out.1);
      self.add_txo(out);
    }

    self.issuance_num
        .insert(issue.body.code, issue.body.seq_num);
    log!(ledger, "insert asset issue code {:?} -> seq {:?}", issue.body.code,
      issue.body.seq_num);
  }

  fn apply_asset_creation(&mut self, create: &AssetCreation) {
    let token: AssetToken = AssetToken { properties: create.body.asset.clone(),
                                         ..Default::default() };
    self.tokens.insert(token.properties.code, token);
  }

  fn apply_operation(&mut self, op: &Operation) {
    match op {
      Operation::AssetTransfer(transfer) => self.apply_asset_transfer(transfer),
      Operation::AssetIssuance(issuance) => self.apply_asset_issuance(issuance),
      Operation::AssetCreation(creation) => self.apply_asset_creation(creation),
    }
  }

  #[cfg(test)]
  fn validate_transaction(&mut self, txn: &Transaction) -> bool {
    println!("Validating transaction");
    for op in &txn.operations {
      if !self.validate_operation(op) {
        return false;
      }
    }
    true
  }

  #[cfg(test)]
  fn validate_operation(&mut self, op: &Operation) -> bool {
    println!("Validating operation");
    match op {
      Operation::AssetTransfer(transfer) => self.validate_asset_transfer(transfer),
      Operation::AssetIssuance(issuance) => self.validate_asset_issuance(issuance),
      Operation::AssetCreation(creation) => self.validate_asset_creation(creation),
    }
  }

  // An asset transfer is valid iff:
  //     1) The signatures are valid.
  //     2) The UTXOs exist on the ledger and match the zei transaction.
  //     3) The zei transaction is valid.
  // TODO:  How do we know that the zei transaction matches?
  #[cfg(test)]
  fn validate_asset_transfer(&mut self, transfer: &AssetTransfer) -> bool {
    // [1] The signatures are valid.
    for signature in &transfer.body_signatures {
      if !signature.verify(&serde_json::to_vec(&transfer.body).unwrap()) {
        return false;
      }
    }

    // [2] The utxos exist on the ledger and match the zei transaction?
    let null_policies = vec![];
    let mut prng = ChaChaRng::from_seed([0u8; 32]);

    for utxo_addr in &transfer.body.inputs {
      if self.check_utxo(*utxo_addr).is_none() {
        return false;
      }

      if verify_xfr_note(&mut prng, &transfer.body.transfer, &null_policies).is_err() {
        return false;
      }
    }

    true
  }

  // An asset issuance is valid iff:
  //     1) The operation is unique (not a replay).
  //     2) The signature is valid and belongs to the anchor (the issuer).
  #[cfg(test)]
  fn validate_asset_issuance(&mut self, issue: &AssetIssuance) -> bool {
    // Get a valid token
    let token = self.get_asset_token(&issue.body.code);

    if token.is_none() {
      println!("validate_asset_issuance:  token.is_none()");
      return false;
    }

    let token = token.unwrap();
    let lookup_issuance_num = &self.get_issuance_num(&issue.body.code);
    let issuance_num = if lookup_issuance_num.is_none() {
      0
    } else {
      lookup_issuance_num.unwrap()
    };

    // [1] Is this a replay attack?
    if issue.body.seq_num <= issuance_num {
      println!("validate_asset_issuance:  replay attack:  {} vs {}",
               issue.body.seq_num, issuance_num);
      return false;
    }

    // [2] The signature on the body is correct.
    if issue.pubkey
            .key
            .verify(&serde_json::to_vec(&issue.body).unwrap(), &issue.signature)
            .is_err()
    {
      println!("validate_asset_issuance:  invalid signature");
      return false;
    }

    //[4] The signature belongs to the anchor.
    if token.properties.issuer != issue.pubkey {
      println!("validate_asset_issuance:  invalid issuer");
      return false;
    }

    true
  }

  // An asset creation is valid iff:
  //     1) The signature is valid.
  //     2) The token? has NOT been used by a different asset.
  // Token?
  #[cfg(test)]
  fn validate_asset_creation(&mut self, create: &AssetCreation) -> bool {
    //[1] the token is not already created, [2] the signature is correct.
    !self.tokens.contains_key(&create.body.asset.code)
    && create.pubkey
             .key
             .verify(&serde_json::to_vec(&create.body).unwrap(),
                     &create.signature)
             .is_ok()
  }
}

pub struct BlockContext<LA: LedgerAccess> {
  ledger: Arc<RwLock<LA>>,
  tokens: HashMap<AssetTokenCode, AssetToken>,
  contracts: HashMap<SmartContractKey, SmartContract>,
  policies: HashMap<AssetPolicyKey, CustomAssetPolicy>,
  issuance_num: HashMap<AssetTokenCode, u64>,
  used_txos: HashSet<TxoSID>,
}

impl<LA: LedgerAccess> BlockContext<LA> {
  pub fn new(ledger_access: &Arc<RwLock<LA>>) -> Result<Self, PlatformError> {
    Ok(BlockContext { ledger: ledger_access.clone(),
                      tokens: HashMap::new(),
                      contracts: HashMap::new(),
                      policies: HashMap::new(),
                      issuance_num: HashMap::new(),
                      used_txos: HashSet::new() })
  }

  pub fn apply_operation(&mut self, op: &Operation) {
    match op {
      Operation::AssetCreation(ac) => {
        let token: AssetToken = AssetToken { properties: ac.body.asset.clone(),
                                             ..Default::default() };
        self.tokens.insert(ac.body.asset.code, token);
      }
      Operation::AssetIssuance(ai) => {
        self.issuance_num.insert(ai.body.code, ai.body.seq_num);
      }
      Operation::AssetTransfer(at) => {
        for txo_sid in &at.body.inputs {
          if txo_sid.index < TXN_SEQ_ID_PLACEHOLDER {
            self.used_txos.insert(txo_sid.clone());
          }
        }
      }
    }
  }
}

impl<'la, LA> LedgerAccess for BlockContext<LA> where LA: LedgerAccess
{
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo> {
    if let Some(_used) = self.used_txos.get(&addr) {
      None
    } else if let Ok(la_reader) = self.ledger.read() {
      la_reader.check_utxo(addr)
    } else {
      None
    }
  }

  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
    match self.tokens.get(code) {
      Some(token) => Some(token.clone()),
      None => {
        if let Ok(la_reader) = self.ledger.read() {
          la_reader.get_asset_token(code)
        } else {
          None
        }
      }
    }
  }

  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
    match self.policies.get(key) {
      Some(policy) => Some(policy.clone()),
      None => {
        if let Ok(la_reader) = self.ledger.read() {
          la_reader.get_asset_policy(key)
        } else {
          None
        }
      }
    }
  }

  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
    match self.contracts.get(key) {
      Some(contract) => Some(contract.clone()),
      None => {
        if let Ok(la_reader) = self.ledger.read() {
          la_reader.get_smart_contract(key)
        } else {
          None
        }
      }
    }
  }

  fn get_issuance_num(&self, code: &AssetTokenCode) -> Option<u64> {
    match self.issuance_num.get(code) {
      Some(num) => Some(*num),
      None => {
        if let Ok(la_reader) = self.ledger.read() {
          la_reader.get_issuance_num(code)
        } else {
          None
        }
      }
    }
  }
}

pub struct TxnContext<'la, LA: LedgerAccess> {
  block_context: &'la mut BlockContext<LA>,
  utxos: HashMap<TxoSID, Utxo>,
}

impl<'la, LA: LedgerAccess> TxnContext<'la, LA> {
  pub fn new(block_context: &'la mut BlockContext<LA>) -> Result<Self, PlatformError> {
    Ok(TxnContext { block_context,
                    utxos: HashMap::new() })
  }

  pub fn apply_operation(&mut self, op: &Operation) {
    match op {
      Operation::AssetIssuance(ai) => {
        for (ref addr, out) in ai.body.outputs.iter().zip(ai.body.records.iter()) {
          let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(out).unwrap()),
                                output: out.clone() };
          self.utxos.insert((*addr).clone(), utxo_ref);
        }
      }
      Operation::AssetTransfer(at) => {
        for addr in at.body.inputs.iter() {
          if addr.index >= TXN_SEQ_ID_PLACEHOLDER {
            let _op = self.utxos.remove(addr);
          }
        }
        for (ref addr, out) in at.body.outputs.iter().zip(at.body.transfer.outputs_iter()) {
          let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(out).unwrap()),
                                output: TxOutput::BlindAssetRecord(out.clone()) };
          self.utxos.insert((*addr).clone(), utxo_ref);
        }
      }
      _ => {}
    }
    self.block_context.apply_operation(op);
  }

  // An asset transfer is valid iff:
  //     1) The signatures on the body all are valid.
  //     2) The UTXOs exist on the ledger and match the zei transaction.
  //     3) The zei transaction is valid.
  fn validate_asset_transfer(&mut self, transfer: &AssetTransfer) -> bool {
    // [1] The signatures are valid.
    for signature in &transfer.body_signatures {
      if !signature.verify(&serde_json::to_vec(&transfer.body).unwrap()) {
        return false;
      }
    }

    // [2] The utxos exist on ledger and match the zei transaction.
    let null_policies = vec![];
    let mut prng: ChaChaRng;
    prng = ChaChaRng::from_seed([0u8; 32]);

    for utxo_addr in &transfer.body.inputs {
      if self.check_utxo(*utxo_addr).is_none() {
        return false;
      }

      if verify_xfr_note(&mut prng, &transfer.body.transfer, &null_policies).is_err() {
        return false;
      }
    }

    true
  }

  // The asset issuance is valid iff:
  //      1) The operation is unique (not a replay).
  //      2) The signature is valid.
  //      3) The assets were issued by the proper agent (the anchor).
  //      3) The assets in the TxOutputs are owned by the signatory.
  //      4) The signature belongs to the appropriate anchor (issuer).
  fn validate_asset_issuance(&mut self, issue: &AssetIssuance) -> bool {
    // Create a token.
    let token = self.block_context.get_asset_token(&issue.body.code);

    if token.is_none() {
      return false;
    }

    let token = token.unwrap();
    let lookup_issuance_num = &self.block_context.get_issuance_num(&issue.body.code);
    let issuance_num = if lookup_issuance_num.is_none() {
      0
    } else {
      lookup_issuance_num.unwrap()
    };

    // [1] Check for a replay attack.
    if issue.body.seq_num <= issuance_num {
      return false;
    }

    // [2] The signature on the body is correct.
    if issue.pubkey
            .key
            .verify(&serde_json::to_vec(&issue.body).unwrap(), &issue.signature)
            .is_err()
    {
      return false;
    }

    // [4] The signature belongs to the anchor (issuer).
    if !(token.properties.issuer == issue.pubkey) {
      return false;
    }

    true
  }

  // An asset creation is valid iff:
  //     1) The token id is available.
  //     2) The signature is valid.
  fn validate_asset_creation(&mut self, create: &AssetCreation) -> bool {
    // [1] The token is available
    // [2] the signature is correct.
    !self.block_context
         .tokens
         .contains_key(&create.body.asset.code)
    && create.pubkey
             .key
             .verify(&serde_json::to_vec(&create.body).unwrap(),
                     &create.signature)
             .is_ok()
  }

  fn validate_operation(&mut self, op: &Operation) -> bool {
    match op {
      Operation::AssetTransfer(transfer) => self.validate_asset_transfer(transfer),
      Operation::AssetIssuance(issuance) => self.validate_asset_issuance(issuance),
      Operation::AssetCreation(creation) => self.validate_asset_creation(creation),
    }
  }
}

impl<'la, LA> LedgerAccess for TxnContext<'la, LA> where LA: LedgerAccess
{
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo> {
    match self.utxos.get(&addr) {
      Some(utxo) => Some(utxo.clone()),
      None => self.block_context.check_utxo(addr),
    }
  }

  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
    self.block_context.get_asset_token(code)
  }

  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
    self.block_context.get_asset_policy(key)
  }

  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
    self.block_context.get_smart_contract(key)
  }

  fn get_issuance_num(&self, code: &AssetTokenCode) -> Option<u64> {
    self.block_context.get_issuance_num(code)
  }
}

impl<'la, LA> LedgerValidate for TxnContext<'la, LA> where LA: LedgerAccess
{
  fn validate_transaction(&mut self, txn: &Transaction) -> bool {
    for op in &txn.operations {
      if !self.validate_operation(op) {
        return false;
      }
    }
    true
  }
}

impl LedgerUpdate for LedgerState {
  fn apply_transaction(&mut self, txn: &mut Transaction) -> TxoSID {
    let sid = self.txn_base_sid;
    self.txn_base_sid.index = self.max_applied_sid.index + 1;
    log!(ledger, "apply {:?}", sid);

    // Apply the operations
    for op in &txn.operations {
      self.apply_operation(op);
    }
    txn.sid = sid;	// TODO(Jonathan):  confirm
    sid
  }
}

impl ArchiveUpdate for LedgerState {
  fn append_transaction(&mut self, txn: Transaction) {
    self.txs.push(txn);
  }
}

impl LedgerAccess for LedgerState {
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo> {
    match self.utxos.get(&addr) {
      Some(utxo) => Some(utxo.clone()),
      None => None,
    }
  }

  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken> {
    match self.tokens.get(code) {
      Some(token) => Some(token.clone()),
      None => None,
    }
  }

  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy> {
    match self.policies.get(key) {
      Some(policy) => Some(policy.clone()),
      None => None,
    }
  }

  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract> {
    match self.contracts.get(key) {
      Some(contract) => Some(contract.clone()),
      None => None,
    }
  }

  fn get_issuance_num(&self, code: &AssetTokenCode) -> Option<u64> {
    match self.issuance_num.get(code) {
      Some(num) => {
        println!("issuance_num.get -> {}", *num);
        Some(*num)
      }
      None => {
        println!("No issuance_num.get:  {:?}", self.issuance_num);
        None
      }
    }
  }
}

impl ArchiveAccess for LedgerState {
  fn get_transaction(&self, addr: TxnSID) -> Option<&Transaction> {
    if addr.index < self.txs.len() {
      Some(&self.txs[addr.index])
    } else {
      None
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::data_model::{
    Asset, AssetCreationBody, AssetIssuanceBody, ConfidentialMemo, IssuerPublicKey, Memo,
  };
  use rand::{CryptoRng, Rng, SeedableRng};
  use rand_chacha::ChaChaRng;
  use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};

  fn build_keys<R: CryptoRng + Rng>(prng: &mut R) -> (XfrPublicKey, XfrSecretKey) {
    let keypair = XfrKeyPair::generate(prng);

    (keypair.get_pk_ref().clone(), keypair.get_sk())
  }

  fn compute_signature<T>(secret_key: &XfrSecretKey,
                          public_key: &XfrPublicKey,
                          asset_body: &T)
                          -> XfrSignature
    where T: serde::Serialize
  {
    secret_key.sign(&serde_json::to_vec(&asset_body).unwrap(), &public_key)
  }

  fn asset_creation_body(token_code: &AssetTokenCode,
                         issuer_key: &XfrPublicKey,
                         updatable: bool,
                         memo: &Option<Memo>,
                         confidential_memo: &Option<ConfidentialMemo>)
                         -> AssetCreationBody {
    let mut token_properties: Asset = Default::default();
    token_properties.code = token_code.clone();
    token_properties.issuer = IssuerPublicKey { key: issuer_key.clone() };
    token_properties.updatable = updatable;

    if memo.is_some() {
      token_properties.memo = memo.as_ref().unwrap().clone();
    } else {
      token_properties.memo = Memo {};
    }

    if confidential_memo.is_some() {
      token_properties.confidential_memo = confidential_memo.as_ref().unwrap().clone();
    } else {
      token_properties.confidential_memo = ConfidentialMemo {};
    }

    AssetCreationBody { asset: token_properties }
  }

  fn asset_creation_operation(asset_body: &AssetCreationBody,
                              public_key: &XfrPublicKey,
                              secret_key: &XfrSecretKey)
                              -> AssetCreation {
    let sign = compute_signature(&secret_key, &public_key, &asset_body);
    AssetCreation { body: asset_body.clone(),
                    pubkey: IssuerPublicKey { key: public_key.clone() },
                    signature: sign }
  }

  #[test]
  fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::default();
    let mut tx = Transaction::default();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, &None, &None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(state.validate_transaction(&tx));

    state.apply_transaction(&mut tx);
    state.append_transaction(tx);
    assert!(state.get_asset_token(&token_code1).is_some());

    assert_eq!(asset_body.asset,
               state.get_asset_token(&token_code1).unwrap().properties);

    assert_eq!(0, state.get_asset_token(&token_code1).unwrap().units);
  }

  // Change the signature to have the wrong public key
  #[test]
  fn test_asset_creation_invalid_public_key() {
    // Create a valid asset creation operation.
    let mut state = LedgerState::default();
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);
    let asset_body = asset_creation_body(&token_code1, &public_key1, true, &None, &None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    // Now re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);

    asset_create.pubkey.key = public_key2;
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(!state.validate_transaction(&tx));
  }

  // Sign with the wrong key.
  #[test]
  fn test_asset_creation_invalid_signature() {
    // Create a valid operation.
    let mut state = LedgerState::default();
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };

    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key1, true, &None, &None);
    let mut asset_create = asset_creation_operation(&asset_body, &public_key1, &secret_key1);

    // Re-sign the operation with the wrong key.
    let mut prng = ChaChaRng::from_seed([1u8; 32]);
    let (public_key2, _secret_key2) = build_keys(&mut prng);

    asset_create.pubkey.key = public_key2;
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(!state.validate_transaction(&tx));
  }

  #[test]
  fn asset_issued() {
    let mut state = LedgerState::default();
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, &None, &None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(state.validate_transaction(&tx));

    state.apply_transaction(&mut tx);

    let mut tx = Transaction::default();

    let asset_issuance_body = AssetIssuanceBody { seq_num: 0,
                                                  code: token_code1,
                                                  outputs: Vec::new(),
                                                  records: Vec::new() };

    let sign = compute_signature(&secret_key, &public_key, &asset_issuance_body);

    let asset_issuance_operation = AssetIssuance { body: asset_issuance_body,
                                                   pubkey: IssuerPublicKey { key:
                                                                               public_key.clone() },
                                                   signature: sign };

    let issue_op = Operation::AssetIssuance(asset_issuance_operation);

    tx.operations.push(issue_op);
    let sid = state.apply_transaction(&mut tx);
    state.append_transaction(tx);

    println!("sid = {:?}, placeholder = {:?}, base = {:?}, applied = {:?}",
      sid, TXN_SEQ_ID_PLACEHOLDER, state.txn_base_sid, state.max_applied_sid);
    assert!(sid.index < TXN_SEQ_ID_PLACEHOLDER);
    assert!(sid.index <= state.txn_base_sid.index);
    assert!(state.tokens.contains_key(&token_code1));
    assert!(state.txs.len() == 1);
    assert!(state.txs[0].sid == sid);
    // TODO assert!(state.utxos.contains_key(&sid));
    println!("utxos = {:?}", state.utxos);
    println!("txs = {:#?}", state.txs);
  }
}
