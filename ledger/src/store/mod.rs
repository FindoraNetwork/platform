extern crate bincode;
extern crate byteorder;
extern crate findora;
extern crate tempdir;

use crate::data_model::errors::PlatformError;
use crate::data_model::{
  AssetCreation, AssetIssuance, AssetPolicyKey, AssetToken, AssetTokenCode, AssetTransfer,
  CustomAssetPolicy, Operation, SmartContract, SmartContractKey, Transaction, TxOutput,
  TxOutput::BlindAssetRecord, TxnSID, TxoSID, Utxo, TXN_SEQ_ID_PLACEHOLDER,
};
use append_only_merkle::{AppendOnlyMerkle, Proof};
use bitmap::BitMap;
use findora::timestamp;
use findora::EnableMap;
use findora::DEFAULT_MAP;
use logged_merkle::LoggedMerkle;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use sha2::{Digest, Sha256};
use sodiumoxide::crypto::hash::sha256;
use sodiumoxide::crypto::hash::sha256::Digest as BitDigest;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::fs::OpenOptions;
use std::io::BufReader;
use std::slice::from_raw_parts;
use std::sync::{Arc, RwLock};
use std::u64;
use tempdir::TempDir;
use zei::xfr::lib::verify_xfr_note;
use zei::xfr::structs::EGPubKey;

pub mod append_only_merkle;
pub mod bitmap;
pub mod errors;
pub mod logged_merkle;

#[allow(non_upper_case_globals)]
static store: EnableMap = DEFAULT_MAP;

#[allow(non_upper_case_globals)]
static ledger_map: EnableMap = DEFAULT_MAP;

#[allow(non_upper_case_globals)]
static issue_map: EnableMap = DEFAULT_MAP;

pub struct SnapshotId {
  pub id: u64,
}

pub trait LedgerAccess {
  fn check_utxo(&self, addr: TxoSID) -> Option<Utxo>;
  fn get_issuance_num(&self, code: &AssetTokenCode) -> Option<u64>;
  fn get_asset_token(&self, code: &AssetTokenCode) -> Option<AssetToken>;
  fn get_asset_policy(&self, key: &AssetPolicyKey) -> Option<CustomAssetPolicy>;
  fn get_tracked_sids(&self, key: &EGPubKey) -> Option<Vec<TxoSID>>; // Asset issuers can query ids of UTXOs of assets they are tracking
  fn get_smart_contract(&self, key: &SmartContractKey) -> Option<SmartContract>;
  fn check_txn_structure(&self, _txn: &Transaction) -> bool {
    true
  }
}

pub trait LedgerUpdate {
  fn apply_transaction(&mut self, txn: &Transaction) -> TxoSID;
}

pub trait LedgerValidate {
  fn validate_transaction(&mut self, txn: &Transaction) -> bool;
}

pub trait ArchiveUpdate {
  fn append_transaction(&mut self, txn: Transaction) -> Transaction;
}

pub trait ArchiveAccess {
  fn get_transaction(&self, addr: TxnSID) -> Option<&Transaction>;
  fn get_proof(&self, addr: TxnSID) -> Option<Proof>;
  fn get_utxo_map(&mut self) -> Option<Vec<u8>>;
  fn get_utxos(&mut self, list: Vec<usize>) -> Option<Vec<u8>>;
  fn get_utxo_checksum(&self, version: u64) -> Option<BitDigest>;
  fn get_global_hash(&self) -> (BitDigest, u64);
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

#[repr(C)]
struct GlobalHashData {
  pub bitmap: BitDigest,
  pub merkle: append_only_merkle::HashValue,
  pub block: u64,
  pub global_hash: BitDigest,
}

impl GlobalHashData {
  fn as_ref(&self) -> &[u8] {
    unsafe {
      from_raw_parts((self as *const GlobalHashData) as *const u8,
                     std::mem::size_of::<GlobalHashData>())
    }
  }
}

const MAX_VERSION: usize = 100;

#[derive(Deserialize, Serialize)]
pub struct LedgerState {
  merkle_path: String,
  txn_path: String,
  snapshot_path: String,
  #[serde(skip)]
  merkle: Option<LoggedMerkle>,
  #[serde(skip)]
  txs: Vec<Transaction>,
  utxos: HashMap<TxoSID, Utxo>,
  #[serde(skip)]
  utxo_map: Option<BitMap>,
  utxo_map_versions: VecDeque<(usize, BitDigest)>,
  contracts: HashMap<SmartContractKey, SmartContract>,
  policies: HashMap<AssetPolicyKey, CustomAssetPolicy>,
  tracked_sids: HashMap<EGPubKey, Vec<TxoSID>>,
  tokens: HashMap<AssetTokenCode, AssetToken>,
  issuance_num: HashMap<AssetTokenCode, u64>,
  txn_count: usize,
  txn_base_sid: TxoSID,    // Next TxoSID to be commited
  max_applied_sid: TxoSID, // Last commited TxoSID
  loading: bool,
  #[serde(skip)]
  txn_log: Option<File>,
  global_hash: BitDigest,
  global_commit_count: u64,
}

impl LedgerState {
  // Create a ledger for use by a unit test.
  pub fn test_ledger() -> LedgerState {
    let tmp_dir = TempDir::new("test").unwrap();
    let merkle_buf = tmp_dir.path().join("test_ledger_merkle");
    let merkle_path = merkle_buf.to_str().unwrap();
    let txn_buf = tmp_dir.path().join("test_ledger_txns");
    let txn_path = txn_buf.to_str().unwrap();
    let snap_buf = tmp_dir.path().join("test_ledger_snap");
    let snap_path = snap_buf.to_str().unwrap();
    let utxo_map_buf = tmp_dir.path().join("test_ledger_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();
    LedgerState::new(&merkle_path, &txn_path, &snap_path, &utxo_map_path, true).unwrap()
  }

  fn load_transaction_log(path: &str) -> Result<Vec<Transaction>, std::io::Error> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut v = Vec::new();
    while let Ok(next_txn) =
      bincode::deserialize_from::<&mut BufReader<File>, Transaction>(&mut reader)
    {
      v.push(next_txn);
    }
    Ok(v)
  }

  fn save_utxo_map_version(&mut self) {
    if self.utxo_map_versions.len() >= MAX_VERSION {
      self.utxo_map_versions.pop_front();
    }

    self.utxo_map_versions
        .push_back((self.txn_count, self.utxo_map.as_mut().unwrap().compute_checksum()));
  }

  fn save_global_hash(&mut self) {
    let data = GlobalHashData { bitmap: self.utxo_map.as_mut().unwrap().compute_checksum(),
                                merkle: self.merkle.as_ref().unwrap().get_root_hash(),
                                block: self.global_commit_count,
                                global_hash: self.global_hash };

    self.global_hash = sha256::hash(data.as_ref());
    self.global_commit_count += 1;
  }

  // If store_transaction is no longer empty: add a test
  fn store_transaction(&self, _txn: &Transaction) {}

  // Initialize a logged Merkle tree for the ledger.  We might
  // be creating a new tree or opening an existing one.  We
  // always start a new log file.
  fn init_merkle_log(path: &str, create: bool) -> Result<LoggedMerkle, std::io::Error> {
    // Create a merkle tree or open an existing one.
    let result = if create {
      AppendOnlyMerkle::create(path)
    } else {
      AppendOnlyMerkle::open(path)
    };

    log!(store, "Using path {} for the Merkle tree.", path);

    let tree = match result {
      Err(x) => {
        return Err(x);
      }
      Ok(tree) => tree,
    };

    // Create a log for the tree.  The tree size ("state") is appended to
    // the end of the path.
    let next_id = tree.total_size();
    let writer = LedgerState::create_merkle_log(path.to_owned(), next_id)?;
    Ok(LoggedMerkle::new(tree, writer))
  }

  // Initialize a bitmap to track the unspent utxos.
  fn init_utxo_map(path: &str, create: bool) -> Result<BitMap, std::io::Error> {
    let file = OpenOptions::new().read(true)
                                 .write(true)
                                 .create_new(create)
                                 .open(path)?;

    if create {
      BitMap::create(file)
    } else {
      BitMap::open(file)
    }
  }

  // Initialize a new Ledger structure.
  pub fn new(merkle_path: &str,
             txn_path: &str,
             snapshot_path: &str,
             utxo_map_path: &str,
             create: bool)
             -> Result<LedgerState, std::io::Error> {
    let ledger = LedgerState { merkle_path: merkle_path.to_owned(),
                               txn_path: txn_path.to_owned(),
                               snapshot_path: snapshot_path.to_owned(),
                               merkle: Some(LedgerState::init_merkle_log(merkle_path, create)?),
                               txs: Vec::new(),
                               utxos: HashMap::new(),
                               utxo_map: Some(LedgerState::init_utxo_map(utxo_map_path,
                                                                         create)?),
                               utxo_map_versions: VecDeque::new(),
                               contracts: HashMap::new(),
                               policies: HashMap::new(),
                               tokens: HashMap::new(),
                               tracked_sids: HashMap::new(),
                               issuance_num: HashMap::new(),
                               txn_count: 0,
                               txn_base_sid: TxoSID::default(),
                               max_applied_sid: TxoSID::default(),
                               loading: false,
                               txn_log: Some(std::fs::OpenOptions::new().create(create)
                                                                        .append(true)
                                                                        .open(txn_path)?),
                               global_hash: BitDigest { 0: [0_u8; 32] },
                               global_commit_count: 0 };

    Ok(ledger)
  }

  // Load a ledger given the paths to the various storage elements.
  pub fn load(merkle_path: &str,
              txn_path: &str,
              snapshot_path: &str)
              -> Result<LedgerState, std::io::Error> {
    let merkle = LedgerState::init_merkle_log(merkle_path, false)?;
    let txs = LedgerState::load_transaction_log(txn_path)?;
    let ledger_file = File::open(snapshot_path)?;
    let mut ledger = bincode::deserialize_from::<BufReader<File>, LedgerState>(BufReader::new(ledger_file))
      .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
    ledger.merkle = Some(merkle);
    ledger.txn_log = Some(OpenOptions::new().append(true).open(txn_path)?);
    ledger.loading = true;
    for txn in &txs[ledger.txn_count..] {
      ledger.apply_transaction(&txn);
    }
    ledger.txs = txs;
    ledger.loading = false;
    Ok(ledger)
  }

  // Snapshot the ledger state.  This involves synchronizing
  // the durable data structures to the disk and starting a
  // new log file for the logged Merkle tree.
  pub fn snapshot(&mut self) -> Result<SnapshotId, std::io::Error> {
    let state = if let Some(merkle) = &self.merkle {
      merkle.state()
    } else {
      0
    };
    let writer = LedgerState::create_merkle_log(self.merkle_path.clone(), state)?;
    if let Some(merkle) = &mut self.merkle {
      merkle.snapshot(writer)?
    };

    Ok(SnapshotId { id: state })
  }

  pub fn begin_commit(&mut self) {
    self.txn_base_sid.index = self.max_applied_sid.index + 1;
  }

  pub fn end_commit(&mut self) {
    self.save_utxo_map_version();
    self.save_global_hash();
  }

  fn add_txo(&mut self, txo: (&TxoSID, TxOutput)) {
    let mut utxo_addr = *txo.0;

    if !self.loading {
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
    }
    let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(&txo.1).unwrap()),
                          output: txo.1 };
    // Check for asset tracing
    match &utxo_ref.output {
      BlindAssetRecord(record) => {
        if let Some(issuer_public_key) = &record.issuer_public_key {
          match self.tracked_sids
                    .get_mut(&issuer_public_key.eg_ristretto_pub_key)
          {
            // add utxo address to the list of indices that can be unlocked by the
            // issuer's public key
            None => {
              self.tracked_sids
                  .insert(issuer_public_key.eg_ristretto_pub_key.clone(),
                          vec![utxo_addr]);
            }
            Some(vec) => {
              vec.push(utxo_addr);
            }
          };
        }
      }
    }

    // Add a bit to the utxo bitmap.
    self.utxo_map
        .as_mut()
        .unwrap()
        .set(utxo_addr.index as usize)
        .unwrap();
    self.utxos.insert(utxo_addr, utxo_ref);
    self.max_applied_sid = utxo_addr;
  }

  fn apply_asset_transfer(&mut self, transfer: &AssetTransfer) {
    for utxo in &transfer.body.inputs {
      let mut rectified_txo = *utxo;
      if !self.loading {
        if let TXN_SEQ_ID_PLACEHOLDER..=u64::MAX = rectified_txo.index {
          rectified_txo.index -= TXN_SEQ_ID_PLACEHOLDER;
          rectified_txo.index += self.txn_base_sid.index;
        }
      }
      // Update the utxo bitmap to remove this asset.
      self.utxo_map
          .as_mut()
          .unwrap()
          .clear(rectified_txo.index as usize)
          .unwrap();
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
    debug!(issue_map, "outputs {:?}", issue.body.outputs);
    debug!(issue_map, "records {:?}", issue.body.records);
    for out in issue.body
                    .outputs
                    .iter()
                    .zip(issue.body.records.iter().map(|ref o| (*o).clone()))
    {
      debug!(ledger_map, "add txo {:?}", out.1);
      self.add_txo(out);
    }

    self.issuance_num
        .insert(issue.body.code, issue.body.seq_num);
    debug!(ledger_map,
           "insert asset issue code {:?} -> seq {:?}", issue.body.code, issue.body.seq_num);
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
    for op in &txn.operations {
      if !self.validate_operation(op) {
        return false;
      }
    }
    true
  }

  #[cfg(test)]
  fn validate_operation(&mut self, op: &Operation) -> bool {
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
    }

    if verify_xfr_note(&mut prng, &transfer.body.transfer, &null_policies).is_err() {
      return false;
    }

    true
  }

  // An asset issuance is valid iff:
  //     1) The operation is unique (not a replay).
  //     2) The signature is valid.
  //     3) The signature belongs to the anchor (the issuer).
  //     4) For traceable assets, tracing key is included in output records.
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

    // [1] Check for a replay attack.
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

    // [3] The signature belongs to the anchor (the issuer).
    if token.properties.issuer != issue.pubkey {
      println!("validate_asset_issuance:  invalid issuer");
      return false;
    }

    //[4] TODO: Noah For traceable assets, tracing key must exist and be included in all output records
    true
  }

  // An asset creation is valid iff:
  //     1) The signature is valid.
  //     2) The token? has NOT been used by a different asset token?
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

  // Create a file structure for a Merkle tree log.
  // Mostly just make a path of the form:
  //
  //     <tree_path>-log-<Merkle tree state>
  //
  fn create_merkle_log(base_path: String, next_id: u64) -> Result<File, std::io::Error> {
    let log_path = base_path.to_owned() + "-log-" + &next_id.to_string();
    println!("merkle log:  {}", log_path);
    let result = OpenOptions::new().write(true)
                                   .create(true)
                                   .truncate(true)
                                   .open(&log_path);

    let file = match result {
      Ok(file) => file,
      Err(error) => {
        println!("File open failed for {}", log_path);
        return Err(error);
      }
    };

    Ok(file)
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

  fn get_tracked_sids(&self, key: &EGPubKey) -> Option<Vec<TxoSID>> {
    if let Ok(la_reader) = self.ledger.read() {
      la_reader.get_tracked_sids(key)
    } else {
      None
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
  //      3) The signature belongs to the anchor (the issuer).
  //      4) The assets were issued by the proper agent (the anchor).
  //      5) The assets in the TxOutputs are owned by the signatory.
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

    // [3] The signature belongs to the anchor (the issuer).
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

  fn get_tracked_sids(&self, key: &EGPubKey) -> Option<Vec<TxoSID>> {
    self.block_context.get_tracked_sids(key)
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
  fn apply_transaction(&mut self, txn: &Transaction) -> TxoSID {
    let sid = self.txn_base_sid;
    self.txn_base_sid.index = self.max_applied_sid.index + 1;
    debug!(ledger_map, "apply {:?}", sid);

    // Apply the operations
    for op in &txn.operations {
      debug!(ledger_map, "Applying op:  {:?}", op);
      self.apply_operation(op);
    }
    sid
  }
}

// TODO(jonathan) This routine should propagate errors.
impl ArchiveUpdate for LedgerState {
  fn append_transaction(&mut self, mut txn: Transaction) -> Transaction {
    let index = self.txs.len();
    txn.tx_id = TxnSID { index };
    txn.merkle_id = 0;
    // The transaction now is complete and all the fields had better
    // be ready, except for the Merkle tree id.
    let hash = txn.compute_merkle_hash();

    match &mut self.merkle {
      Some(merkle) => match merkle.append(&hash) {
        Ok(n) => {
          txn.merkle_id = n;
        }
        Err(x) => {
          panic!("append failed:  {}", x);
        }
      },
      None => {
        panic!("merkle tree not loaded!");
      }
    }

    let result = txn.clone();
    self.store_transaction(&txn);
    self.txs.push(txn);
    self.txn_count = self.txs.len();
    result
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

  fn get_tracked_sids(&self, key: &EGPubKey) -> Option<Vec<TxoSID>> {
    match self.tracked_sids.get(key) {
      Some(sids) => Some(sids.clone()),
      None => None,
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

  fn get_proof(&self, addr: TxnSID) -> Option<Proof> {
    match self.get_transaction(addr) {
      None => None,
      Some(txn) => {
        match &self.merkle {
          Some(merkle) => {
            match merkle.get_proof(txn.merkle_id, 0) {
              Ok(proof) => Some(proof),
              Err(x) => {
                // TODO log error and recover?
                println!("get_proof failed:  {}", x);
                None
              }
            }
          }
          None => None,
        }
      }
    }
  }

  fn get_utxo_map(&mut self) -> Option<Vec<u8>> {
    Some(self.utxo_map.as_mut().unwrap().serialize(self.txn_count))
  }

  fn get_utxos(&mut self, utxo_list: Vec<usize>) -> Option<Vec<u8>> {
    Some(self.utxo_map
             .as_mut()
             .unwrap()
             .serialize_partial(utxo_list, self.txn_count))
  }

  fn get_utxo_checksum(&self, version: u64) -> Option<BitDigest> {
    // TODO:  This could be done via a hashmap to support more versions
    // efficiently.
    for pair in self.utxo_map_versions.iter() {
      if pair.0 as u64 == version {
        return Some(pair.1);
      }
    }

    None
  }

  fn get_global_hash(&self) -> (BitDigest, u64) {
    (self.global_hash, self.global_commit_count)
  }
}

pub mod helpers {
  use super::*;
  use crate::data_model::{Asset, AssetCreationBody, ConfidentialMemo, IssuerPublicKey, Memo};
  use rand::{CryptoRng, Rng};
  use zei::basic_crypto::signatures::{XfrKeyPair, XfrPublicKey, XfrSecretKey, XfrSignature};

  pub fn build_keys<R: CryptoRng + Rng>(prng: &mut R) -> (XfrPublicKey, XfrSecretKey) {
    let keypair = XfrKeyPair::generate(prng);

    (*keypair.get_pk_ref(), keypair.get_sk())
  }

  pub fn compute_signature<T>(secret_key: &XfrSecretKey,
                              public_key: &XfrPublicKey,
                              asset_body: &T)
                              -> XfrSignature
    where T: serde::Serialize
  {
    secret_key.sign(&serde_json::to_vec(&asset_body).unwrap(), &public_key)
  }

  pub fn asset_creation_body(token_code: &AssetTokenCode,
                             issuer_key: &XfrPublicKey,
                             updatable: bool,
                             traceable: bool,
                             memo: Option<Memo>,
                             confidential_memo: Option<ConfidentialMemo>)
                             -> AssetCreationBody {
    let mut token_properties: Asset = Default::default();
    token_properties.code = *token_code;
    token_properties.issuer = IssuerPublicKey { key: *issuer_key };
    token_properties.updatable = updatable;
    token_properties.traceable = traceable;

    if memo.is_some() {
      token_properties.memo = memo.unwrap();
    } else {
      token_properties.memo = Memo {};
    }

    if confidential_memo.is_some() {
      token_properties.confidential_memo = confidential_memo.unwrap();
    } else {
      token_properties.confidential_memo = ConfidentialMemo {};
    }

    AssetCreationBody { asset: token_properties }
  }

  pub fn asset_creation_operation(asset_body: &AssetCreationBody,
                                  public_key: &XfrPublicKey,
                                  secret_key: &XfrSecretKey)
                                  -> AssetCreation {
    let sign = compute_signature(&secret_key, &public_key, &asset_body);
    AssetCreation { body: asset_body.clone(),
                    pubkey: IssuerPublicKey { key: *public_key },
                    signature: sign }
  }
}

#[cfg(test)]
mod tests {
  use super::helpers::*;
  use super::*;
  use crate::data_model::{
    AssetCreationBody, AssetIssuanceBody, AssetTransfer, AssetTransferBody, IssuerPublicKey,
  };
  use bulletproofs::PedersenGens;
  use curve25519_dalek::scalar::Scalar;
  use rand::SeedableRng;
  use std::fs;
  use std::io::BufWriter;
  use tempfile::{tempdir, tempfile};
  use zei::algebra::bls12_381::{BLSScalar, BLSG1};
  use zei::algebra::groups::Group;
  use zei::algebra::ristretto::RistPoint;
  use zei::basic_crypto::elgamal::{
    elgamal_derive_public_key, elgamal_generate_secret_key, ElGamalPublicKey,
  };
  use zei::basic_crypto::signatures::XfrKeyPair;
  use zei::xfr::structs::{AssetAmountProof, AssetIssuerPubKeys, XfrBody, XfrNote, XfrProofs};

  #[test]
  fn test_load_transaction_log() {
    // Verify that loading transaction fails with incorrect path
    let result_err = LedgerState::load_transaction_log("incorrect/path");
    assert!(result_err.is_err());

    // Create values to be used to instantiate operations
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";

    let public_key = *keypair.get_pk_ref();
    let signature = keypair.sign(message);

    // Instantiate an AssetIssuance operation
    let asset_issuance_body = AssetIssuanceBody { code: Default::default(),
                                                  seq_num: 0,
                                                  outputs: Vec::new(),
                                                  records: Vec::new() };

    let asset_issurance = AssetIssuance { body: asset_issuance_body,
                                          pubkey: IssuerPublicKey { key: public_key },
                                          signature: signature.clone() };

    let issurance_operation = Operation::AssetIssuance(asset_issurance);

    // Instantiate an AssetCreation operation
    let asset = Default::default();

    let asset_creation = AssetCreation { body: AssetCreationBody { asset },
                                         pubkey: IssuerPublicKey { key: public_key },
                                         signature: signature };

    let creation_operation = Operation::AssetCreation(asset_creation);

    // Verify that loading transaction succeeds with correct path
    let transaction_0: Transaction = Default::default();

    let transaction_1 = Transaction { operations: vec![issurance_operation.clone()],
                                      variable_utxos: Vec::new(),
                                      credentials: Vec::new(),
                                      memos: Vec::new(),
                                      tx_id: TxnSID { index: TXN_SEQ_ID_PLACEHOLDER as usize },
                                      merkle_id: TXN_SEQ_ID_PLACEHOLDER,
                                      outputs: 1 };

    let transaction_2 = Transaction { operations: vec![issurance_operation, creation_operation],
                                      variable_utxos: Vec::new(),
                                      credentials: Vec::new(),
                                      memos: Vec::new(),
                                      tx_id: TxnSID { index: TXN_SEQ_ID_PLACEHOLDER as usize },
                                      merkle_id: TXN_SEQ_ID_PLACEHOLDER,
                                      outputs: 2 };

    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_transactions");
    let path = buf.to_str().unwrap();

    {
      let file = File::create(path).unwrap();
      let mut writer = BufWriter::new(file);

      bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_0).unwrap();
      bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_1).unwrap();
      bincode::serialize_into::<&mut BufWriter<File>, Transaction>(&mut writer, &transaction_2).unwrap();
    }

    let result_ok = LedgerState::load_transaction_log(&path);
    assert_eq!(result_ok.ok(),
               Some(vec![transaction_0, transaction_1, transaction_2]));

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_save_utxo_map_version() {
    let mut ledger_state = LedgerState::test_ledger();
    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.utxo_map_versions = vec![(0, digest); MAX_VERSION - 1].into_iter().collect();

    // Verify that save_utxo_map_version increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.save_utxo_map_version();
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION);

    // Verify that save_utxo_map_version doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.utxo_map_versions.push_back((0, digest));
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.save_utxo_map_version();
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION + 1);

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.txn_count,
                      ledger_state.utxo_map.as_mut().unwrap().compute_checksum())));
  }

  #[test]
  fn test_save_global_hash() {
    let mut ledger_state = LedgerState::test_ledger();

    let data =
      GlobalHashData { bitmap: ledger_state.utxo_map.as_mut().unwrap().compute_checksum(),
                       merkle: ledger_state.merkle.as_ref().unwrap().get_root_hash(),
                       block: ledger_state.global_commit_count,
                       global_hash: ledger_state.global_hash };

    let count_original = ledger_state.global_commit_count;

    ledger_state.save_global_hash();

    assert_eq!(ledger_state.global_hash, sha256::hash(data.as_ref()));
    assert_eq!(ledger_state.global_commit_count, count_original + 1);
  }

  #[test]
  fn test_init_merkle_log() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_merkle");
    let path = buf.to_str().unwrap();

    // Verify that opening a non-existing Merkle tree fails
    let result_open_err = LedgerState::init_merkle_log(path, false);
    assert_eq!(result_open_err.err().unwrap().kind(),
               std::io::ErrorKind::NotFound);

    // Verify that creating a non-existing Merkle tree succeeds
    let result_create_ok = LedgerState::init_merkle_log(path, true);
    assert!(result_create_ok.is_ok());

    // Verify that opening an existing Merkle tree succeeds
    let result_open_ok = LedgerState::init_merkle_log(path, false);
    assert!(result_open_ok.is_ok());

    // Verify that creating an existing Merkle tree fails
    let result_create_err = LedgerState::init_merkle_log(path, true);
    assert_eq!(result_create_err.err().unwrap().kind(),
               std::io::ErrorKind::AlreadyExists);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_init_utxo_map() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_init_bitmap");
    let path = buf.to_str().unwrap();

    // Verify that opening a non-existing bitmap fails
    let result_open_err = LedgerState::init_utxo_map(path, false);
    assert_eq!(result_open_err.err().unwrap().kind(),
               std::io::ErrorKind::NotFound);

    // Verify that creating a non-existing bitmap succeeds
    let result_create_ok = LedgerState::init_utxo_map(path, true);
    assert!(result_create_ok.is_ok());

    // Verify that creating an existing bitmap succeeds
    let result_open_ok = LedgerState::init_utxo_map(path, false);
    assert!(result_open_ok.is_ok());

    // Verify that opening an existing bitmap fails
    let result_create_err = LedgerState::init_utxo_map(path, true);
    assert_eq!(result_create_err.err().unwrap().kind(),
               std::io::ErrorKind::AlreadyExists);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_snapshot() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("test_snapshot");
    let path = buf.to_str().unwrap();

    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.merkle_path = path.to_string();
    let result = ledger_state.snapshot();

    // Verify that the SnapshotId is correct
    assert_eq!(result.ok().unwrap().id, 0);

    tmp_dir.close().unwrap();
  }

  #[test]
  fn test_begin_commit() {
    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.begin_commit();

    assert_eq!(ledger_state.txn_base_sid.index,
               ledger_state.max_applied_sid.index + 1);
  }

  #[test]
  fn test_end_commit() {
    let mut ledger_state = LedgerState::test_ledger();

    let digest = BitDigest { 0: [0_u8; 32] };
    ledger_state.utxo_map_versions = vec![(0, digest); MAX_VERSION - 1].into_iter().collect();

    // Verify that end_commit increases the size of utxo_map_versions by 1 if its length < MAX_VERSION
    ledger_state.end_commit();
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION);

    let count_original = ledger_state.global_commit_count;
    let data =
      GlobalHashData { bitmap: ledger_state.utxo_map.as_mut().unwrap().compute_checksum(),
                       merkle: ledger_state.merkle.as_ref().unwrap().get_root_hash(),
                       block: count_original,
                       global_hash: ledger_state.global_hash };

    // Verify that end_commit doesn't change the size of utxo_map_versions if its length >= MAX_VERSION
    ledger_state.utxo_map_versions.push_back((0, digest));
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION + 1);
    ledger_state.end_commit();
    assert_eq!(ledger_state.utxo_map_versions.len(), MAX_VERSION + 1);

    // Verify that the element pushed to the back is as expected
    let back = ledger_state.utxo_map_versions.get(MAX_VERSION);
    assert_eq!(back,
               Some(&(ledger_state.txn_count,
                      ledger_state.utxo_map.as_mut().unwrap().compute_checksum())));

    // Verify that the global hash is saved as expected
    assert_eq!(ledger_state.global_hash, sha256::hash(data.as_ref()));
    assert_eq!(ledger_state.global_commit_count, count_original + 1);
  }

  #[test]
  fn test_add_txo() {
    // Instantiate a BlindAssetRecord
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let pc_gens = PedersenGens::default();

    let sk = elgamal_generate_secret_key::<_, Scalar>(&mut prng);
    let xfr_pub_key = elgamal_derive_public_key(&pc_gens.B, &sk);
    let elgamal_public_key = ElGamalPublicKey(RistPoint(xfr_pub_key.get_point()));

    let sk = elgamal_generate_secret_key::<_, BLSScalar>(&mut prng);
    let id_reveal_pub_key = elgamal_derive_public_key(&BLSG1::get_base(), &sk);

    let asset_issuer_pub_key = AssetIssuerPubKeys { eg_ristretto_pub_key:
                                                      elgamal_public_key.clone(),
                                                    eg_blsg1_pub_key: id_reveal_pub_key };

    let record = zei::xfr::structs::BlindAssetRecord { issuer_public_key:
                                                         Some(asset_issuer_pub_key),
                                                       issuer_lock_type: None,
                                                       issuer_lock_amount: None,
                                                       amount: None,
                                                       asset_type: None,
                                                       public_key: Default::default(),
                                                       amount_commitments: None,
                                                       asset_type_commitment: None,
                                                       blind_share: Default::default(),
                                                       lock: None };

    // Instantiate a transaction output
    let sid = TxoSID { index: TXN_SEQ_ID_PLACEHOLDER };
    let txo = (&sid, TxOutput::BlindAssetRecord(record));

    // Instantiate a LedgerState
    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.add_txo(txo.clone());

    // Verify that add_txo sets values correctly
    let utxo_addr = TxoSID { index: 0 };

    assert_eq!(ledger_state.tracked_sids.get(&elgamal_public_key),
               Some(&vec![utxo_addr]));

    let utxo_ref = Utxo { digest: compute_sha256_hash(&serde_json::to_vec(&txo.1).unwrap()),
                          output: txo.1 };
    assert_eq!(ledger_state.utxos.get(&utxo_addr).unwrap(), &utxo_ref);

    assert_eq!(ledger_state.max_applied_sid, utxo_addr)
  }

  #[test]
  fn test_apply_asset_transfer_no_tracking() {
    // Instantiate an AssetTransfer
    let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
                                             outputs: Vec::new(),
                                             proofs: XfrProofs { asset_amount_proof:
                                                                   AssetAmountProof::NoProof,
                                                                 asset_tracking_proof:
                                                                   Default::default() } },
                             multisig: Default::default() };

    let assert_transfer_body =
      AssetTransferBody { inputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
                          outputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
                          transfer: Box::new(xfr_note) };

    let asset_transfer = AssetTransfer { body: assert_transfer_body,
                                         body_signatures: Vec::new() };

    // Instantiate a LedgerState
    let mut ledger_state = LedgerState::test_ledger();

    let map_file = tempfile().unwrap();

    let mut bitmap = BitMap::create(map_file).unwrap();
    bitmap.append().unwrap();

    ledger_state.utxo_map = Some(bitmap);

    ledger_state.apply_asset_transfer(&asset_transfer);

    assert!(ledger_state.tracked_sids.is_empty());
  }

  #[test]
  fn test_apply_asset_transfer_with_tracking() {
    // Instantiate a BlindAssetRecord
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let pc_gens = PedersenGens::default();

    let sk = elgamal_generate_secret_key::<_, Scalar>(&mut prng);
    let xfr_pub_key = elgamal_derive_public_key(&pc_gens.B, &sk);
    let elgamal_public_key = ElGamalPublicKey(RistPoint(xfr_pub_key.get_point()));

    let sk = elgamal_generate_secret_key::<_, BLSScalar>(&mut prng);
    let id_reveal_pub_key = elgamal_derive_public_key(&BLSG1::get_base(), &sk);

    let asset_issuer_pub_key = AssetIssuerPubKeys { eg_ristretto_pub_key:
                                                      elgamal_public_key.clone(),
                                                    eg_blsg1_pub_key: id_reveal_pub_key };

    let record = zei::xfr::structs::BlindAssetRecord { issuer_public_key:
                                                         Some(asset_issuer_pub_key),
                                                       issuer_lock_type: None,
                                                       issuer_lock_amount: None,
                                                       amount: None,
                                                       asset_type: None,
                                                       public_key: Default::default(),
                                                       amount_commitments: None,
                                                       asset_type_commitment: None,
                                                       blind_share: Default::default(),
                                                       lock: None };

    // Instantiate an AssetTransfer
    let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
                                             outputs: vec![record],
                                             proofs: XfrProofs { asset_amount_proof:
                                                                   AssetAmountProof::NoProof,
                                                                 asset_tracking_proof:
                                                                   Default::default() } },
                             multisig: Default::default() };

    let assert_transfer_body =
      AssetTransferBody { inputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
                          outputs: vec![TxoSID { index: TXN_SEQ_ID_PLACEHOLDER }],
                          transfer: Box::new(xfr_note) };

    let asset_transfer = AssetTransfer { body: assert_transfer_body,
                                         body_signatures: Vec::new() };

    // Instantiate a LedgerState
    let mut ledger_state = LedgerState::test_ledger();

    let map_file = tempfile().unwrap();

    let mut bitmap = BitMap::create(map_file).unwrap();
    bitmap.append().unwrap();

    ledger_state.utxo_map = Some(bitmap);

    ledger_state.apply_asset_transfer(&asset_transfer);

    assert_eq!(ledger_state.tracked_sids.get(&elgamal_public_key),
               Some(&vec![TxoSID { index: 0 }]));
  }

  #[test]
  fn test_apply_asset_issuance() {
    // Instantiate an AssetIssuance
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";
    let public_key = *keypair.get_pk_ref();
    let signature = keypair.sign(message);

    let asset_issuance_body = AssetIssuanceBody { code: Default::default(),
                                                  seq_num: 0,
                                                  outputs: vec![TxoSID { index: 0 },
                                                                TxoSID { index: 1 }],
                                                  records: Vec::new() };

    let asset_issurance = AssetIssuance { body: asset_issuance_body,
                                          pubkey: IssuerPublicKey { key: public_key },
                                          signature: signature };

    // Instantiate a LedgerState and apply the AssetIssuance
    let mut ledger_state = LedgerState::test_ledger();
    ledger_state.apply_asset_issuance(&asset_issurance);

    // Verify that apply_asset_issuance correctly adds each txo to tracked_sids
    for output in asset_issurance.body
                                 .outputs
                                 .iter()
                                 .zip(asset_issurance.body.records.iter().map(|ref o| (*o)))
    {
      match &output.1 {
        BlindAssetRecord(record) => {
          assert!(ledger_state.tracked_sids
                              .get(&record.issuer_public_key
                                          .as_ref()
                                          .unwrap()
                                          .eg_ristretto_pub_key)
                              .unwrap()
                              .contains(output.0));
        }
      }
    }

    // Verify that issuance_num is correctly set
    assert_eq!(ledger_state.issuance_num.get(&asset_issurance.body.code),
               Some(&asset_issurance.body.seq_num));
  }

  #[test]
  fn test_apply_asset_creation() {
    let mut ledger_state = LedgerState::test_ledger();

    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";
    let public_key = *keypair.get_pk_ref();
    let signature = keypair.sign(message);

    let asset_creation = AssetCreation { body: AssetCreationBody { asset: Default::default() },
                                         pubkey: IssuerPublicKey { key: public_key },
                                         signature: signature };

    let token = AssetToken { properties: asset_creation.body.asset.clone(),
                             ..Default::default() };

    ledger_state.apply_asset_creation(&asset_creation);

    assert_eq!(ledger_state.tokens.get(&token.properties.code),
               Some(&token));
  }

  #[test]
  fn test_apply_operation() {
    // Create values to be used to instantiate operations
    let mut prng = rand_chacha::ChaChaRng::from_seed([0u8; 32]);

    let keypair = XfrKeyPair::generate(&mut prng);
    let message: &[u8] = b"test";

    let public_key = *keypair.get_pk_ref();
    let signature = keypair.sign(message);

    // Instantiate an AssetTransfer operation
    let xfr_note = XfrNote { body: XfrBody { inputs: Vec::new(),
                                             outputs: Vec::new(),
                                             proofs: XfrProofs { asset_amount_proof:
                                                                   AssetAmountProof::NoProof,
                                                                 asset_tracking_proof:
                                                                   Default::default() } },
                             multisig: Default::default() };

    let assert_transfer_body = AssetTransferBody { inputs: Vec::new(),
                                                   outputs: Vec::new(),
                                                   transfer: Box::new(xfr_note) };

    let asset_transfer = AssetTransfer { body: assert_transfer_body,
                                         body_signatures: Vec::new() };

    let transfer_operation = Operation::AssetTransfer(asset_transfer.clone());

    // Instantiate an AssetIssuance operation
    let asset_issuance_body = AssetIssuanceBody { code: Default::default(),
                                                  seq_num: 0,
                                                  outputs: Vec::new(),
                                                  records: Vec::new() };

    let asset_issurance = AssetIssuance { body: asset_issuance_body,
                                          pubkey: IssuerPublicKey { key: public_key },
                                          signature: signature.clone() };

    let issurance_operation = Operation::AssetIssuance(asset_issurance.clone());

    // Instantiate an AssetCreation operation
    let asset = Default::default();

    let asset_creation = AssetCreation { body: AssetCreationBody { asset },
                                         pubkey: IssuerPublicKey { key: public_key },
                                         signature: signature };

    let creation_operation = Operation::AssetCreation(asset_creation.clone());

    // Test apply_operation
    let mut ledger_state = LedgerState::test_ledger();

    assert_eq!(ledger_state.apply_operation(&transfer_operation),
               ledger_state.apply_asset_transfer(&asset_transfer));
    assert_eq!(ledger_state.apply_operation(&issurance_operation),
               ledger_state.apply_asset_issuance(&asset_issurance));
    assert_eq!(ledger_state.apply_operation(&creation_operation),
               ledger_state.apply_asset_creation(&asset_creation));
  }

  #[test]
  fn test_create_merkle_log() {
    let tmp_dir = tempdir().unwrap();
    let buf = tmp_dir.path().join("merkle_log");
    let base_path = buf.to_str().unwrap();

    let result = LedgerState::create_merkle_log(base_path.to_string(), 0);
    assert!(result.is_ok());

    let path = base_path.to_owned() + "-log-0";
    assert!(fs::metadata(path).is_ok());

    tmp_dir.close().unwrap();
  }

  // TODO (Keyao): Add unit tests for
  //   BlockContext::new
  //   BlockContext::apply_operation
  //   LedgerAccess for BlockContext
  //     LedgerAccess::check_utxo
  //     LedgerAccess::get_asset_token
  //     LedgerAccess::get_asset_policy
  //     LedgerAccess::get_smart_contract
  //     LedgerAccess::get_issuance_num
  //     LedgerAccess::get_tracked_sids
  //   TxnContext::new
  //   TxnContext::apply_operation
  //   LedgerAccess for TxnContext
  //     LedgerAccess::check_utxo
  //     LedgerAccess::get_asset_token
  //     LedgerAccess::get_asset_policy
  //     LedgerAccess::get_smart_contract
  //     LedgerAccess::get_issuance_num
  //     LedgerAccess::get_tracked_sids
  //   LedgerUpdate for LedgerState
  //     LedgerUpdate::apply_transaction
  //   ArchiveUpdate for LedgerState
  //     ArchiveUpdate::append_transaction
  //   LedgerAccess for LedgerState
  //     LedgerAccess::check_utxo
  //     LedgerAccess::get_asset_token
  //     LedgerAccess::get_asset_policy
  //     LedgerAccess::get_smart_contract
  //     LedgerAccess::get_issuance_num
  //     LedgerAccess::get_tracked_sids
  //   ArchiveAccess for LedgerState
  //     ArchiveAccess::get_transaction
  //     ArchiveAccess::get_proof
  //     ArchiveAccess::get_utxo_map
  //     ArchiveAccess::get_utxos
  //     ArchiveAccess::get_utxo_checksum
  //     ArchiveAccess::get_global_hash

  #[test]
  fn test_asset_creation_valid() {
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();

    let token_code1 = AssetTokenCode { val: [1; 16] };
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(state.validate_transaction(&tx));

    state.apply_transaction(&tx);
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
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);
    let asset_body = asset_creation_body(&token_code1, &public_key1, true, false, None, None);
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
    let mut state = LedgerState::test_ledger();
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };

    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key1, secret_key1) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key1, true, false, None, None);
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
    let tmp_dir = TempDir::new("test").unwrap();
    let merkle_buf = tmp_dir.path().join("test_merkle");
    let merkle_path = merkle_buf.to_str().unwrap();
    let txn_buf = tmp_dir.path().join("test_txnlog");
    let txn_path = txn_buf.to_str().unwrap();
    let ledger_buf = tmp_dir.path().join("test_ledger");
    let ledger_path = ledger_buf.to_str().unwrap();
    let utxo_map_buf = tmp_dir.path().join("test_utxo_map");
    let utxo_map_path = utxo_map_buf.to_str().unwrap();

    let mut ledger =
      LedgerState::new(&merkle_path, &txn_path, &ledger_path, &utxo_map_path, true).unwrap();

    assert!(ledger.get_global_hash() == (BitDigest { 0: [0_u8; 32] }, 0));
    let mut tx = Transaction::default();
    let token_code1 = AssetTokenCode { val: [1; 16] };
    let mut prng = ChaChaRng::from_seed([0u8; 32]);
    let (public_key, secret_key) = build_keys(&mut prng);

    let asset_body = asset_creation_body(&token_code1, &public_key, true, false, None, None);
    let asset_create = asset_creation_operation(&asset_body, &public_key, &secret_key);
    tx.operations.push(Operation::AssetCreation(asset_create));

    assert!(ledger.validate_transaction(&tx));

    ledger.apply_transaction(&tx);

    let mut tx = Transaction::default();

    let asset_issuance_body = AssetIssuanceBody { seq_num: 0,
                                                  code: token_code1,
                                                  outputs: vec![TxoSID { index: 0 }],
                                                  records: Vec::new() };

    let sign = compute_signature(&secret_key, &public_key, &asset_issuance_body);

    let asset_issuance_operation = AssetIssuance { body: asset_issuance_body,
                                                   pubkey: IssuerPublicKey { key:
                                                                               public_key.clone() },
                                                   signature: sign };

    let issue_op = Operation::AssetIssuance(asset_issuance_operation);

    tx.operations.push(issue_op);
    let sid = ledger.apply_transaction(&tx);
    let transaction = ledger.append_transaction(tx);
    let txn_id = transaction.tx_id;

    println!("utxos = {:?}", ledger.utxos);
    // TODO assert!(ledger.utxos.contains_key(&sid));

    match ledger.get_proof(txn_id) {
      Some(proof) => {
        assert!(proof.tx_id == ledger.txs[txn_id.index].merkle_id);
      }
      None => {
        panic!("get_proof failed for tx_id {}, merkle_id {}, state {}",
               transaction.tx_id.index,
               transaction.merkle_id,
               ledger.merkle.unwrap().state());
      }
    }

    // We don't actually have anything to commmit yet,
    // but this will save the empty checksum, which is
    // enough for a bit of a test.
    ledger.end_commit();
    assert!(ledger.get_global_hash() == (ledger.global_hash, 1));
    let query_result = ledger.get_utxo_checksum(ledger.txn_count as u64).unwrap();
    let compute_result = ledger.utxo_map.as_mut().unwrap().compute_checksum();
    println!("query_result = {:?}, compute_result = {:?}",
             query_result, compute_result);

    assert!(query_result == compute_result);

    match ledger.snapshot() {
      Ok(n) => {
        assert!(n.id == 1);
      }
      Err(x) => {
        panic!("snapshot failed:  {}", x);
      }
    }

    asset_transfer(&mut ledger, &sid);
  }

  fn asset_transfer(_ledger: &mut LedgerState, _sid: &TxoSID) {
    // ledger.utxos[sid] is a valid utxo.
  }
}
