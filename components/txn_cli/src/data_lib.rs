use credentials::{credential_issuer_key_gen, CredIssuerPublicKey, CredIssuerSecretKey};
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AssetTypeCode, TxoSID};
use ledger::{des_fail, error_location, ser_fail};
use log::trace; // Other options: debug, info, warn
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use txn_builder::TransactionBuilder;
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::asset_tracer::gen_asset_tracer_keypair;
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{
  AssetRecordTemplate, AssetTracerKeyPair, AssetTracerMemo, AssetTracingPolicies,
  AssetTracingPolicy, BlindAssetRecord, OpenAssetRecord, OwnerMemo,
};

/// Path to the initial data when the program starts.
// TODO (Keyao): Redmine issue: #42: Store txn_cli data externally
// Make this data driven, not embedded in the Rust code.
// The attribute names will be determined by the customer's application and will differ from customer to customer.
// Or we'll develop a standard registry or dictionary of attributes.
const INIT_DATA_PATH: &str = "init_data.json";
/// Path to the data file.
const DATA_FILE: &str = "data.json";
/// Arbitrary choice of the maximum backup extension number.
const BACKUP_COUNT_MAX: i32 = 10000;

/// Tuple of blind asset record and associated tracer and owner memos. Memos are optional.
pub(crate) type BlindAssetRecordAndMemos =
  (BlindAssetRecord, Vec<AssetTracerMemo>, Option<OwnerMemo>);
/// Tuple of tracer and owner memos, optional.
pub(crate) type TracerAndOwnerMemos = (Vec<AssetTracerMemo>, Option<OwnerMemo>);

//
// Credentials
//
/// Credential value comparison types.
pub(crate) enum ComparisonType {
  /// Requirement: attribute value == required value
  Equal,
  /// Requirement: attribute value >= required value
  AtLeast,
}

#[derive(Clone, Copy, Deserialize, Debug, Eq, PartialEq, Serialize)]
/// Credential attribute names and their corresponding indices in the credential's values data and lender's requirements data.
///
/// See `get_name_and_length` for the value length of each attribute.
///
/// # Examples
/// * `"values": ["630", null, "1"]` in a credential's data indicates:
///   * Lower bound of the borrower's credit score is 630.
///   * Lower bound of the borrower's income isn't provided.
///   * The country code of the borrower's citizenship is 1.
/// * `"requirements": [null, "900", "7"]` in a lender's requirements data indicates:
///   * Lower bound of the credit score isn't required.
///   * Lower bound of the borrower's income must be at least 900.
///   * The country code of the borrower's citizenship must be 7.
// Note: If this pub(crate) enum is modified, update the `create_or_overwrite_credential` command too.
pub enum CredentialIndex {
  /// Lower bound of the credit score
  MinCreditScore = 0,
  /// lower bound of the income
  MinIncome = 1,
  /// Country code of citizenship
  /// See https://countrycode.org/ for country code definition.
  Citizenship = 2,
}

impl CredentialIndex {
  /// Gets the attribute name.
  pub fn get_name(self) -> String {
    match self {
      CredentialIndex::MinCreditScore => "min_credit_score".to_string(),
      CredentialIndex::MinIncome => "min_income".to_string(),
      _ => "citizenship".to_string(),
    }
  }

  /// Gets the attribute name and length.
  pub(crate) fn get_name_and_length(self) -> (String, usize) {
    match self {
      CredentialIndex::MinCreditScore => ("min_credit_score".to_string(), 3 as usize),
      CredentialIndex::MinIncome => ("min_income".to_string(), 4 as usize),
      _ => ("citizenship".to_string(), 3 as usize),
    }
  }

  /// Convertes the index in the credential record to CredentialIndex
  pub(crate) fn get_credential_index(index: u64) -> Result<Self, PlatformError> {
    match index {
      0 => Ok(CredentialIndex::MinCreditScore),
      1 => Ok(CredentialIndex::MinIncome),
      2 => Ok(CredentialIndex::Citizenship),
      _ => {
        println!("Index too large: {}", index);
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Gets the requirement type based on the index in the credential record.
  /// See the enum `ComparisonType` for supported requirement types.
  /// See the enum `CredentialIndex` for how the credential attributes are ordered.
  pub(crate) fn get_requirement_type(index: u64) -> ComparisonType {
    if index <= 1 {
      ComparisonType::AtLeast
    } else {
      ComparisonType::Equal
    }
  }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Borrower's credential records.
pub struct Credential {
  /// Credential ID
  id: u64,
  /// Borrower ID
  borrower: u64,
  /// Credential issuer ID
  pub credential_issuer: u64,
  /// Credential values, in the order defined in the enum `CredentialIndex`.
  /// Null value indicates the credential value isn't provided yet.
  /// # Examples
  /// * `"attributes": ["630", null, "1"]` indicates:
  /// * Lower bound of the borrower's credit score is 630.
  /// * Lower bound of the borrower's income isn't provided.
  /// * The country code of the borrower's citizenship is 1.
  pub values: Vec<Option<String>>,
}

impl Credential {
  /// Conpub(crate) structs a credential
  /// # Arguments
  /// `id`: credential ID
  /// `borrower`: borrower ID
  /// `credential_issuer`: credential issuer ID
  /// `values`: credential values, in the order defined in the enum `CredentialIndex`.
  pub fn new(id: u64, borrower: u64, credential_issuer: u64, values: Vec<Option<String>>) -> Self {
    Credential { id,
                 borrower,
                 credential_issuer,
                 values }
  }
}

//
// Users
//
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Asset issuer's account information.
pub(crate) struct AssetIssuer {
  /// AssetIssuer ID
  id: u64,
  /// Name
  name: String,
  /// Serialized key pair
  key_pair: String,
  /// Serialized asset tracer key pair
  tracer_key_pair: String,
}

impl AssetIssuer {
  pub fn new(id: usize, name: String) -> Result<Self, PlatformError> {
    // Generate asset issuer key pair
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());

    // Generate asset tracer key pair
    let tracer_key_pair = gen_asset_tracer_keypair(&mut ChaChaRng::from_entropy());
    let tracer_key_pair_str =
      serde_json::to_string(&tracer_key_pair).or_else(|e| Err(ser_fail!(e)))?;

    Ok(AssetIssuer { id: id as u64,
                     name,
                     key_pair: key_pair_str,
                     tracer_key_pair: tracer_key_pair_str })
  }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Credential issuer's account information.
pub struct CredentialIssuer {
  /// Credential issuer ID
  id: u64,
  /// Name
  name: String,
  /// Serialized key pair
  key_pair: String,
}

impl CredentialIssuer {
  /// Conpub(crate) structs a credential issuer for the credit score attribute.
  pub fn new(id: usize,
             name: String,
             attributes: Vec<CredentialIndex>)
             -> Result<Self, PlatformError> {
    let mut attribute_names_and_sizes = Vec::new();
    for attribute in attributes {
      attribute_names_and_sizes.push(attribute.get_name_and_length());
    }

    let key_pair =
      credential_issuer_key_gen(&mut ChaChaRng::from_entropy(), &attribute_names_and_sizes);
    let key_pair_str = serde_json::to_string(&key_pair).or_else(|e| Err(ser_fail!(e)))?;

    Ok(CredentialIssuer { id: id as u64,
                          name,
                          key_pair: hex::encode(key_pair_str) })
  }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Lender's account information.
pub struct Lender {
  /// Lender ID
  id: u64,
  /// Name
  name: String,
  /// Serialized key pair
  key_pair: String,
  /// Credential requirements, in the order defined in the enum `CredentialIndex`.
  /// Null value indicates the credential attribute isn't required.
  /// # Examples  
  /// * `"requirements": [null, "900", "7"]` indicates:
  ///   * Lower bound of the credit score isn't requirement.
  ///   * Lower bound of the borrower's income must be at least 900.
  ///   * The country code of the borrower's citizenship must be 7.
  pub requirements: Vec<Option<String>>,
  /// List of loan IDs
  pub loans: Vec<u64>,
}

impl Lender {
  pub fn new(id: usize, name: String) -> Self {
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());
    Lender { id: id as u64,
             name,
             key_pair: key_pair_str,
             requirements: vec![None, None, None],
             loans: Vec::new() }
  }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Borrower's account information.
pub struct Borrower {
  /// Borrower ID
  id: u64,
  /// Name
  pub name: String,
  /// Serialized key pair
  key_pair: String,
  /// Credential ID, if exists
  pub credentials: Option<u64>,
  /// List of loan IDs
  pub loans: Vec<u64>,
  /// Balance
  pub balance: u64,
  /// Fiat asset UTXO (unspent transaction output) SIDs, if any
  pub fiat_utxo: Option<TxoSID>,
}

impl Borrower {
  pub fn new(id: usize, name: String) -> Self {
    // Get the encoded key pair
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());

    // Conpub(crate) struct the Borrower
    Borrower { id: id as u64,
               name,
               key_pair: key_pair_str,
               credentials: None,
               loans: Vec::new(),
               balance: 0,
               fiat_utxo: None }
  }
}

//
// Loan
//
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
/// Loan statuses.
pub enum LoanStatus {
  /// The borrower has requested the loan, but the lender hasn't fulfill it
  Requested,
  /// The lender has declined the loan
  Declined,
  /// The lender has fulfilled the loan, but the borrower hasn't paid it off
  Active,
  /// The borrower has paid off the loan
  Complete,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Loan information.
pub struct Loan {
  /// Loan ID
  id: u64,
  /// Issuer ID, null if the loan isn't fulfilled          
  pub issuer: Option<u64>,
  /// Lender ID           
  pub lender: u64,
  /// Borrower ID          
  pub borrower: u64,
  /// Loan status, possible values defined in the enum `LoanStatus`
  pub status: LoanStatus,
  /// Total amount
  pub amount: u64,
  /// Outstanding balance
  pub balance: u64,
  /// Interest per 1000
  /// # Examples
  /// * `120`: interest rate is 0.12        
  pub interest_per_mille: u64,
  /// Loan duration
  duration: u64,
  /// Number of payments that have been made
  pub payments: u64,
  /// Serialized debt token code, null if the loan isn't fulfilled     
  pub code: Option<String>,
  /// Debt asset UTXO (unspent transaction output) SIDs, null if the loan isn't fulfilled     
  pub debt_utxo: Option<TxoSID>,
}

impl Loan {
  pub fn new(id: usize,
             lender: u64,
             borrower: u64,
             amount: u64,
             interest_per_mille: u64,
             duration: u64)
             -> Self {
    Loan { id: id as u64,
           issuer: None,
           lender,
           borrower,
           status: LoanStatus::Requested,
           amount,
           balance: amount,
           interest_per_mille,
           duration,
           payments: 0,
           code: None,
           debt_utxo: None }
  }
}

//
// Data
//
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Information of users, loans, fiat token code, and sequence number.
pub struct Data {
  /// List of user records
  asset_issuers: Vec<AssetIssuer>,
  pub credential_issuers: Vec<CredentialIssuer>,
  pub lenders: Vec<Lender>,
  pub borrowers: Vec<Borrower>,

  /// List of loan records
  pub loans: Vec<Loan>,

  /// List of credential records
  pub credentials: Vec<Credential>,

  /// Serialized token code of fiat asset, if defined
  pub fiat_code: Option<String>,

  /// Sequence number of the next transaction
  sequence_number: u64,
}

impl Data {
  pub fn add_loan(&mut self,
                  data_dir: &str,
                  lender: u64,
                  borrower: u64,
                  amount: u64,
                  interest_per_mille: u64,
                  duration: u64)
                  -> Result<(), PlatformError> {
    let id = self.loans.len();
    self.loans
        .push(Loan::new(id, lender, borrower, amount, interest_per_mille, duration));
    self.lenders[lender as usize].loans.push(id as u64);
    self.borrowers[borrower as usize].loans.push(id as u64);
    store_data_to_file(self.clone(), data_dir)
  }

  pub fn add_asset_issuer(&mut self, data_dir: &str, name: String) -> Result<(), PlatformError> {
    let id = self.asset_issuers.len();
    self.asset_issuers.push(AssetIssuer::new(id, name.clone())?);
    println!("{}'s id is {}.", name, id);
    store_data_to_file(self.clone(), data_dir)
  }

  pub fn get_asset_issuer_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.asset_issuers[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|e| Err(ser_fail!(e)))?))
  }

  pub fn get_asset_tracer_key_pair(&self, id: u64) -> Result<AssetTracerKeyPair, PlatformError> {
    let tracer_key_pair_str = &self.asset_issuers[id as usize].tracer_key_pair;
    let tracer_key_pair =
      serde_json::from_str(&tracer_key_pair_str).or_else(|e| Err(des_fail!(e)))?;
    Ok(tracer_key_pair)
  }

  pub fn add_credential_issuer(&mut self,
                               data_dir: &str,
                               name: String,
                               attributes: Vec<CredentialIndex>)
                               -> Result<(), PlatformError> {
    let id = self.credential_issuers.len();
    self.credential_issuers
        .push(CredentialIssuer::new(id, name.clone(), attributes)?);
    println!("{}'s id is {}.", name, id);
    store_data_to_file(self.clone(), data_dir)
  }

  pub fn get_credential_issuer_key_pair(
    &self,
    id: u64)
    -> Result<(CredIssuerPublicKey, CredIssuerSecretKey), PlatformError> {
    let key_pair_str = &self.credential_issuers[id as usize].key_pair;
    let key_pair = serde_json::from_str(&key_pair_str).or_else(|e| Err(des_fail!(e)))?;
    Ok(key_pair)
  }

  pub fn add_lender(&mut self, data_dir: &str, name: String) -> Result<(), PlatformError> {
    let id = self.lenders.len();
    self.lenders.push(Lender::new(id, name.clone()));
    println!("{}'s id is {}.", name, id);
    store_data_to_file(self.clone(), data_dir)
  }

  pub(crate) fn get_lender_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.lenders[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|e| Err(des_fail!(e)))?))
  }

  /// Creates or overwrites a credential requirement.
  /// * If the requirement attribute doesn't exist, add it to the requirements.
  /// * Otherwise, overwrite the value.
  ///
  /// # Arguments
  /// * `lender_id`: lender ID.
  /// * `attribute`: credential attribute, possible names defined in the enum `CredentialIndex`.
  /// * `requirement`: required value.
  pub fn create_or_overwrite_requirement(&mut self,
                                         data_dir: &str,
                                         lender_id: u64,
                                         attribute: CredentialIndex,
                                         requirement: &str)
                                         -> Result<(), PlatformError> {
    if self.lenders[lender_id as usize].requirements[attribute as usize] == None {
      println!("Adding the credential requirement.");
    } else {
      println!("Overwriting the credential requirement.");
    }
    self.lenders[lender_id as usize].requirements[attribute as usize] =
      Some(requirement.to_string());

    // Update the data
    store_data_to_file(self.clone(), data_dir)
  }

  pub fn add_borrower(&mut self, data_dir: &str, name: String) -> Result<(), PlatformError> {
    let id = self.borrowers.len();
    self.borrowers.push(Borrower::new(id, name.clone()));
    println!("{}'s id is {}.", name, id);
    store_data_to_file(self.clone(), data_dir)
  }

  pub fn get_borrower_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.borrowers[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|e| Err(des_fail!(e)))?))
  }

  /// Creates or overwrites a credential data.
  /// * If the credential attribute doesn't exist, add it to the credential data.
  /// * Otherwise, overwrite the value.
  ///
  /// # Arguments
  /// * `borrower_id`: borrower ID.
  /// * `credential_issuer_id`: credential issuer ID.
  /// * `attribute`: credential attribute, possible names defined in the enum `CredentialIndex`.
  /// * `value`: credential value.
  pub fn create_or_overwrite_credential(&mut self,
                                        data_dir: &str,
                                        borrower_id: u64,
                                        credential_issuer_id: u64,
                                        attribute: CredentialIndex,
                                        value: &str)
                                        -> Result<(), PlatformError> {
    // If the borrower has some credential data, update it
    // Otherwise, create a new credential to the borrower's data
    if let Some(credential_id) = self.borrowers[borrower_id as usize].credentials {
      if self.credentials[credential_id as usize].values[attribute as usize].clone() == None
         && credential_issuer_id == self.credentials[credential_id as usize].credential_issuer
      {
        println!("Adding the credential attribute.");
      } else {
        println!("Overwriting the credential attribute.");
      }
      self.credentials[credential_id as usize].values[attribute as usize] = Some(value.to_string());
    } else {
      println!("Creating the credential record.");
      let credential_id = self.credentials.len();
      let mut values = vec![None, None, None];
      values[attribute as usize] = Some(value.to_string());
      self.credentials.push(Credential::new(credential_id as u64,
                                            borrower_id,
                                            credential_issuer_id,
                                            values));
      self.borrowers[borrower_id as usize].credentials = Some(credential_id as u64);
    }

    // Update the data
    store_data_to_file(self.clone(), data_dir)
  }
}

/// Gets the sequence number and increments it.
pub fn get_and_update_sequence_number(data_dir: &str) -> Result<u64, PlatformError> {
  // Get the sequence number
  let mut data = load_data(data_dir)?;
  let sequence_number = data.sequence_number;
  println!("Sequence number: {}", sequence_number);

  // Increment the sequence number
  data.sequence_number += 1;
  store_data_to_file(data, data_dir)?;

  Ok(sequence_number)
}

/// Parses a string to u64.
/// # Arguments
/// * `val_str`: string representation of a value.
pub fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
  if let Ok(val) = val_str.trim().parse::<u64>() {
    Ok(val)
  } else {
    println!("Improperly formatted number.");
    Err(PlatformError::InputsError(error_location!()))
  }
}

/// Parses a string to a list of u64 values.
/// # Arguments
/// * `vals_str`: string representation of a list of values.
pub fn parse_to_u64_vec(vals_str: &str) -> Result<Vec<u64>, PlatformError> {
  let vals_vec = split_arg(vals_str);
  let mut vals = Vec::new();
  for val_str in vals_vec {
    if let Ok(val) = val_str.trim().parse::<u64>() {
      vals.push(val);
    } else {
      return Err(PlatformError::InputsError(error_location!()));
    }
  }
  Ok(vals)
}

//
// Load functions
//
/// Loads data.
/// * If the data file exists, loads data from it.
/// * Otherwise, loads the initial data.
pub fn load_data(data_dir: &str) -> Result<Data, PlatformError> {
  let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
  match fs::read_to_string(data_file_path) {
    Ok(data) => serde_json::from_str::<Data>(&data).or_else(|e| Err(des_fail!(e))),
    Err(_) => match fs::read_to_string(INIT_DATA_PATH) {
      Ok(init_data) => {
        let data = serde_json::from_str::<Data>(&init_data).or_else(|e| Err(des_fail!(e)))?;
        store_data_to_file(data.clone(), data_dir)?;
        Ok(data)
      }
      Err(_) => Err(PlatformError::IoError(format!("Failed to read file: {}", INIT_DATA_PATH))),
    },
  }
}

/// Loads transaction record from file
/// # Arguments
/// * `file_path`: file path.
pub fn load_txn_from_file(file_path: &str) -> Result<TransactionBuilder, PlatformError> {
  let txn = fs::read_to_string(file_path).or_else(|_| {
              Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
            })?;
  println!("Parsing builder from file contents: \"{}\"", &txn);
  match serde_json::from_str(&txn) {
    Ok(builder) => Ok(builder),
    Err(e) => Err(des_fail!(e)),
  }
}

/// Split a string by comma (`,`).
/// # Arguments
/// * `string`: string to split
pub fn split_arg(string: &str) -> Vec<&str> {
  string.split(',').collect::<Vec<&str>>()
}

/// Loads UTXO (unspent transaction output) SIDs from file.
/// # Arguments
/// * `file_path`: file path
pub fn load_sids_from_file(file_path: &str) -> Result<Vec<u64>, PlatformError> {
  let sids_str = fs::read_to_string(file_path).or_else(|_| {
                   Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
                 })?;

  let mut sids = Vec::new();
  for sid_str in split_arg(&sids_str) {
    if sid_str == "" {
      break;
    }
    sids.push(parse_to_u64(sid_str)?);
  }

  Ok(sids)
}

/// Loads blind asset record and optional owner memo from transaction file.
/// # Arguments
/// * `file_path`: file path to transaction record.
pub(crate) fn load_blind_asset_record_and_owner_memo_from_file(
  file_path: &str)
  -> Result<(BlindAssetRecord, Option<OwnerMemo>), PlatformError> {
  let txn = fs::read_to_string(file_path).or_else(|_| {
              Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
            })?;
  let _ = fs::remove_file(file_path);
  println!("Parsing builder from file contents: \"{}\"", &txn);
  match serde_json::from_str::<TransactionBuilder>(&txn) {
    Ok(builder) => {
      Ok((builder.get_output_ref(0).0.clone(), builder.get_owner_memo_ref(0).cloned()))
    }
    Err(e) => Err(des_fail!(e)),
  }
}

/// Loads blind asset records and optional owner memos from transaction files.
/// # Arguments
/// * `file_paths`: file paths to transaction records.
pub fn load_blind_asset_records_and_owner_memos_from_files(
  file_paths: &str)
  -> Result<Vec<(BlindAssetRecord, Option<OwnerMemo>)>, PlatformError> {
  let mut bars_and_owner_memos = Vec::new();
  for file_path in split_arg(file_paths) {
    let blind_asset_record_and_owner_memo =
      load_blind_asset_record_and_owner_memo_from_file(file_path)?;
    bars_and_owner_memos.push(blind_asset_record_and_owner_memo);
  }
  Ok(bars_and_owner_memos)
}

/// Loads the open asset record by getting the blind asset record and owner memo from transaction file and removes the file.
/// # Arguments
/// * `file_path`: path to the transaction file.
/// * `key_pair`: key pair of the asset record.
pub(crate) fn load_open_asset_record_from_file(file_path: &str,
                                               key_pair: &XfrKeyPair)
                                               -> Result<OpenAssetRecord, PlatformError> {
  let (blind_asset_record, owner_memo) =
    load_blind_asset_record_and_owner_memo_from_file(file_path)?;
  open_blind_asset_record(&blind_asset_record, &owner_memo, key_pair.get_sk_ref()).or_else(|error| {
                                                                            Err(PlatformError::ZeiError(error_location!(), error))
                                                                          })
}

/// Loads tracer memo from memo file
/// # Arguments
/// * `file_path`: file path to the tracer memo.
pub fn load_tracer_memo_from_file(file_path: &str) -> Result<AssetTracerMemo, PlatformError> {
  let tracer_memo = fs::read_to_string(file_path).or_else(|_| {
                      Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
                    })?;
  println!("Parsing tracer memo from file contents: \"{}\"",
           &tracer_memo);
  serde_json::from_str::<AssetTracerMemo>(&tracer_memo).or_else(|e| Err(des_fail!(e)))
}

/// Loads tracer and owner memos from memo files
/// # Arguments
/// * `file_paths`: file paths to the tracer and owner memos.
pub fn load_tracer_and_owner_memos_from_files(
  file_paths: &str)
  -> Result<Vec<TracerAndOwnerMemos>, PlatformError> {
  let mut tracer_and_owner_memos = Vec::new();
  for file_path in split_arg(file_paths) {
    let memos = fs::read_to_string(file_path).or_else(|_| {
                  Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
                })?;
    println!("Parsing tracer and owner memos from file contents: \"{}\"",
             &memos);
    match serde_json::from_str::<TracerAndOwnerMemos>(&memos) {
      Ok(memos) => {
        tracer_and_owner_memos.push(memos);
      }
      Err(e) => {
        return Err(des_fail!(e));
      }
    }
  }
  Ok(tracer_and_owner_memos)
}

//
// Store functions
//
/// Stores the program data to `DATA_FILE`, when the program starts or the data is updated.
/// # Arguments
/// * `data`: data to store.
pub(crate) fn store_data_to_file(data: Data, data_dir: &str) -> Result<(), PlatformError> {
  let data_file_path = format!("{}/{}", data_dir, DATA_FILE);
  if let Ok(as_json) = serde_json::to_string(&data) {
    if let Err(error) = fs::write(data_file_path, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                DATA_FILE, error)));
    };
  }
  Ok(())
}

/// Stores transaction record to file.
/// # Arguments
/// * `path_str`: file path to store the transaction record.
/// * `txn`: transaction builder.
pub fn store_txn_to_file(path_str: &str, txn: &TransactionBuilder) -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(txn) {
    if let Err(error) = fs::write(path_str, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                path_str, error)));
    };
    Ok(())
  } else {
    Err(PlatformError::IoError(format!("Error converting {:?} to json", txn)))
  }
}

/// Stores SIDs to file.
/// # Arguments
/// * `path_str`: file path to store the key pair.
/// * `sids`: SIDs to store, separated by comma (`,`).
pub fn store_sids_to_file(path_str: &str, sids: &str) -> Result<(), PlatformError> {
  if let Err(error) = fs::write(path_str, sids) {
    return Err(PlatformError::IoError(format!("Failed to create file {}: {}.", path_str, error)));
  };
  Ok(())
}

/// Stores tracer memo to file.
/// # Arguments
/// * `path_str`: file path to store the tracer memo.
/// * `tracer_memo`: tracer memo to store.
pub(crate) fn store_tracer_memo_to_file(path_str: &str,
                                        tracer_memo: AssetTracerMemo)
                                        -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(&tracer_memo) {
    if let Err(error) = fs::write(path_str, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                path_str, error)));
    };
  }
  Ok(())
}

/// Stores tracer and owner memos to file.
/// # Arguments
/// * `path_str`: file path to store the tracer and owner memos.
/// * `tracer_and_owner_memos`: tracer and owner memos to store.
pub fn store_tracer_and_owner_memos_to_file(path_str: &str,
                                            tracer_and_owner_memos: TracerAndOwnerMemos)
                                            -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(&tracer_and_owner_memos) {
    if let Err(error) = fs::write(path_str, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                path_str, error)));
    };
  }
  Ok(())
}

/// Gets the blind asset record and associated memos.
/// # Arguments
/// * `pub_key`: public key of the asset record.
/// * `amount`: amount of the asset record.
/// * `token_code`: token code of the asset rercord.
/// * `asset_record_type`: booleans representing whether the amount and asset are confidential.
/// * `tracing_policy`: asset tracing policy, optional.
pub fn get_blind_asset_record_and_memos(pub_key: XfrPublicKey,
                                        amount: u64,
                                        token_code: AssetTypeCode,
                                        asset_record_type: AssetRecordType,
                                        tracing_policy: Option<AssetTracingPolicy>)
                                        -> Result<BlindAssetRecordAndMemos, PlatformError> {
  let mut policies = AssetTracingPolicies::new();
  if let Some(policy) = tracing_policy {
    policies.add(policy);
  }
  let template = AssetRecordTemplate::with_asset_tracking(amount,
                                                          token_code.val,
                                                          asset_record_type,
                                                          pub_key,
                                                          policies);
  let mut prng = ChaChaRng::from_entropy();
  let params = PublicParams::new();
  Ok(build_blind_asset_record(&mut prng, &params.pc_gens, &template, vec![None]))
}

/// Gets and stores tracer and owner memos to file.
/// # Arguments
/// * `path_str`: file path to store the tracer and owner memos.
/// * `pub_key`: issuer public key.
/// * `amount`: asset amount.
/// * `token_code`: asset token code.
/// * `record_type`: booleans representing whether the amount and asset are confidential.
pub fn get_and_store_memos_to_file(path_str: &str,
                                   pub_key: XfrPublicKey,
                                   amount: u64,
                                   token_code: AssetTypeCode,
                                   record_type: AssetRecordType,
                                   policy: Option<AssetTracingPolicy>)
                                   -> Result<(), PlatformError> {
  let (_, tracer_memo, owner_memo) =
    get_blind_asset_record_and_memos(pub_key, amount, token_code, record_type, policy)?;
  store_tracer_and_owner_memos_to_file(path_str, (tracer_memo, owner_memo))
}

//
// Path related helper functions
//
/// Creates the directory for the file if missing.
/// # Arguments
/// * `path_str`: string representation of the file path.
pub(crate) fn create_directory_if_missing(path_str: &str) -> Result<(), PlatformError> {
  let path = Path::new(path_str);
  if path.exists() {
    return Ok(());
  }

  if let Some(parent) = path.parent() {
    if parent.exists() {
      return Ok(());
    }
    if let Err(error) = fs::create_dir_all(&parent) {
      return Err(PlatformError::IoError(format!("Failed to create directory for the parent path of {}: {}", path_str, error)));
    }
  }

  Ok(())
}

/// Creates the directory for the file, and renames the file with the same path if it exists.
/// # Arguments
/// * `path_str`: string representation of the file path.
/// * `overwrite`: whether to overwrite or find the available path if the file exists.
pub fn create_directory_and_rename_path(path_str: &str,
                                        overwrite: bool)
                                        -> Result<(), PlatformError> {
  let path = Path::new(&path_str);
  create_directory_if_missing(&path_str)?;
  if path.exists() && !overwrite {
    rename_existing_path(&path)?;
  }
  Ok(())
}

/// Recursively finds a backup file name not currently in use.
///
/// All path components of path must exist and be readable.
///
/// Assumes:
/// * The extension of path can be replaced by n.
/// * It is safe to check the existence of the path after doing so.
/// * Recursion won't hurt us here.
///
/// # Arguments
/// * `path`: base path to look at.
/// * `n`: extension number to try and increment.
pub(crate) fn find_available_path(path: &Path, n: i32) -> Result<PathBuf, PlatformError> {
  if n < BACKUP_COUNT_MAX {
    let path_n = path.with_extension(&n.to_string());
    if path_n.exists() {
      find_available_path(path, n + 1)
    } else {
      Ok(path_n)
    }
  } else {
    Err(PlatformError::IoError(format!("Too many backups for {:?}. Use --path to specify another path.",
    path)))
  }
}

/// Derives a backup file path.
///
/// The path must not be empty and must not be dot (".").
///
/// # Arguments
/// * `path`: path to derive from.
pub(crate) fn next_path(path: &Path) -> Result<PathBuf, PlatformError> {
  pub(crate) fn add_backup_extension(path: &Path) -> Result<PathBuf, PlatformError> {
    let mut pb = PathBuf::from(path);
    if let Some(name) = path.file_name() {
      if let Some(name_str) = name.to_str() {
        pb.set_file_name(format!("{}.0", name_str));
        Ok(pb)
      } else {
        Err(PlatformError::IoError("Failed to convert the path to string.".to_owned()))
      }
    } else {
      Err(PlatformError::IoError("Failed to get the file name.".to_owned()))
    }
  }

  if let Some(ext) = path.extension() {
    let ext_str = if let Some(string) = ext.to_str() {
      string
    } else {
      return Err(PlatformError::IoError("Failed to convert the path to string.".to_owned()));
    };

    if let Ok(n) = ext_str.parse::<i32>() {
      // Has a numeric extension
      find_available_path(path, n)
    } else {
      // Doesn't have a numeric extension
      find_available_path(&add_backup_extension(&path)?, 0)
    }
  } else {
    // Doesn't have any extension.
    if path.components().next() == None {
      println!("Is empty: {:?}. Specify a file path.", path);
      Err(PlatformError::InputsError(error_location!()))
    } else if path.file_name() == None {
      println!("Is directory: {:?}. Specify a file path.", path);
      Err(PlatformError::InputsError(error_location!()))
    } else {
      find_available_path(&add_backup_extension(&path)?, 0)
    }
  }
}

/// Renames the file
/// # Arguments
/// * `path`: file path.
pub fn rename_existing_path(path: &Path) -> Result<(), PlatformError> {
  let next = next_path(path)?;
  trace!("Next path for {:?} is {:?}", &path, &next);
  if let Err(error) = fs::rename(path, next.as_path()) {
    return Err(PlatformError::IoError(format!("Failed to rename path: {}", error)));
  }
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  fn check_next_path(input: &str, expected: &str) {
    let as_path = Path::new(input);
    if let Ok(result) = next_path(as_path) {
      let as_str = result.to_str().unwrap();
      if as_str != expected {
        panic!("{} failed:  {}", input, as_str);
      }
    }
  }

  // Note: creates and removes a file of the given name.
  // If such a file was present, it gets overwritten
  // and then removed.
  fn check_next_path_typical(input: &str, expected: &str) {
    trace!("check_next_path_typical({}, {})", input, expected);
    if let Err(e) = fs::write(input, "txn_cli next_path() test detritus") {
      panic!("write error: {:?}", e);
    }
    check_next_path(input, expected);
    if let Err(e) = fs::remove_file(input) {
      panic!("remove_file error: {:?}", e);
    }
  }

  fn check_next_path_nonextant(input: &str, expected: &str) {
    check_next_path(input, expected)
  }

  #[test]
  fn test_next_path() {
    check_next_path_typical("1000", "1000.0");
    check_next_path_nonextant("1000", "1000.0");

    check_next_path_typical("abc", "abc.0");
    check_next_path_nonextant("abc", "abc.0");

    check_next_path_typical("abc.def", "abc.def.0");
    check_next_path_nonextant("abc.def", "abc.def.0");

    check_next_path_typical("a.12", "a.13");
    check_next_path_nonextant("a.12", "a.12");

    check_next_path_typical(".12", ".12.0");
    check_next_path_nonextant(".12", ".12.0");

    check_next_path_typical("abc.12", "abc.13");
    check_next_path_nonextant("abc.12", "abc.12");

    check_next_path_typical("abc.0", "abc.1");
    check_next_path_nonextant("abc.0", "abc.0");
  }

  #[test]
  fn test_store_and_load_sids() {
    let paths = vec!["sids1", "sids2", "sids3"];
    let sids = vec!["1,2,4", "1,2, 4", "1,a,4"];

    for i in 0..3 {
      store_sids_to_file(paths[i], sids[i]).unwrap();
    }

    let expected_txo_refs = vec![1, 2, 4];

    assert_eq!(load_sids_from_file(paths[0]).unwrap(), expected_txo_refs);
    assert_eq!(load_sids_from_file(paths[1]).unwrap(), expected_txo_refs);
    assert!(load_sids_from_file(paths[2]).is_err());

    paths.into_iter()
         .map(|path| fs::remove_file(path).unwrap())
         .collect()
  }

  #[test]
  fn test_parse_to_u64_vec() {
    let amounts_arg = "1, 2,4";
    let expected_amounts = vec![1, 2, 4];

    assert_eq!(parse_to_u64_vec(amounts_arg).unwrap(), expected_amounts);
  }
}
