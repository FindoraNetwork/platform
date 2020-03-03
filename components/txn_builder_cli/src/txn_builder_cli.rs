#![deny(warnings)]
use clap::{App, Arg, SubCommand};
use env_logger::{Env, Target};
use hex;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::{AccountAddress, AssetTypeCode, TransferType, TxOutput, TxoRef, TxoSID};
use ledger::policies::{DebtMemo, Fraction};
use log::trace; // Other options: debug, info, warn
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::process::Command;
use std::thread;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::{BuildsTransactions, TransactionBuilder, TransferOperationBuilder};
use zei::api::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey, ACRevealSig,
  Credential as ZeiCredential,
};
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_asset_record, AssetRecordType};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{AssetRecord, BlindAssetRecord, OpenAssetRecord};
extern crate exitcode;

// TODO (Keyao): Determine which commands is no longer useful, remove them, and update README
// E.g. funds loading can be achieved by either of the following:
//      (1) "add define_asset" + "submit" + "add issue_and_transfer_asset"
//      (2) "borrower load_funds"
// (2) seems better since it handles all the necessary steps.
// Then do we still want to support "add define_asset", "submit" and "add issue_and_transfer_asset" commands?

const INIT_DATA: &str = r#"
{
  "issuers": [
    {
      "id": 0,
      "name": "Izzie",
      "key_pair": "76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc720fdbac9b10b7587bba7b5bc163bce69e796d71e4ed44c10fcb4488689f7a144"
    }
  ],
  "lenders": [
    {
      "id": 0,
      "name": "Lenny",
      "key_pair": "023f37203a2476c42566a61cc55c3ca875dbb4cc41c0deb789f8e7bf881836384d4b18062f8502598de045ca7b69f067f59f93b16e3af8733a988adc2341f5c8",
      "min_credit_score": 500,
      "loans": []
    },
    {
      "id": 1,
      "name": "Luna",
      "key_pair": "023f37203a2476c42566a61cc55c3ca875dbb4cc41c0deb789f8e7bf881836384d4b18062f8502598de045ca7b69f067f59f93b16e3af8733a988adc2341f5c8",
      "min_credit_score": 680,
      "loans": []
    }
  ],
  "borrowers": [
    {
      "id": 0,
      "name": "Ben",
      "key_pair": "f6a12ca8ffc30a66ca140ccc7276336115819361186d3f535dd99f8eaaca8fce7d177f1e71b490ad0ce380f9578ab12bb0fc00a98de8f6a555c81d48c2039249",
      "credentials": [
          0,
          null,
          null
      ],
      "loans": [],
      "balance": 0,
      "utxo": null
    }
  ],
  "credentials": [
    {
      "id": 0,
      "borrower": 0,
      "attribute": "MinCreditScore",
      "value": 650,
      "proof": null
    }
  ],
  "loans": [],
  "fiat_code": null,
  "sequence_number": 1
}"#;
const DATA_FILE: &str = "data.json";
const QUERY_PORT: &str = "8668";
const SUBMIT_PORT: &str = "8669";
const LEDGER_STANDALONE: &str = "../../target/debug/ledger_standalone";

//
// Credentials
//
// TODO (Keyao): Support more attributes
#[derive(Clone, Deserialize, Debug, Eq, PartialEq, Serialize)]
// Credential attributes and their corresponding indeces in the borrower's data
enum CredentialIndex {
  MinCreditScore = 0,
  MinIncome = 1,
  Citizenship = 2,
}

#[derive(Clone, Deserialize, Debug, Serialize)]
struct Credential {
  id: u64,
  borrower: u64,
  attribute: CredentialIndex,
  value: u64,
  proof: Option<String>,
}

impl Credential {
  fn new(id: u64, borrower: u64, attribute: CredentialIndex, value: u64) -> Self {
    Credential { id,
                 borrower,
                 attribute,
                 value,
                 proof: None }
  }
}

//
// Users
//
#[derive(Clone, Deserialize, Serialize)]
struct Issuer {
  id: u64,
  name: String,
  key_pair: String,
}

impl Issuer {
  fn new(id: usize, name: String) -> Self {
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([0u8; 32]));
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());
    Issuer { id: id as u64,
             name,
             key_pair: key_pair_str }
  }
}

#[derive(Clone, Deserialize, Serialize)]
struct Lender {
  id: u64,
  name: String,
  key_pair: String,
  min_credit_score: u64,
  loans: Vec<u64>,
}

impl Lender {
  fn new(id: usize, name: String, min_credit_score: u64) -> Self {
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([1u8; 32]));
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());
    Lender { id: id as u64,
             name,
             key_pair: key_pair_str,
             min_credit_score,
             loans: Vec::new() }
  }
}

#[derive(Clone, Deserialize, Serialize)]
struct Borrower {
  id: u64,
  name: String,
  key_pair: String,
  credentials: [Option<u64>; 3], // Credential ids, ordered by CredentialIndex
  loans: Vec<u64>,
  balance: u64,
  utxo: Option<TxoSID>, // Fiat utxo sid
}

impl Borrower {
  fn new(id: usize, name: String) -> Self {
    // Get the encoded key pair
    let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_seed([2u8; 32]));
    let key_pair_str = hex::encode(key_pair.zei_to_bytes());

    // Construct the Borrower
    Borrower { id: id as u64,
               name,
               key_pair: key_pair_str,
               credentials: [None; 3],
               loans: Vec::new(),
               balance: 0,
               utxo: None }
  }
}

//
// Loan
//
#[derive(Clone, Deserialize, Debug, Serialize)]
struct Loan {
  id: u64,                 // Loan id
  lender: u64,             // Lender id
  borrower: u64,           // Borrower id
  active: bool,            // Whether the loan has been activated
  rejected: bool,          // Whether the loan has been rejected
  amount: u64,             // Amount in total
  balance: u64,            // Loan balance
  interest_per_mille: u64, // Interest per 1000. E.g. 120 means the interest rate is 0.12
  duration: u64,           // Duration of the loan
  payments: u64,           // Number of payments that have been made
  code: Option<String>,    // Debt token code
  utxo: Option<TxoSID>,    // Debt utxo sid
}

impl Loan {
  fn new(id: usize,
         lender: u64,
         borrower: u64,
         amount: u64,
         interest_per_mille: u64,
         duration: u64)
         -> Self {
    Loan { id: id as u64,
           lender,
           borrower,
           active: false,
           rejected: false,
           amount,
           balance: amount,
           interest_per_mille,
           duration,
           payments: 0,
           code: None,
           utxo: None }
  }
}

//
// Data
//
#[derive(Clone, Deserialize, Serialize)]
struct Data {
  // Users
  issuers: Vec<Issuer>,
  lenders: Vec<Lender>,
  borrowers: Vec<Borrower>,

  // Loans
  loans: Vec<Loan>,

  // Credentials
  credentials: Vec<Credential>,

  // Fiat token coce
  fiat_code: Option<String>,

  // Sequence number of the next transaction
  sequence_number: u64,
}

impl Data {
  fn add_loan(&mut self,
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
    store_data_to_file(self.clone())
  }

  fn add_issuer(&mut self, name: String) -> Result<(), PlatformError> {
    let id = self.issuers.len();
    self.issuers.push(Issuer::new(id, name));
    store_data_to_file(self.clone())
  }

  fn get_issuer_key_pair(&mut self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.issuers[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
  }

  fn add_lender(&mut self, name: String, min_credit_score: u64) -> Result<(), PlatformError> {
    let id = self.lenders.len();
    self.lenders.push(Lender::new(id, name, min_credit_score));
    store_data_to_file(self.clone())
  }

  fn get_lender_key_pair(&mut self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.lenders[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
  }

  fn add_borrower(&mut self, name: String) -> Result<(), PlatformError> {
    let id = self.borrowers.len();
    self.borrowers.push(Borrower::new(id, name));
    store_data_to_file(self.clone())
  }

  fn get_borrower_key_pair(&mut self, id: u64) -> Result<XfrKeyPair, PlatformError> {
    let key_pair_str = &self.borrowers[id as usize].key_pair;
    Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
  }

  fn add_or_update_credential(&mut self,
                              borrower_id: u64,
                              attribute: CredentialIndex,
                              value: u64)
                              -> Result<(), PlatformError> {
    // If there's an existing record, update the value and remove the proof
    // Otherwise, add the record directly
    if let Some(credential_id) =
      self.borrowers[borrower_id as usize].credentials[attribute.clone() as usize]
    {
      println!("Updating the credential record.");
      self.credentials[credential_id as usize].value = value;
      self.credentials[credential_id as usize].proof = None;
    } else {
      println!("Adding the credential record.");
      let credential_id = self.credentials.len();
      self.credentials
          .push(Credential::new(credential_id as u64, borrower_id, attribute.clone(), value));
      self.borrowers[borrower_id as usize].credentials[attribute as usize] =
        Some(credential_id as u64);
    }

    // Update the data
    store_data_to_file(self.clone())
  }
}

fn get_init_data() -> Result<Data, PlatformError> {
  serde_json::from_str::<Data>(INIT_DATA).or(Err(PlatformError::DeserializationError))
}

//
// Load functions
//
fn load_data() -> Result<Data, PlatformError> {
  let mut file;
  match File::open(DATA_FILE) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      let data = get_init_data()?;
      store_data_to_file(data.clone())?;
      return Ok(data);
    }
  }
  let mut data = String::new();
  if file.read_to_string(&mut data).is_err() {
    Err(PlatformError::IoError(format!("Failed to read file: {}", "data")))
  } else {
    serde_json::from_str::<Data>(&data).or(Err(PlatformError::DeserializationError))
  }
}

// Get the sequence number and increment it
fn load_and_update_sequence_number() -> Result<u64, PlatformError> {
  // Get the sequence number
  let mut data = load_data()?;
  let sequence_number = data.sequence_number;
  println!("Sequence number: {}", sequence_number);

  // Increment the sequence number
  data.sequence_number += 1;
  store_data_to_file(data)?;

  Ok(sequence_number)
}

fn load_txn_builder_from_file(file_path: &str) -> Result<TransactionBuilder, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      return Err(PlatformError::IoError(format!("File doesn't exist: {}. Try subcommand create.",
                                         file_path)));
    }
  }
  let mut txn = String::new();
  if file.read_to_string(&mut txn).is_err() {
    return Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)));
  }
  println!("Parsing builder from file contents: \"{}\"", &txn);
  match serde_json::from_str(&txn) {
    Ok(builder) => Ok(builder),
    Err(_) => Err(PlatformError::DeserializationError),
  }
}

fn load_pub_key_from_file(file_path: &str) -> Result<XfrPublicKey, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      return Err(PlatformError::IoError(format!("File doesn't exist: {}. Try subcommand pubkeygen.",
               file_path)));
    }
  }

  let key: XfrPublicKey;
  let mut key_byte_buffer = Vec::new();
  match file.read_to_end(&mut key_byte_buffer) {
    Ok(_len) => {
      key = XfrPublicKey::zei_from_bytes(&key_byte_buffer);
    }
    Err(_e) => {
      return Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)));
    }
  }
  Ok(key)
}

fn split_arg(string: &str) -> Vec<&str> {
  string.split(',').collect::<Vec<&str>>()
}

fn load_sids_from_file(file_path: &str) -> Result<Vec<TxoRef>, PlatformError> {
  let mut file;
  match File::open(file_path) {
    Ok(f) => {
      file = f;
    }
    Err(_) => {
      return Err(PlatformError::IoError(format!("File doesn't exist: {}. Try subcommand store --sids.",file_path)));
    }
  }

  let mut sids_str = String::new();
  if file.read_to_string(&mut sids_str).is_err() {
    return Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)));
  }

  let mut txo_refs = Vec::new();
  for sid_str in split_arg(&sids_str) {
    if let Ok(sid) = sid_str.trim().parse::<u64>() {
      txo_refs.push(TxoRef::Absolute(TxoSID(sid)));
    } else {
      println!("Improperly formatted sid.");
      return Err(PlatformError::InputsError);
    }
  }

  Ok(txo_refs)
}

fn load_blind_asset_records_from_files(file_paths: &str)
                                       -> Result<Vec<BlindAssetRecord>, PlatformError> {
  let mut blind_asset_records = Vec::new();

  for mut file_path in split_arg(file_paths) {
    file_path = file_path.trim();
    let mut file;
    match File::open(file_path) {
      Ok(f) => {
        file = f;
      }
      Err(_) => {
        return Err(PlatformError::IoError(format!("File doesn't exist: {}. Try subcommand store --blind_asset_record.",
                 file_path)));
      }
    }

    let mut blind_asset_record_str = String::new();
    if file.read_to_string(&mut blind_asset_record_str).is_err() {
      return Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)));
    }

    if let Ok(blind_asset_record) = serde_json::from_str(&blind_asset_record_str) {
      blind_asset_records.push(blind_asset_record);
    } else {
      println!("Improperly formatted blind asset record.");
      return Err(PlatformError::InputsError);
    }
  }

  Ok(blind_asset_records)
}

fn load_addresses_from_files(file_paths: &str) -> Result<Vec<AccountAddress>, PlatformError> {
  let mut addresses = Vec::new();

  for file_path in split_arg(file_paths) {
    let address_key = load_pub_key_from_file(file_path.trim())?;
    addresses.push(AccountAddress { key: address_key });
  }

  Ok(addresses)
}

//
// Store functions
//
fn store_data_to_file(data: Data) -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(&data) {
    if let Err(error) = fs::write(DATA_FILE, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                DATA_FILE, error)));
    };
  }
  Ok(())
}

fn store_txn_builder_to_file(file_path: &str,
                             txn: &TransactionBuilder)
                             -> Result<(), PlatformError> {
  if let Ok(as_json) = serde_json::to_string(txn) {
    if let Err(error) = fs::write(file_path, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                file_path, error)));
    };
  }
  Ok(())
}

// Write a new key pair to the given paths.
// Assumes tilde expansion has already been done on paths.
fn store_key_pair_to_file(path_str: &str) -> Result<(), PlatformError> {
  let file_path = Path::new(path_str);
  let parent_path = if let Some(path) = file_path.parent() {
    path
  } else {
    return Err(PlatformError::IoError(format!("Failed to get the parent path of file {}.",
                                              file_path.display())));
  };
  match fs::create_dir_all(parent_path) {
    Ok(()) => {
      let mut prng: ChaChaRng;
      prng = ChaChaRng::from_seed([0u8; 32]);
      let key_pair = XfrKeyPair::generate(&mut prng);
      if let Err(error) = fs::write(&file_path, key_pair.zei_to_bytes()) {
        return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                  file_path.display(),
                                                  error)));
      };
    }
    Err(error) => {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                file_path.display(),
                                                error)));
    }
  }
  Ok(())
}

// Write a new public key to the given paths.
// Assumes tilde expansion has already been done on paths.
fn store_pub_key_to_file(path_str: &str) -> Result<(), PlatformError> {
  let file_path = Path::new(path_str);
  let parent_path = if let Some(path) = file_path.parent() {
    path
  } else {
    return Err(PlatformError::IoError(format!("Failed to get the parent path of file {}.",
                                              file_path.display())));
  };
  match fs::create_dir_all(parent_path) {
    Ok(()) => {
      let mut prng = ChaChaRng::from_seed([0u8; 32]);
      let key_pair = XfrKeyPair::generate(&mut prng);
      if let Err(error) = fs::write(&file_path, key_pair.get_pk_ref().as_bytes()) {
        return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                  file_path.display(),
                                                  error)));
      };
    }
    Err(error) => {
      return Err(PlatformError::IoError(format!("Failed to create directory for file {}: {}.",
                                                file_path.display(),
                                                error)));
    }
  }
  Ok(())
}

fn store_sids_to_file(file_path: &str, sids: &str) -> Result<(), PlatformError> {
  if let Err(error) = fs::write(file_path, sids) {
    return Err(PlatformError::IoError(format!("Failed to create file {}: {}.", file_path, error)));
  };
  Ok(())
}

fn store_blind_asset_record(file_path: &str,
                            amount: u64,
                            asset_type: &str,
                            pub_key_path: &str,
                            confidential_amount: bool,
                            confidential_asset: bool)
                            -> Result<(), PlatformError> {
  let mut asset_type_arr = [0u8; 16];
  let bytes = asset_type.as_bytes();
  asset_type_arr.copy_from_slice(&bytes[..16]);

  let asset_record =
    AssetRecord::new(amount,
                     asset_type_arr,
                     load_pub_key_from_file(pub_key_path)?).or_else(|error| {
                                                             Err(PlatformError::ZeiError(error))
                                                           })?;

  let blind_asset_record =
    build_blind_asset_record(&mut ChaChaRng::from_entropy(),
                             &PublicParams::new().pc_gens,
                             &asset_record,
                             AssetRecordType::from_booleans(confidential_amount,
                                                            confidential_asset),
                             &None);

  if let Ok(as_json) = serde_json::to_string(&blind_asset_record) {
    if let Err(error) = fs::write(file_path, &as_json) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                file_path, error)));
    };
    println!("Blind asset record stored to {}", file_path);
  }

  Ok(())
}

//
// Path related helper functions
//
fn create_directory_if_missing(path_to_file_in_dir: &str) -> Result<(), PlatformError> {
  let as_path = Path::new(path_to_file_in_dir);
  if as_path.exists() {
    return Ok(());
  }

  if let Some(parent) = as_path.parent() {
    if parent.exists() {
      return Ok(());
    }
    if let Err(error) = fs::create_dir_all(&parent) {
      return Err(PlatformError::IoError(format!("Failed to create directory for the parent path of {}: {}", path_to_file_in_dir, error)));
    }
  }

  Ok(())
}

const BACKUP_COUNT_MAX: i32 = 10000; // Arbitrary choice.

// Find a backup file name not currently in use.
// Assumes the extension of path can be replaced by n.
// Assumes it is safe to check the existence of the path after doing so.
// This implies all path components of path must exist and be readable.
// Assumes recursion won't hurt us here.
fn find_available_path(path: &Path, n: i32) -> Result<PathBuf, PlatformError> {
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

// Return a backup file path derived from path or an InputsError if an
// unused path cannot be derived. The path must not be empty
// and must not be dot (".").
fn next_path(path: &Path) -> Result<PathBuf, PlatformError> {
  fn add_backup_extension(path: &Path) -> Result<PathBuf, PlatformError> {
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
      Err(PlatformError::InputsError)
    } else if path.file_name() == None {
      println!("Is directory: {:?}. Specify a file path.", path);
      Err(PlatformError::InputsError)
    } else {
      find_available_path(&add_backup_extension(&path)?, 0)
    }
  }
}

fn rename_existing_path(path: &Path) -> Result<(), PlatformError> {
  let next = next_path(path)?;
  trace!("Next path for {:?} is {:?}", &path, &next);
  if let Err(error) = fs::rename(path, next.as_path()) {
    return Err(PlatformError::IoError(format!("Failed to rename path: {}", error)));
  }
  Ok(())
}

fn parse_to_u64(amount_arg: &str) -> Result<u64, PlatformError> {
  if let Ok(amount) = amount_arg.parse::<u64>() {
    Ok(amount)
  } else {
    println!("Improperly formatted number.");
    Err(PlatformError::InputsError)
  }
}

fn get_amounts(amounts_arg: &str) -> Result<Vec<u64>, PlatformError> {
  let amounts_str = split_arg(amounts_arg);
  let mut amounts = Vec::new();
  for amount_str in amounts_str {
    if let Ok(amount) = amount_str.trim().parse::<u64>() {
      amounts.push(amount);
    } else {
      return Err(PlatformError::InputsError);
    }
  }
  Ok(amounts)
}

fn define_asset(fiat_asset: bool,
                issuer_id: u64,
                token_code: AssetTypeCode,
                memo: &str,
                allow_updates: bool,
                traceable: bool,
                transaction_file_name: &str)
                -> Result<(), PlatformError> {
  let mut data = load_data()?;
  let issuer_key_pair = data.get_issuer_key_pair(issuer_id)?;
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation_create_asset(&issuer_key_pair,
                                         Some(token_code),
                                         allow_updates,
                                         traceable,
                                         &memo)?;
  store_txn_builder_to_file(&transaction_file_name, &txn_builder)?;

  // Update data
  if fiat_asset {
    data.fiat_code = Some(token_code.to_base64());
    store_data_to_file(data)
  } else {
    Ok(())
  }
}

fn run_ledger_standalone() -> Result<(), PlatformError> {
  thread::spawn(move || {
    let status = Command::new(LEDGER_STANDALONE).status();
    if status.is_err() {
      return Err(PlatformError::SubmissionServerError(Some("Failed to run ledger.".to_owned())));
    };
    Ok(())
  });
  Ok(())
}

fn submit(protocol: &str, host: &str, transaction_file_name: &str) -> Result<(), PlatformError> {
  // Submit transaction
  let txn_builder = load_txn_builder_from_file(transaction_file_name)?;
  let client = reqwest::Client::new();
  let txn = txn_builder.transaction();
  let mut res =
    client.post(&format!("{}://{}:{}/{}",
                         protocol, host, SUBMIT_PORT, "submit_transaction"))
          .json(&txn)
          .send()
          .or_else(|_| {
            Err(PlatformError::SubmissionServerError(Some("Failed to submit.".to_owned())))
          })?;
  // Log body
  println!("Submission response: {}",
           res.json::<TxnHandle>().expect("<Invalid JSON>"));
  println!("Submission status: {}", res.status());
  Ok(())
}

fn submit_and_get_sids(protocol: &str,
                       host: &str,
                       transaction_file_name: &str)
                       -> Result<Vec<TxoSID>, PlatformError> {
  // Submit transaction
  let txn_builder = load_txn_builder_from_file(transaction_file_name)?;

  let client = reqwest::Client::new();
  let txn = txn_builder.transaction();
  let mut res =
    client.post(&format!("{}://{}:{}/{}",
                         protocol, host, SUBMIT_PORT, "submit_transaction"))
          .json(&txn)
          .send()
          .or_else(|_| {
            Err(PlatformError::SubmissionServerError(Some("Failed to submit.".to_owned())))
          })?;

  // Log body
  let handle = res.json::<TxnHandle>().expect("<Invalid JSON>");
  println!("Submission response: {}", handle);
  println!("Submission status: {}", res.status());

  // Store and return sid
  let res = query(protocol, host, SUBMIT_PORT, "txn_status", &handle.0)?;
  match serde_json::from_str::<TxnStatus>(&res).or_else(|_| {
                                                 Err(PlatformError::DeserializationError)
                                               })? {
    TxnStatus::Committed((_sid, txos)) => Ok(txos),
    _ => Err(PlatformError::DeserializationError),
  }
}

fn query(protocol: &str,
         host: &str,
         port: &str,
         item: &str,
         value: &str)
         -> Result<String, PlatformError> {
  let mut res = if let Ok(response) =
    reqwest::get(&format!("{}://{}:{}/{}/{}", protocol, host, port, item, value))
  {
    response
  } else {
    return Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())));
  };

  // Log body
  println!("Querying status: {}", res.status());
  let text =
    res.text().or_else(|_| {
                 Err(PlatformError::SubmissionServerError(Some("Failed to query.".to_owned())))
               })?;
  println!("Querying result: {}", text);

  Ok(text)
}

fn get_blind_asset_record(pub_key: XfrPublicKey,
                          amount: u64,
                          token_code: AssetTypeCode,
                          confidential_amount: bool,
                          confidential_asset: bool)
                          -> Result<BlindAssetRecord, PlatformError> {
  let mut prng = ChaChaRng::from_seed([0u8; 32]);
  let params = PublicParams::new();
  let asset_record_type = AssetRecordType::from_booleans(confidential_amount, confidential_asset);
  let asset_record = match AssetRecord::new(amount, token_code.val, pub_key) {
    Ok(record) => record,
    Err(error) => {
      return Err(PlatformError::ZeiError(error));
    }
  };
  Ok(build_blind_asset_record(&mut prng,
                              &params.pc_gens,
                              &asset_record,
                              asset_record_type,
                              &None))
}

fn issue_and_transfer(issuer_key_pair: &XfrKeyPair,
                      recipient_key_pair: &XfrKeyPair,
                      amount: u64,
                      token_code: AssetTypeCode,
                      transaction_file_name: &str)
                      -> Result<(), PlatformError> {
  let blind_asset_record =
    get_blind_asset_record(issuer_key_pair.get_pk(), amount, token_code, false, false)?;

  // Transfer Operation
  let xfr_op =
    TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                              open_asset_record(&blind_asset_record,
                                                                issuer_key_pair.get_sk_ref())?,
                                              amount)?
                                   .add_output(amount, recipient_key_pair.get_pk_ref(), token_code)?
                                   .balance()?
                                   .create(TransferType::Standard)?
                                   .sign(issuer_key_pair)?
                                   .transaction()?;

  // Issue and Transfer transaction
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation_issue_asset(issuer_key_pair,
                                        &token_code,
                                        load_and_update_sequence_number()?,
                                        &[TxOutput(blind_asset_record)])?
             .add_operation(xfr_op)
             .transaction();

  store_txn_builder_to_file(&transaction_file_name, &txn_builder)
}

fn merge_records(key_pair: &XfrKeyPair,
                 sid1: TxoRef,
                 sid2: TxoRef,
                 blind_asset_record1: BlindAssetRecord,
                 blind_asset_record2: BlindAssetRecord,
                 token_code: AssetTypeCode,
                 transaction_file_name: &str)
                 -> Result<(), PlatformError> {
  let oar1 = open_asset_record(&blind_asset_record1, key_pair.get_sk_ref())?;
  let oar2 = open_asset_record(&blind_asset_record2, key_pair.get_sk_ref())?;
  let amount1 = *oar1.get_amount();
  let amount2 = *oar2.get_amount();

  // Transfer Operation
  let xfr_op =
    TransferOperationBuilder::new().add_input(sid1, oar1, amount1)?
                                   .add_input(sid2, oar2, amount2)?
                                   .add_output(amount1 + amount2,
                                               key_pair.get_pk_ref(),
                                               token_code)?
                                   .create(TransferType::Standard)?
                                   .sign(key_pair)?
                                   .transaction()?;

  // Merge records
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation(xfr_op).transaction();
  store_txn_builder_to_file(&transaction_file_name, &txn_builder)
}

fn load_funds(issuer_id: u64,
              recipient_id: u64,
              amount: u64,
              transaction_file_name: &str,
              protocol: &str,
              host: &str)
              -> Result<(), PlatformError> {
  // Get data
  let mut data = load_data()?;
  let issuer_key_pair = &data.clone().get_issuer_key_pair(issuer_id)?;
  let recipient = &data.borrowers.clone()[recipient_id as usize];
  let recipient_key_pair = &data.clone().get_borrower_key_pair(recipient_id)?;

  // Get or define fiat asset
  let token_code = if let Some(code) = &data.clone().fiat_code {
    AssetTypeCode::new_from_base64(code)?
  } else {
    let fiat_code = AssetTypeCode::gen_random();
    define_asset(true,
                 issuer_id,
                 fiat_code,
                 "Fiat asset",
                 false,
                 false,
                 transaction_file_name)?;
    // Store data before submitting the transaction to avoid data overwriting
    let data = load_data()?;
    submit(protocol, host, transaction_file_name)?;
    store_data_to_file(data)?;
    fiat_code
  };

  // Issue and transfer asset
  issue_and_transfer(issuer_key_pair,
                     recipient_key_pair,
                     amount,
                     token_code,
                     transaction_file_name)?;

  // Submit transaction and get the new record
  let sid_new = submit_and_get_sids(protocol, host, transaction_file_name)?[0];
  let res_new = query(protocol,
                      host,
                      QUERY_PORT,
                      "utxo_sid",
                      &format!("{}", sid_new.0))?;
  let blind_asset_record_new =
    serde_json::from_str::<BlindAssetRecord>(&res_new).or_else(|_| {
                                                        Err(PlatformError::DeserializationError)
                                                      })?;

  // Merge records
  let sid_merged = if let Some(sid_pre) = recipient.utxo {
    let res_pre = query(protocol,
                        host,
                        QUERY_PORT,
                        "utxo_sid",
                        &format!("{}", sid_pre.0))?;
    let blind_asset_record_pre =
      serde_json::from_str::<BlindAssetRecord>(&res_pre).or_else(|_| {
                                                          Err(PlatformError::DeserializationError)
                                                        })?;
    merge_records(recipient_key_pair,
                  TxoRef::Absolute(sid_pre),
                  TxoRef::Absolute(sid_new),
                  blind_asset_record_pre,
                  blind_asset_record_new,
                  token_code,
                  transaction_file_name)?;

    submit_and_get_sids(protocol, host, transaction_file_name)?[0]
  } else {
    sid_new
  };

  // Update data
  data = load_data()?;
  data.borrowers[recipient_id as usize].balance = recipient.balance + amount;
  data.borrowers[recipient_id as usize].utxo = Some(sid_merged);
  store_data_to_file(data)
}

// Get the blind asset record by querying the utxo sid
// Construct the open asset record with the blind asset record and the secret key
fn get_open_asset_record(protocol: &str,
                         host: &str,
                         sid: TxoSID,
                         key_pair: &XfrKeyPair)
                         -> Result<OpenAssetRecord, PlatformError> {
  let res = query(protocol,
                  host,
                  QUERY_PORT,
                  "utxo_sid",
                  &format!("{}", sid.0))?;
  let blind_asset_record =
    serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                    Err(PlatformError::DeserializationError)
                                                  })?;
  open_asset_record(&blind_asset_record, key_pair.get_sk_ref()).or_else(|error| {
                                                                 Err(PlatformError::ZeiError(error))
                                                               })
}

enum RelationType {
  // // Requirement: attribute value == requirement
  // Equal,

  // Requirement: attribute value >= requirement
  AtLeast,
}

fn prove(reveal_sig: &ACRevealSig,
         ac_issuer_pk: &ACIssuerPublicKey,
         value: u64,
         requirement: u64,
         relation_type: RelationType)
         -> Result<(), PlatformError> {
  // 1. Prove that the attribut meets the requirement
  match relation_type {
    // //    Case 1. "Equal" requirement
    // //    E.g. prove that the country code is the same as the requirement
    // RelationType::Equal => {
    //   if value != requirement {
    //     println!("Value should be: {}.", requirement);
    //     return Err(PlatformError::InputsError);
    //   }
    // }
    //    Case 2. "AtLeast" requirement
    //    E.g. prove that the credit score is at least the required value
    RelationType::AtLeast => {
      if value < requirement {
        println!("Value should be at least: {}.", requirement);
        return Err(PlatformError::InputsError);
      }
    }
  }

  // 2. Prove that the attribute is true
  //    E.g. verify the lower bound of the credit score
  let attrs = [Some(value.to_le_bytes())];
  ac_verify(ac_issuer_pk,
            &attrs,
            &reveal_sig.sig_commitment,
            &reveal_sig.pok).or_else(|error| Err(PlatformError::ZeiError(error)))
}

// Issues and transfers fiat and debt token to the lender and borrower, respectively
// Then activate the loan
fn activate_loan(loan_id: u64,
                 issuer_id: u64,
                 transaction_file_name: &str,
                 protocol: &str,
                 host: &str)
                 -> Result<(), PlatformError> {
  // Get data
  let mut data = load_data()?;
  let issuer_key_pair = &data.clone().get_issuer_key_pair(issuer_id)?;
  let loan = &data.loans.clone()[loan_id as usize];
  if loan.active {
    println!("Loan {} has already been activated.", loan_id);
    return Err(PlatformError::InputsError);
  }
  if loan.rejected {
    println!("Loan {} has already been rejected.", loan_id);
    return Err(PlatformError::InputsError);
  }

  let lender_id = loan.lender;
  let lender = &data.lenders.clone()[lender_id as usize];
  let lender_key_pair = &data.get_lender_key_pair(loan.lender)?;
  let borrower_id = loan.borrower;
  let borrower = &data.borrowers.clone()[borrower_id as usize];
  let borrower_key_pair = &data.get_borrower_key_pair(borrower_id)?;
  let amount = loan.amount;

  // Credential check
  // TODO (Keyao): Add requirements about other credential attributes, and attest them too
  let credential_id =
    if let Some(id) = borrower.credentials[CredentialIndex::MinCreditScore as usize] {
      id as usize
    } else {
      println!("Minimum credit score is required. Use create credential.");
      return Err(PlatformError::InputsError);
    };
  let credential = &data.credentials.clone()[credential_id as usize];
  let requirement = lender.min_credit_score;

  // If the proof exists and the proved value is valid, attest with the proof
  // Otherwise, prove and attest the value
  if let Some(proof) = &credential.proof {
    println!("Attesting with the existing proof.");
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let issuer_pk = ac_keygen_issuer::<_>(&mut prng, 1).0;
    if let Err(error) =
      prove(&serde_json::from_str::<ACRevealSig>(proof).or_else(|_| {
                                                         Err(PlatformError::DeserializationError)
                                                       })?,
            &issuer_pk,
            credential.value,
            requirement,
            RelationType::AtLeast)
    {
      // Update loans data
      data.loans[loan_id as usize].rejected = true;
      store_data_to_file(data)?;
      return Err(error);
    }
  } else {
    println!("Proving before attesting.");
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, 1);
    let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer_pk.clone());

    let value = credential.value;
    let attributes = [value.to_le_bytes()].to_vec();
    let signature = ac_sign(&mut prng, &issuer_sk, &user_pk, &attributes);
    let zei_credential = ZeiCredential { signature,
                                         attributes,
                                         issuer_pk: issuer_pk.clone() };

    let reveal_sig =
      ac_reveal(&mut prng, &user_sk, &zei_credential, &[true]).or_else(|error| {
                                                                Err(PlatformError::ZeiError(error))
                                                              })?;

    prove(&reveal_sig,
          &issuer_pk,
          value,
          requirement,
          RelationType::AtLeast)?;

    // Update credentials data
    data.credentials[credential_id as usize].proof =
      Some(serde_json::to_string(&reveal_sig).or_else(|_| Err(PlatformError::SerializationError))?);
    store_data_to_file(data)?;
    data = load_data()?;
  }

  // Get or define fiat asset
  let fiat_code = if let Some(code) = data.fiat_code {
    println!("Fiat code: {}", code);
    AssetTypeCode::new_from_base64(&code)?
  } else {
    let fiat_code = AssetTypeCode::gen_random();
    define_asset(true,
                 issuer_id,
                 fiat_code,
                 "Fiat asset",
                 false,
                 false,
                 transaction_file_name)?;
    // Store data before submitting the transaction to avoid data overwriting
    let data = load_data()?;
    submit(protocol, host, transaction_file_name)?;
    store_data_to_file(data)?;
    fiat_code
  };

  // Issue and transfer fiat token
  issue_and_transfer(issuer_key_pair,
                     lender_key_pair,
                     amount,
                     fiat_code,
                     transaction_file_name)?;
  let fiat_sid = submit_and_get_sids(protocol, host, transaction_file_name)?[0];
  println!("Fiat sid: {}", fiat_sid.0);
  let fiat_open_asset_record = get_open_asset_record(protocol, host, fiat_sid, lender_key_pair)?;

  // Define debt asset
  let mut txn_builder = TransactionBuilder::default();
  let debt_code = AssetTypeCode::gen_random();
  println!("Generated debt code: {}",
           serde_json::to_string(&debt_code.val).or_else(|_| {
                                                  Err(PlatformError::SerializationError)
                                                })?);
  let memo = DebtMemo { interest_rate: Fraction::new(loan.interest_per_mille, 1000),
                        fiat_code,
                        loan_amount: amount };
  let memo_str = serde_json::to_string(&memo).or_else(|_| Err(PlatformError::SerializationError))?;
  if let Err(e) = txn_builder.add_operation_create_asset(&borrower_key_pair,
                                                         Some(debt_code),
                                                         false,
                                                         false,
                                                         &memo_str)
  {
    println!("Failed to add operation to transaction.");
    return Err(e);
  }
  store_txn_builder_to_file(&transaction_file_name, &txn_builder)?;
  // Store data before submitting the transaction to avoid data overwriting
  let data = load_data()?;
  submit(protocol, host, transaction_file_name)?;
  store_data_to_file(data)?;

  // Issue and transfer debt token
  issue_and_transfer(borrower_key_pair,
                     borrower_key_pair,
                     amount,
                     debt_code,
                     transaction_file_name)?;
  let debt_sid = submit_and_get_sids(protocol, host, transaction_file_name)?[0];
  println!("Fiat sid: {}", debt_sid.0);
  let debt_open_asset_record = get_open_asset_record(protocol, host, debt_sid, borrower_key_pair)?;

  // Initiate loan
  let xfr_op =
    TransferOperationBuilder::new().add_input(TxoRef::Absolute(fiat_sid),
                                              fiat_open_asset_record,
                                              amount)?
                                   .add_input(TxoRef::Absolute(debt_sid),
                                              debt_open_asset_record,
                                              amount)?
                                   .add_output(amount, lender_key_pair.get_pk_ref(), debt_code)?
                                   .add_output(amount, borrower_key_pair.get_pk_ref(), fiat_code)?
                                   .create(TransferType::Standard)?
                                   .sign(lender_key_pair)?
                                   .sign(borrower_key_pair)?
                                   .transaction()?;
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation(xfr_op).transaction();
  store_txn_builder_to_file(&transaction_file_name, &txn_builder)?;

  // Submit transaction and get the new record
  let sids_new = submit_and_get_sids(protocol, host, transaction_file_name)?;
  let res_new = query(protocol,
                      host,
                      QUERY_PORT,
                      "utxo_sid",
                      &format!("{}", sids_new[1].0))?;
  let blind_asset_record_new =
    serde_json::from_str::<BlindAssetRecord>(&res_new).or_else(|_| {
                                                        Err(PlatformError::DeserializationError)
                                                      })?;

  // Merge records
  let fiat_sid_merged = if let Some(sid_pre) = borrower.utxo {
    let res_pre = query(protocol,
                        host,
                        QUERY_PORT,
                        "utxo_sid",
                        &format!("{}", sid_pre.0))?;
    let blind_asset_record_pre =
      serde_json::from_str::<BlindAssetRecord>(&res_pre).or_else(|_| {
                                                          Err(PlatformError::DeserializationError)
                                                        })?;
    merge_records(borrower_key_pair,
                  TxoRef::Absolute(sid_pre),
                  TxoRef::Absolute(sids_new[1]),
                  blind_asset_record_pre,
                  blind_asset_record_new,
                  fiat_code,
                  transaction_file_name)?;
    submit_and_get_sids(protocol, host, transaction_file_name)?[0]
  } else {
    sids_new[1]
  };

  println!("New debt utxo sid: {}, fiat utxo sid: {}.",
           sids_new[0].0, fiat_sid_merged.0);

  // Update data
  let mut data = load_data()?;
  data.fiat_code = Some(fiat_code.to_base64());
  data.loans[loan_id as usize].active = true;
  data.loans[loan_id as usize].code = Some(debt_code.to_base64());
  data.loans[loan_id as usize].utxo = Some(sids_new[0]);
  data.borrowers[borrower_id as usize].balance = borrower.balance + amount;
  data.borrowers[borrower_id as usize].utxo = Some(fiat_sid_merged);
  store_data_to_file(data)
}

// Pay loan with certain amount
fn pay_loan(loan_id: u64,
            amount: u64,
            transaction_file_name: &str,
            protocol: &str,
            host: &str)
            -> Result<(), PlatformError> {
  // Get data
  let mut data = load_data()?;
  let loan = &data.loans.clone()[loan_id as usize];

  // Check if the loan has been activated
  if !loan.active {
    println!("Loan {} hasn't been activated yet. Use active_loan to activate the loan.",
             loan_id);
    return Err(PlatformError::InputsError);
  }
  let lender_id = loan.lender;
  let borrower_id = loan.borrower;
  let borrower = &data.borrowers.clone()[borrower_id as usize];
  let lender_key_pair = &data.get_lender_key_pair(lender_id)?;
  let borrower_key_pair = &data.get_borrower_key_pair(borrower_id)?;

  // Check if funds are sufficient
  if amount > borrower.balance {
    println!("Insufficient funds. Use --load_funds to load more funds.");
    return Err(PlatformError::InputsError);
  }

  // Check if the amount meets the minimum requirement, i.e., the fee
  let fee =
    ledger::policies::calculate_fee(loan.balance, Fraction::new(loan.interest_per_mille, 1000));
  if amount < fee {
    println!("Payment amount should be at least: {}", fee);
    return Err(PlatformError::InputsError);
  }

  // Get the amount to burn the balance, and the total amount the borrow will spend
  let mut amount_to_burn = amount - fee;
  if amount_to_burn > loan.balance {
    println!("Paying {} is enough.", loan.balance);
    amount_to_burn = loan.balance;
  }
  let amount_to_spend = amount_to_burn + fee;
  println!("The borrower will spend {} to burn {}.",
           amount_to_spend, amount_to_burn);

  // Get fiat and debt sids
  let fiat_sid = if let Some(sid) = borrower.utxo {
    sid
  } else {
    println!("Missing fiat utxo in the borrower record. Try --activate_loan.");
    return Err(PlatformError::InputsError);
  };
  let debt_sid = if let Some(sid) = loan.utxo {
    sid
  } else {
    println!("Missing debt utxo in the loan record. Try --activate_loan.");
    return Err(PlatformError::InputsError);
  };
  let fiat_open_asset_record = get_open_asset_record(protocol, host, fiat_sid, lender_key_pair)?;
  let debt_open_asset_record = get_open_asset_record(protocol, host, debt_sid, borrower_key_pair)?;

  // Get fiat and debt codes
  let fiat_code = if let Some(code) = data.clone().fiat_code {
    AssetTypeCode::new_from_base64(&code)?
  } else {
    println!("Missing fiat code. Try --active_loan.");
    return Err(PlatformError::InputsError);
  };
  let debt_code = if let Some(code) = &loan.code {
    AssetTypeCode::new_from_base64(&code)?
  } else {
    println!("Missing debt code in the loan record. Try --activate_loan.");
    return Err(PlatformError::InputsError);
  };

  println!("Fiat code: {}", serde_json::to_string(&fiat_code.val)?);
  println!("Debt code: {}", serde_json::to_string(&debt_code.val)?);

  let op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(debt_sid),
                                                     debt_open_asset_record,
                                                     amount_to_burn)?
                                          .add_input(TxoRef::Absolute(fiat_sid),
                                                     fiat_open_asset_record,
                                                     amount_to_spend)?
                                          .add_output(amount_to_spend,
                                                      lender_key_pair.get_pk_ref(),
                                                      fiat_code)?
                                          .add_output(amount_to_burn,
                                                      &XfrPublicKey::zei_from_bytes(&[0; 32]),
                                                      debt_code)?
                                          .add_output(loan.amount - amount_to_burn,
                                                      lender_key_pair.get_pk_ref(),
                                                      debt_code)?
                                          .add_output(loan.amount - amount_to_spend,
                                                      borrower_key_pair.get_pk_ref(),
                                                      fiat_code)?
                                          .create(TransferType::DebtSwap)?
                                          .sign(borrower_key_pair)?
                                          .transaction()?;
  let mut txn_builder = TransactionBuilder::default();
  txn_builder.add_operation(op).transaction();
  store_txn_builder_to_file(&transaction_file_name, &txn_builder)?;

  // Submit transaction and update data
  let sids = submit_and_get_sids(protocol, host, transaction_file_name)?;

  data = load_data()?;
  data.loans[loan_id as usize].balance = loan.balance - amount_to_burn;
  data.loans[loan_id as usize].payments = loan.payments + 1;
  data.loans[loan_id as usize].utxo = Some(sids[2]);
  data.borrowers[borrower_id as usize].balance = borrower.balance - amount_to_spend;
  data.borrowers[borrower_id as usize].utxo = Some(sids[3]);

  store_data_to_file(data)
}

// Use environment variable RUST_LOG to select log level and filter
// output by module or regex. For example,
//
//    RUST_LOG=ledger::data_model=info,main=trace/rec[ie]+ve ./main
//
// TODO Verify that this comment is correct.
//
// By default, log everything "trace" level or greater to stdout.
// TODO switch to using from_default_env()
fn init_logging() {
  env_logger::from_env(Env::default().default_filter_or("trace")).target(Target::Stdout)
                                                                 .init();
}

/// Match the PlatformError with an exitcode and exit
/// 1.  SerializationError: exit with code DATAERR
/// 2.  DeserializationError: exit with code DATAERR
/// 3.  IoError:
///     3.1 If the input file doesn't exist: exit with code NOINPUT
///         Note: make sure the error message contains "File doesn't exist:" when constructing the PlatformError
///     3.2 If the input file isn't readable: exit with code NOINPUT
///         Note: make sure the error message contains "Failed to read" when constructing the PlatformError
///     3.3 If the output file or directory can't be created: exit with code CANTCREAT
///         Note: make sure the error message contains "Failed to create" when constructing the PlatformError
///     3.4 Otherwise: exit with code IOERR
/// 4. SubmissionServerError: exit with code UNAVAILABLE
/// 5. Otherwise: exit with code USAGE
fn match_error_and_exit(error: PlatformError) {
  match error {
    PlatformError::SerializationError => exit(exitcode::DATAERR),
    PlatformError::DeserializationError => exit(exitcode::DATAERR),
    PlatformError::IoError(io_error) => {
      if io_error.contains("File doesn't exist:") || io_error.contains("Failed to read") {
        exit(exitcode::NOINPUT)
      }
      if io_error.contains("Failed to create") {
        exit(exitcode::CANTCREAT)
      }
      exit(exitcode::IOERR)
    }
    _ => exit(exitcode::USAGE),
  }
}

/// If the function process_inputs returns an error, for different types of error:
/// SerializationError or DeserializationError: exit with code DATAERR
/// IoError: exit with code IOERR
/// Other types (e.g. InputsError): exit with code USAGE
fn main() {
  init_logging();
  let inputs = App::new("Transaction Builder")
    .version("0.0.1")
    .about("Copyright 2019 © Findora. All rights reserved.")
    .arg(Arg::with_name("config")
      .short("c")
      .long("config")
      .value_name("PATH/TO/FILE")
      .help("Specify a custom config file (default: \"$FINDORA_DIR/config.toml\")")
      .takes_value(true))
    .arg(Arg::with_name("findora_dir")
      .short("d")
      .long("dir")
      .value_name("PATH")
      .help("Directory for configuaration, security, and temporary files; must be writable")
      .takes_value(true)
      .env("FINDORA_DIR"))
    .arg(Arg::with_name("key_pair_path")
      .short("k")
      .long("key_pair")
      .value_name("PATH/TO/FILE")
      .help("Path to key pair)")
      .takes_value(true))
    .arg(Arg::with_name("txn")
      .long("txn")
      .value_name("FILE")
      .help("Use a name transaction file (will always be under findora_dir)")
      .takes_value(true))
    .subcommand(SubCommand::with_name("issuer")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Issuer's name."))))
    .subcommand(SubCommand::with_name("lender")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Lender's name."))
        .arg(Arg::with_name("min_credit_score")
          .short("m")
          .long("min_credit_score")
          .required(true)
          .takes_value(true)
          .help("Minimum credit score requirement.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Lender id."))
      .subcommand(SubCommand::with_name("view_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .takes_value(true)
          .help("Display the loan with the specified id only."))
        .arg(Arg::with_name("filter")
          .short("f")
          .long("filter")
          .takes_value(true)
          .possible_values(&["active", "inactive", "unrejected"])
          .help("Display the loan with the specified status only."))
        .help("By default, display all loans of this lender."))
      .subcommand(SubCommand::with_name("activate_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .required(true)
          .takes_value(true)
          .help("Loan id."))
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Issuer id."))
        .arg(Arg::with_name("protocol")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("host")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used."))))
    .subcommand(SubCommand::with_name("borrower")
      .subcommand(SubCommand::with_name("sign_up")
        .arg(Arg::with_name("name")
          .short("n")
          .long("name")
          .required(true)
          .takes_value(true)
          .help("Borrower's name.")))
      .arg(Arg::with_name("id")
        .short("i")
        .long("id")
        .takes_value(true)
        .help("Borrower id."))
      .subcommand(SubCommand::with_name("load_funds")
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .takes_value(true)
          .help("Required: issuer id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer to the recipient."))
        .arg(Arg::with_name("protocol")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("host")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used.")))
      .subcommand(SubCommand::with_name("view_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .takes_value(true)
          .help("Display the loan with the specified id only."))
        .arg(Arg::with_name("filter")
          .short("f")
          .long("filter")
          .takes_value(true)
          .possible_values(&["active", "inactive", "unrejected", "completed"])
          .help("Display the loan with the specified status only."))
        .help("By default, display all loans of this borrower."))
      .subcommand(SubCommand::with_name("create_loan")
        .arg(Arg::with_name("lender")
          .short("l")
          .long("lender")
          .required(true)
          .takes_value(true)
          .help("Lender id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of the loan."))
        .arg(Arg::with_name("interest_per_mille")
          .short("i")
          .long("interest_per_mille")
          .required(true)
          .takes_value(true)
          .help("Interest per mille. The interest rate will be interest_per_mille/1000."))
        .arg(Arg::with_name("duration")
          .short("d")
          .long("duration")
          .required(true)
          .takes_value(true)
          .help("Payment duration")))
      .subcommand(SubCommand::with_name("pay_loan")
        .arg(Arg::with_name("loan")
          .short("l")
          .long("loan")
          .required(true)
          .takes_value(true)
          .help("Loan id."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Payment amount."))
        .arg(Arg::with_name("protocol")
          .long("http")
          .takes_value(false)
          .help("Specify that http, not https should be used."))
        .arg(Arg::with_name("host")
          .long("localhost")
          .takes_value(false)
          .help("Specify that localhost, not testnet.findora.org should be used.")))
      .subcommand(SubCommand::with_name("view_credential")
        .arg(Arg::with_name("credential")
          .short("c")
          .long("credential")
          .takes_value(true)
          .help("Display the credential with the specified id only."))
        .help("By default, display all credentials of this borrower."))
      .subcommand(SubCommand::with_name("create_or_overwrite_credential")
        .arg(Arg::with_name("attribute")
          .short("a")
          .long("attribute")
          .required(true)
          .takes_value(true)
          .possible_values(&["min_credit_score", "min_income", "citizenship"])
          .help("Credential attribute."))
        .arg(Arg::with_name("value")
          .short("v")
          .long("value")
          .required(true)
          .takes_value(true)
          .help("Value of the credential record."))
        .help("Create or overwrite a credential record.")))
    .subcommand(SubCommand::with_name("create_txn_builder")
      .about("By default, will rename previous file with a .<number> suffix")
      .arg(Arg::with_name("name")
        .short("n")
        .long("name")
        .value_name("FILE")
        .help("Specify a name for newly created transaction file")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("store")
      .subcommand(SubCommand::with_name("sids")
        .arg(Arg::with_name("path")
          .short("p")
          .long("path")
          .takes_value(true)
          .help("Path to store the sids. If not specified, a default path will be given."))
        .arg(Arg::with_name("overwrite")
          .long("force")
          .alias("overwrite")
          .short("f")
          .help("If specified, the existing file with the same name will be overwritten."))
        .arg(Arg::with_name("indices")
          .short("is")
          .long("indices")
          .required(true)
          .takes_value(true)
          .help("Input TxoSID indices. Separate by comma (\",\")")))
      .subcommand(SubCommand::with_name("blind_asset_record")
        .arg(Arg::with_name("path")
          .short("p")
          .long("path")
          .takes_value(true)
          .help("Path to store the blind asset record. If not specified, a default path will be given."))
        .arg(Arg::with_name("overwrite")
          .long("force")
          .alias("overwrite")
          .short("f")
          .help("If specified, the existing file with the same name will be overwritten."))
        .arg(Arg::with_name("amount")
          .short("a")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Asset amount"))
        .arg(Arg::with_name("asset_type")
          .short("t")
          .long("asset_type")
          .required(true)
          .takes_value(true)
          .help("String representation of the asset type"))
        .arg(Arg::with_name("pub_key_path")
          .short("k")
          .long("pub_key_path")
          .required(true)
          .takes_value(true)
          .help("Path to the public key"))
        .arg(Arg::with_name("confidential_amount")
          .short("m")
          .long("confidential_amount")
          .help("If specified, the amount will be confidential"))
        .arg(Arg::with_name("confidential_asset")
          .short("s")
          .long("confidential_asset")
          .help("If specified, the asset will be confidential"))))
    .subcommand(SubCommand::with_name("add")
      .subcommand(SubCommand::with_name("define_asset")
        .arg(Arg::with_name("fiat")
          .short("f")
          .long("fiat")
          .takes_value(false)
          .help("Indicate the asset is a fiat asset."))
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Issuer id."))
        .arg(Arg::with_name("token_code")
          .long("token_code")
          .short("c")
          .help("Explicit 16 character token code for the new asset; must be a unique name. If specified code is already in use, transaction will fail. If not specified, will display automatically generated token code.")
          .takes_value(true))
        .arg(Arg::with_name("allow_updates")
          .short("u")
          .long("allow_updates")
          .help("If specified, updates may be made to asset memo"))
        .arg(Arg::with_name("traceable")
          .short("trace")
          .long("traceable")
          .help("If specified, asset transfers can be traced by the issuer "))
        .arg(Arg::with_name("memo")
          .short("m")
          .long("memo")
          .required(true)
          .takes_value(true)
          .help("Memo as Json, with escaped quotation marks"))
        .arg(Arg::with_name("confidential")
          .short("xx")
          .long("confidential")
          .help("Make memo confidential"))
        .arg(Arg::with_name("with_policy")
          .short("p")
          .help("TODO: add support for policies")))
      .subcommand(SubCommand::with_name("issue_asset")
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Issuer id."))
        .arg(Arg::with_name("token_code")
          .short("c")
          .long("token_code")
          .required(true)
          .takes_value(true)
          .help("Token code of the asset to be issued. The transaction will fail if no asset with the token code exists."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of tokens to issue.")))
      .subcommand(SubCommand::with_name("transfer_asset")
        .arg(Arg::with_name("issuer")
          .short("d")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Issuer id."))
        .arg(Arg::with_name("sids_path")
          .short("ssp")
          .long("sids_path")
          .required(true)
          .takes_value(true)
          .help("Path to the file where input TxoSID indices are stored."))
        .arg(Arg::with_name("blind_asset_record_paths")
          .short("barps")
          .long("blind_asset_record_paths")
          .required(true)
          .takes_value(true)
          .help("Path to the files where blind asset records are stored. Separate by comma (\",\")."))
        .arg(Arg::with_name("input_amounts")
          .short("iamts")
          .long("input_amounts")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer from each record. Separate by comma (\",\")."))
        .arg(Arg::with_name("output_amounts")
          .short("oamts")
          .long("output_amounts")
          .required(true)
          .takes_value(true)
          .help("Amount to transfer to each account. Separate by comma (\",\")."))
        .arg(Arg::with_name("address_paths")
          .short("asp")
          .long("address_paths")
          .required(true)
          .takes_value(true)
          .help("Path to the files where address keys are stored. If no such file, try pubkeygen subcommand.")))
      .subcommand(SubCommand::with_name("issue_and_transfer_asset")
        .arg(Arg::with_name("issuer")
          .short("i")
          .long("issuer")
          .required(true)
          .takes_value(true)
          .help("Issuer id."))
        .arg(Arg::with_name("recipient")
          .short("r")
          .long("recipient")
          .required(true)
          .takes_value(true)
          .help("Recipient's id."))
        .arg(Arg::with_name("amount")
          .short("amt")
          .long("amount")
          .required(true)
          .takes_value(true)
          .help("Amount of tokens to issue and transfer."))
        .arg(Arg::with_name("token_code")
          .short("tc")
          .long("token_code")
          .required(true)
          .takes_value(true)
          .help("Token code of the asset."))))
    .subcommand(SubCommand::with_name("serialize"))
    .subcommand(SubCommand::with_name("drop"))
    .subcommand(SubCommand::with_name("keygen")
      .arg(Arg::with_name("create_key_pair_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the key pair file.")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("pubkeygen")
      .arg(Arg::with_name("create_pubkey_path")
        .short("n")
        .long("name")
        .help("specify the path and name for the public key file.")
        .takes_value(true))
      .arg(Arg::with_name("overwrite")
        .long("force")
        .alias("overwrite")
        .short("f")
        .help("If specified, the existing file with the same name will be overwritten.")))
    .subcommand(SubCommand::with_name("submit")
      .arg(Arg::with_name("protocol")
        .long("http")
        .takes_value(false)
        .help("Specify that http, not https should be used."))
      .arg(Arg::with_name("host")
        .long("localhost")
        .takes_value(false)
        .help("Specify that localhost, not testnet.findora.org should be used.")))
    .get_matches();
  if let Err(error) = process_inputs(inputs) {
    match_error_and_exit(error);
  }
}

fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
  let _config_file_path: String;
  let transaction_file_name: String;
  let findora_dir = if let Some(dir) = inputs.value_of("findora_dir") {
    dir.to_string()
  } else if let Ok(dir) = env::var("FINDORA_DIR") {
    dir
  } else {
    let home_dir = if let Some(dir) = dirs::home_dir() {
      dir
    } else {
      return Err(PlatformError::IoError("Failed to get the home directory.".to_owned()));
    };
    let dir_str = if let Some(string) = home_dir.to_str() {
      string
    } else {
      return Err(PlatformError::IoError("Failed to convert the path to string.".to_owned()));
    };
    format!("{}/.findora", dir_str)
  };

  if let Some(cfg) = inputs.value_of("config") {
    _config_file_path = cfg.to_string();
  } else {
    _config_file_path = format!("{}/config.toml", findora_dir);
  }

  if let Some(txn_store) = inputs.value_of("txn") {
    transaction_file_name = txn_store.to_string();
  } else {
    transaction_file_name = format!("{}/txn/default.txn", findora_dir);
  }

  match inputs.subcommand() {
    ("issuer", Some(issuer_matches)) => process_issuer_cmd(issuer_matches),
    ("lender", Some(issuer_matches)) => process_lender_cmd(issuer_matches, &transaction_file_name),
    ("borrower", Some(issuer_matches)) => {
      process_borrower_cmd(issuer_matches, &transaction_file_name)
    }
    ("create_txn_builder", Some(create_txn_builder_matches)) => {
      process_create_txn_builder_cmd(create_txn_builder_matches,
                                     &transaction_file_name,
                                     &findora_dir)
    }
    ("store", Some(store_matches)) => process_store_cmd(store_matches, &findora_dir),
    ("add", Some(add_matches)) => process_add_cmd(add_matches, &transaction_file_name),
    ("serialize", Some(_serialize_matches)) => {
      let txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                          println!("Failed to load txn builder from file {}.",
                                   transaction_file_name);
                          Err(e)
                        })?;
      match serde_json::to_string(txn_builder.transaction()) {
        Ok(as_json) => {
          println!("{}", as_json);
          Ok(())
        }
        Err(_) => {
          println!("Failed to serialize txn.");
          Err(PlatformError::SerializationError)
        }
      }
    }
    ("drop", Some(_drop_matches)) => match std::fs::remove_file(&transaction_file_name) {
      Ok(_) => {
        println!("Deleted transaction file {}", transaction_file_name);
        Ok(())
      }
      Err(e) => Err(PlatformError::IoError(format!("Error deleting file: {:?} ", e))),
    },
    ("keygen", Some(keygen_matches)) => {
      let new_key_pair_path =
        if let Some(new_key_pair_path_in) = keygen_matches.value_of("create_key_pair_path") {
          new_key_pair_path_in.to_string()
        } else {
          format!("{}/keypair/default.keypair", &findora_dir)
        };
      let expand_str = shellexpand::tilde(&new_key_pair_path).to_string();
      let overwrite = keygen_matches.is_present("overwrite");
      println!("Storing key pair to {}", expand_str);
      create_directory_and_rename_path(&expand_str, overwrite)?;
      store_key_pair_to_file(&expand_str)
    }
    ("pubkeygen", Some(pubkeygen_matches)) => {
      let new_key_path =
        if let Some(new_key_path_in) = pubkeygen_matches.value_of("create_pubkey_path") {
          new_key_path_in.to_string()
        } else {
          format!("{}/pubkey/default.pubkey", &findora_dir)
        };
      let expand_str = shellexpand::tilde(&new_key_path).to_string();
      println!("Storing public key to {}", expand_str);
      let overwrite = pubkeygen_matches.is_present("overwrite");
      create_directory_and_rename_path(&expand_str, overwrite)?;
      store_pub_key_to_file(&expand_str)
    }
    ("submit", Some(submit_matches)) => process_submit_cmd(submit_matches, &transaction_file_name),
    _ => {
      println!("Subcommand missing or not recognized. Try --help");
      Err(PlatformError::InputsError)
    }
  }
}

fn process_issuer_cmd(issuer_matches: &clap::ArgMatches) -> Result<(), PlatformError> {
  match issuer_matches.subcommand() {
    ("sign_up", Some(sign_up_matches)) => {
      let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
        name_arg.to_owned()
      } else {
        println!("Name is required to sign up an issuer account. Use --name.");
        return Err(PlatformError::InputsError);
      };
      let mut data = load_data()?;
      data.add_issuer(name)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try lender --help");
      Err(PlatformError::InputsError)
    }
  }
}

fn process_lender_cmd(lender_matches: &clap::ArgMatches,
                      transaction_file_name: &str)
                      -> Result<(), PlatformError> {
  let mut data = load_data()?;
  match lender_matches.subcommand() {
    ("sign_up", Some(sign_up_matches)) => {
      let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
        name_arg.to_owned()
      } else {
        println!("Name is required to sign up a lender account. Use --name.");
        return Err(PlatformError::InputsError);
      };
      let min_credit_score = if let Some(min_credit_score_arg) =
        sign_up_matches.value_of("min_credit_score")
      {
        parse_to_u64(min_credit_score_arg)?
      } else {
        println!("Minimum credit score requirement is required to sign up a lender account. Use --min_credit_score.");
        return Err(PlatformError::InputsError);
      };
      data.add_lender(name, min_credit_score)
    }
    ("view_loan", Some(view_loan_matches)) => {
      let lender_id = if let Some(id_arg) = lender_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Lender id is required to get loan information. Use lender --id.");
        return Err(PlatformError::InputsError);
      };
      if let Some(loan_arg) = view_loan_matches.value_of("loan") {
        let loan_id = parse_to_u64(loan_arg)?;
        let loan = data.loans[loan_id as usize].clone();
        if loan.lender != lender_id {
          println!("Lender {} doesn't own loan {}.", lender_id, loan_id);
          return Err(PlatformError::InputsError);
        }
        println!("Displaying loan {}: {:?}.", loan_id, loan);
        return Ok(());
      }
      let mut loans = Vec::new();
      let loan_ids = data.lenders[lender_id as usize].loans.clone();
      if let Some(filter) = view_loan_matches.value_of("filter") {
        for id in loan_ids {
          match filter {
            "active" => {
              if data.loans[id as usize].active {
                loans.push(data.loans[id as usize].clone());
              }
            }
            "inactive" => {
              if !data.loans[id as usize].active {
                loans.push(data.loans[id as usize].clone());
              }
            }
            _ => {
              if !data.loans[id as usize].rejected {
                loans.push(data.loans[id as usize].clone());
              }
            }
          }
        }
      } else {
        for id in loan_ids {
          loans.push(data.loans[id as usize].clone());
        }
      }
      println!("Displaying {} loan(s): {:?}", loans.len(), loans);
      Ok(())
    }
    ("activate_loan", Some(activate_loan_matches)) => {
      let loan_id = if let Some(loan_arg) = activate_loan_matches.value_of("loan") {
        parse_to_u64(loan_arg)?
      } else {
        println!("Loan id is required to activate the loan. Use --loan.");
        return Err(PlatformError::InputsError);
      };
      if let Some(id_arg) = lender_matches.value_of("id") {
        let lender_id = parse_to_u64(id_arg)?;
        let loan = data.loans[loan_id as usize].clone();
        if loan.lender != lender_id {
          println!("Lender {} doesn't own loan {}.", lender_id, loan_id);
          return Err(PlatformError::InputsError);
        }
      } else {
        println!("Lender id is required to activate a loan. Use lender --id.");
        return Err(PlatformError::InputsError);
      };
      let issuer_id = if let Some(issuer_arg) = activate_loan_matches.value_of("issuer") {
        parse_to_u64(issuer_arg)?
      } else {
        println!("Issuer id is required to activate the loan. Use --issuer.");
        return Err(PlatformError::InputsError);
      };
      let protocol = if activate_loan_matches.is_present("http") {
        // Allow HTTP which may be useful for running a ledger locally.
        "http"
      } else {
        // Default to HTTPS
        "https"
      };
      let host = if activate_loan_matches.is_present("localhost") {
        // Use localhost
        run_ledger_standalone()?;
        "localhost"
      } else {
        // Default to testnet.findora.org
        "testnet.findora.org"
      };
      activate_loan(loan_id, issuer_id, transaction_file_name, protocol, host)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try lender --help");
      Err(PlatformError::InputsError)
    }
  }
}

fn process_borrower_cmd(borrower_matches: &clap::ArgMatches,
                        transaction_file_name: &str)
                        -> Result<(), PlatformError> {
  let mut data = load_data()?;
  match borrower_matches.subcommand() {
    ("sign_up", Some(sign_up_matches)) => {
      let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
        name_arg.to_owned()
      } else {
        println!("Name is required to sign up a lender account. Use --name.");
        return Err(PlatformError::InputsError);
      };
      data.add_borrower(name)
    }
    ("load_funds", Some(load_funds_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to load funds. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      process_load_funds_cmd(borrower_id, load_funds_matches, transaction_file_name)
    }
    ("view_loan", Some(view_loan_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to get loan information. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      if let Some(loan_arg) = view_loan_matches.value_of("loan") {
        let loan_id = parse_to_u64(loan_arg)?;
        let loan = data.loans[loan_id as usize].clone();
        if loan.borrower != borrower_id {
          println!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
          return Err(PlatformError::InputsError);
        }
        println!("Displaying loan {}: {:?}.", loan_id, loan);
        return Ok(());
      }
      let mut loans = Vec::new();
      let loan_ids = data.borrowers[borrower_id as usize].loans.clone();
      if let Some(filter) = view_loan_matches.value_of("filter") {
        for id in loan_ids {
          match filter {
            "active" => {
              if data.loans[id as usize].active {
                loans.push(data.loans[id as usize].clone());
              }
            }
            "inactive" => {
              if !data.loans[id as usize].active {
                loans.push(data.loans[id as usize].clone());
              }
            }
            _ => {
              if !data.loans[id as usize].rejected {
                loans.push(data.loans[id as usize].clone());
              }
            }
          }
        }
      } else {
        for id in loan_ids {
          loans.push(data.loans[id as usize].clone());
        }
      }
      println!("Displaying {} loan(s): {:?}", loans.len(), loans);
      Ok(())
    }
    ("create_loan", Some(create_loan_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to create a loan. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      let lender_id = if let Some(lender_arg) = create_loan_matches.value_of("lender") {
        parse_to_u64(lender_arg)?
      } else {
        println!("Lender id is required to create the loan. Use --lender.");
        return Err(PlatformError::InputsError);
      };
      let amount = if let Some(amount_arg) = create_loan_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Amount is required to create the loan. Use --amount.");
        return Err(PlatformError::InputsError);
      };
      let interest_per_mille =
        if let Some(interest_per_mille_arg) = create_loan_matches.value_of("interest_per_mille") {
          parse_to_u64(interest_per_mille_arg)?
        } else {
          println!("Interest per mille is required to create the loan. Use --interest_per_mille.");
          return Err(PlatformError::InputsError);
        };
      let duration = if let Some(duration_arg) = create_loan_matches.value_of("duration") {
        parse_to_u64(duration_arg)?
      } else {
        println!("Duration is required to create the loan. Use --amount.");
        return Err(PlatformError::InputsError);
      };
      let mut data = load_data()?;
      data.add_loan(lender_id, borrower_id, amount, interest_per_mille, duration)
    }
    ("pay_loan", Some(pay_loan_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to pay off the loan. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
        let loan_id = parse_to_u64(loan_arg)?;
        let loan = data.loans[loan_id as usize].clone();
        if loan.borrower != borrower_id {
          println!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
          return Err(PlatformError::InputsError);
        }
      } else {
        println!("Loan id is required to pay the loan.");
        return Err(PlatformError::InputsError);
      }
      process_pay_loan_cmd(pay_loan_matches, transaction_file_name)
    }
    ("view_credential", Some(view_credential_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to get credential information. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      if let Some(credential_arg) = view_credential_matches.value_of("credential") {
        let credential_id = parse_to_u64(credential_arg)?;
        let credential = data.loans[credential_id as usize].clone();
        if credential.borrower != borrower_id {
          println!("Borrower {} doesn't own credential {}.",
                   borrower_id, credential_id);
          return Err(PlatformError::InputsError);
        }
        println!("Displaying credential {}: {:?}.", credential_id, credential);
        return Ok(());
      }
      let mut credentials = Vec::new();
      let credential_ids = data.borrowers[borrower_id as usize].credentials.clone();
      for i in 0..3 {
        if let Some(id) = credential_ids[i] {
          credentials.push(data.credentials[id as usize].clone());
        }
      }
      println!("Displaying {} credential(s): {:?}",
               credentials.len(),
               credentials);
      Ok(())
    }
    ("create_or_overwrite_credential", Some(create_or_overwrite_credential_matches)) => {
      let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
        parse_to_u64(id_arg)?
      } else {
        println!("Borrower id is required to get credential information. Use borrower --id.");
        return Err(PlatformError::InputsError);
      };
      let attribute =
        if let Some(attribute_arg) = create_or_overwrite_credential_matches.value_of("attribute") {
          match attribute_arg {
            "min_credit_score" => CredentialIndex::MinCreditScore,
            "min_income" => CredentialIndex::MinIncome,
            _ => CredentialIndex::Citizenship,
          }
        } else {
          println!("Credential attribute is required to create the credential. Use --attribute.");
          return Err(PlatformError::InputsError);
        };
      let value = if let Some(value_arg) = create_or_overwrite_credential_matches.value_of("value")
      {
        parse_to_u64(value_arg)?
      } else {
        println!("Credential value is required to create the credential. Use --value.");
        return Err(PlatformError::InputsError);
      };
      let mut data = load_data()?;
      data.add_or_update_credential(borrower_id, attribute, value)
    }
    _ => {
      println!("Subcommand missing or not recognized. Try borrower --help");
      Err(PlatformError::InputsError)
    }
  }
}

// Create the specific file if missing
// Rename the existing path if necessary
fn create_directory_and_rename_path(path_str: &str, overwrite: bool) -> Result<(), PlatformError> {
  let path = Path::new(&path_str);
  create_directory_if_missing(&path_str)?;
  if path.exists() && !overwrite {
    rename_existing_path(&path)?;
  }
  Ok(())
}

fn process_create_txn_builder_cmd(create_matches: &clap::ArgMatches,
                                  transaction_file_name: &str,
                                  _findora_dir: &str)
                                  -> Result<(), PlatformError> {
  let name = create_matches.value_of("name");
  let overwrite = create_matches.is_present("overwrite");
  let file_str = if let Some(name) = name {
    name.to_string()
  } else {
    transaction_file_name.to_string()
  };
  let expand_str = shellexpand::tilde(&file_str).to_string();
  create_directory_and_rename_path(&expand_str, overwrite)?;
  let txn_builder = TransactionBuilder::default();
  store_txn_builder_to_file(&expand_str, &txn_builder)
}

fn process_store_cmd(store_matches: &clap::ArgMatches,
                     findora_dir: &str)
                     -> Result<(), PlatformError> {
  match store_matches.subcommand() {
    ("sids", Some(sids_matches)) => {
      let path = if let Some(path_arg) = sids_matches.value_of("path") {
        path_arg.to_string()
      } else {
        format!("{}/values/default.sids", &findora_dir)
      };
      let path_expand = shellexpand::tilde(&path).to_string();
      println!("Storing sids to {}", path_expand);
      let overwrite = sids_matches.is_present("overwrite");
      create_directory_and_rename_path(&path_expand, overwrite)?;
      let sids;
      if let Some(sids_arg) = sids_matches.value_of("indices") {
        sids = sids_arg
      } else {
        println!("TxoSID indices are required. Use --indices.");
        return Err(PlatformError::InputsError);
      }
      store_sids_to_file(&path_expand, sids)
    }

    ("blind_asset_record", Some(blind_asset_record_path_matches)) => {
      let path = if let Some(path_arg) = blind_asset_record_path_matches.value_of("path") {
        path_arg.to_string()
      } else {
        format!("{}/values/default.blind_asset_record", &findora_dir)
      };
      let path_expand = shellexpand::tilde(&path).to_string();
      println!("Storing blind asset records to {}", path_expand);
      let overwrite = blind_asset_record_path_matches.is_present("overwrite");
      create_directory_and_rename_path(&path_expand, overwrite)?;
      let amount = if let Some(amount_arg) = blind_asset_record_path_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Amount is required. Use --amount.");
        return Err(PlatformError::InputsError);
      };
      let asset_type;
      if let Some(asset_type_arg) = blind_asset_record_path_matches.value_of("asset_type") {
        asset_type = asset_type_arg
      } else {
        println!("Asset type is required. Use --asset_type.");
        return Err(PlatformError::InputsError);
      }
      let pub_key_path;
      if let Some(pub_key_path_arg) = blind_asset_record_path_matches.value_of("pub_key_path") {
        pub_key_path = pub_key_path_arg
      } else {
        println!("File to public key is required. If no such file, try pubkeygen subcommand.");
        return Err(PlatformError::InputsError);
      }
      let confidential_amount = blind_asset_record_path_matches.is_present("confidential_amount");
      let confidential_asset = blind_asset_record_path_matches.is_present("confidential_asset");
      store_blind_asset_record(&path_expand,
                               amount,
                               asset_type,
                               pub_key_path,
                               confidential_amount,
                               confidential_asset)
    }

    _ => {
      println!("Subcommand missing or not recognized. Try store --help");
      Err(PlatformError::InputsError)
    }
  }
}

fn process_add_cmd(add_matches: &clap::ArgMatches,
                   transaction_file_name: &str)
                   -> Result<(), PlatformError> {
  match add_matches.subcommand() {
    ("define_asset", Some(define_asset_matches)) => {
      let fiat_asset = define_asset_matches.is_present("fiat");
      let issuer_id = if let Some(issuer_arg) = define_asset_matches.value_of("issuer") {
        parse_to_u64(issuer_arg)?
      } else {
        println!("User id is required to define asset. Use --issuer.");
        return Err(PlatformError::InputsError);
      };
      let token_code = define_asset_matches.value_of("token_code");
      let memo = if let Some(memo) = define_asset_matches.value_of("memo") {
        memo
      } else {
        "{}"
      };
      let allow_updates = define_asset_matches.is_present("allow_updates");
      let traceable = define_asset_matches.is_present("traceable");
      let asset_token: AssetTypeCode;
      if let Some(token_code) = token_code {
        asset_token = AssetTypeCode::new_from_base64(token_code)?;
      } else {
        asset_token = AssetTypeCode::gen_random();
        println!("Creating asset with token code {:?}: {:?}",
                 asset_token.to_base64(),
                 asset_token.val);
      }
      define_asset(fiat_asset,
                   issuer_id,
                   asset_token,
                   &memo,
                   allow_updates,
                   traceable,
                   transaction_file_name)
    }
    ("issue_asset", Some(issue_asset_matches)) => {
      let mut data = load_data()?;
      let key_pair = if let Some(issuer_arg) = issue_asset_matches.value_of("issuer") {
        if let Ok(id) = issuer_arg.parse::<u64>() {
          data.get_issuer_key_pair(id)?
        } else {
          println!("Improperly formatted issuer id.");
          return Err(PlatformError::InputsError);
        }
      } else {
        println!("Issuer id is required to issue asset. Use --issuer.");
        return Err(PlatformError::InputsError);
      };
      let asset_token: AssetTypeCode;
      if let Some(token_code_arg) = issue_asset_matches.value_of("token_code") {
        asset_token = AssetTypeCode::new_from_base64(token_code_arg)?;
      } else {
        println!("Token code is required to issue asset. Use --token_code.");
        return Err(PlatformError::InputsError);
      }
      let amount = if let Some(amount_arg) = issue_asset_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Amount is required to issue asset. Use --amount.");
        return Err(PlatformError::InputsError);
      };
      let mut txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                              println!("Failed to load txn builder from file {}.",
                                       transaction_file_name);
                              Err(e)
                            })?;
      if let Err(e) = txn_builder.add_basic_issue_asset(&key_pair,
                                                        &None,
                                                        &asset_token,
                                                        load_and_update_sequence_number()?,
                                                        amount)
      {
        println!("Failed to add basic issue asset.");
        return Err(e);
      }
      store_txn_builder_to_file(&transaction_file_name, &txn_builder)
    }
    ("transfer_asset", Some(transfer_asset_matches)) => {
      let mut data = load_data()?;
      let issuer_key_pair = if let Some(issuer_arg) = transfer_asset_matches.value_of("issuer") {
        if let Ok(id) = issuer_arg.parse::<u64>() {
          data.get_issuer_key_pair(id)?
        } else {
          println!("Improperly formatted issuer id.");
          return Err(PlatformError::InputsError);
        }
      } else {
        println!("Issuer id is required to issue asset. Use --issuer.");
        return Err(PlatformError::InputsError);
      };
      // Compose transfer_from for add_basic_transfer_asset
      let txo_refs;
      if let Some(sids_path) = transfer_asset_matches.value_of("sids_path") {
        match load_sids_from_file(sids_path) {
          Ok(result) => {
            txo_refs = result;
          }
          Err(error) => {
            println!("Error loading txo_refs from {}: {}", sids_path, error);
            return Err(error);
          }
        }
      } else {
        println!("Path to sids file is required to transfer asset. Use --sids_path");
        return Err(PlatformError::InputsError);
      }
      let blind_asset_records;
      if let Some(blind_asset_record_paths) =
        transfer_asset_matches.value_of("blind_asset_record_paths")
      {
        match load_blind_asset_records_from_files(blind_asset_record_paths) {
          Ok(result) => {
            blind_asset_records = result;
          }
          Err(error) => {
            println!("Error loading blind_asset_records from {}: {}",
                     blind_asset_record_paths, error);
            return Err(error);
          }
        }
      } else {
        println!("Paths to blind asset records are required to transfer asset. Use --blind_asset_record_paths");
        return Err(PlatformError::InputsError);
      }
      let input_amounts;
      if let Some(input_amounts_arg) = transfer_asset_matches.value_of("input_amounts") {
        input_amounts = get_amounts(input_amounts_arg)?;
      } else {
        println!("Input amounts are required to transfer asset. Use --input_amounts.");
        return Err(PlatformError::InputsError);
      }
      let mut count = txo_refs.len();
      if blind_asset_records.len() != count || input_amounts.len() != count {
        println!("Size of input sids, blind asset records, and input amounts should match.");
        return Err(PlatformError::InputsError);
      }
      let mut transfer_from = Vec::new();
      let mut txo_refs_iter = txo_refs.iter();
      let mut blind_asset_records_iter = blind_asset_records.iter();
      let mut input_amounts_iter = input_amounts.iter();
      while count > 0 {
        let txo_ref_next = if let Some(txo_ref) = txo_refs_iter.next() {
          txo_ref
        } else {
          println!("More txo ref expected.");
          return Err(PlatformError::InputsError);
        };
        let blind_asset_record_next =
          if let Some(blind_asset_record) = blind_asset_records_iter.next() {
            blind_asset_record
          } else {
            println!("More blind asset record expected.");
            return Err(PlatformError::InputsError);
          };
        let input_amount_next = if let Some(input_amount) = input_amounts_iter.next() {
          *input_amount
        } else {
          println!("More input amount expected.");
          return Err(PlatformError::InputsError);
        };
        transfer_from.push((txo_ref_next, blind_asset_record_next, input_amount_next));
        count -= 1;
      }

      // Compose transfer_to for add_basic_transfer_asset
      let output_amounts;
      if let Some(output_amounts_arg) = transfer_asset_matches.value_of("output_amounts") {
        output_amounts = get_amounts(output_amounts_arg)?;
      } else {
        println!("Output amounts are required to transfer asset. Use --output_amounts.");
        return Err(PlatformError::InputsError);
      }
      let addresses;
      if let Some(addresses_path) = transfer_asset_matches.value_of("address_paths") {
        addresses = load_addresses_from_files(addresses_path)?;
      } else {
        println!("Paths to address keys are required to transfer asset. Use --address_paths");
        return Err(PlatformError::InputsError);
      }
      let mut count = output_amounts.len();
      if addresses.len() != count {
        println!("Size of output amounts and addresses should match.");
        return Err(PlatformError::InputsError);
      }
      let mut transfer_to = Vec::new();
      let mut output_amounts_iter = output_amounts.iter();
      let mut addresses_iter = addresses.iter();
      while count > 0 {
        let output_amount_next = if let Some(output_amount) = output_amounts_iter.next() {
          *output_amount
        } else {
          println!("More output amount expected.");
          return Err(PlatformError::InputsError);
        };
        let address_next = if let Some(address) = addresses_iter.next() {
          address
        } else {
          println!("More address expected.");
          return Err(PlatformError::InputsError);
        };
        transfer_to.push((output_amount_next, address_next));
        count -= 1;
      }

      // Transfer asset
      let mut txn_builder = load_txn_builder_from_file(&transaction_file_name).or_else(|e| {
                              println!("Failed to load txn builder from file {}.",
                                       transaction_file_name);
                              Err(e)
                            })?;
      if let Err(e) =
        txn_builder.add_basic_transfer_asset(&issuer_key_pair, &transfer_from[..], &transfer_to[..])
      {
        println!("Failed to add operation to transaction.");
        return Err(e);
      };
      store_txn_builder_to_file(&transaction_file_name, &txn_builder)
    }
    ("issue_and_transfer_asset", Some(issue_and_transfer_matches)) => {
      let mut data = load_data()?;
      let issuer_key_pair = if let Some(issuer_arg) = issue_and_transfer_matches.value_of("issuer")
      {
        if let Ok(id) = issuer_arg.parse::<u64>() {
          data.get_issuer_key_pair(id)?
        } else {
          println!("Improperly formatted issuer id.");
          return Err(PlatformError::InputsError);
        }
      } else {
        println!("Issuer id is required to issue asset. Use --issuer.");
        return Err(PlatformError::InputsError);
      };
      let recipient_key_pair =
        if let Some(recipient_arg) = issue_and_transfer_matches.value_of("recipient") {
          if let Ok(id) = recipient_arg.parse::<u64>() {
            data.get_borrower_key_pair(id)?
          } else {
            println!("Improperly formatted recipient's id.");
            return Err(PlatformError::InputsError);
          }
        } else {
          println!("Recipient id is required to issue asset. Use --issuer.");
          return Err(PlatformError::InputsError);
        };
      let amount = if let Some(amount_arg) = issue_and_transfer_matches.value_of("amount") {
        parse_to_u64(amount_arg)?
      } else {
        println!("Amount is required to issue and transfer asset. Use --amount.");
        return Err(PlatformError::InputsError);
      };
      let token_code =
        if let Some(token_code_arg) = issue_and_transfer_matches.value_of("token_code") {
          AssetTypeCode::new_from_base64(token_code_arg)?
        } else {
          println!("Token code is required to issue asset. Use --token_code.");
          return Err(PlatformError::InputsError);
        };

      issue_and_transfer(&issuer_key_pair,
                         &recipient_key_pair,
                         amount,
                         token_code,
                         transaction_file_name)?;
      Ok(())
    }
    _ => {
      println!("Subcommand missing or not recognized. Try add --help");
      Err(PlatformError::InputsError)
    }
  }
}

fn process_submit_cmd(submit_matches: &clap::ArgMatches,
                      transaction_file_name: &str)
                      -> Result<(), PlatformError> {
  // Get protocol and host.
  let protocol = if submit_matches.is_present("http") {
    // Allow HTTP which may be useful for running a ledger locally.
    "http"
  } else {
    // Default to HTTPS
    "https"
  };
  let host = if submit_matches.is_present("localhost") {
    // Use localhost
    run_ledger_standalone()?;
    "localhost"
  } else {
    // Default to testnet.findora.org
    "testnet.findora.org"
  };

  submit(protocol, host, &transaction_file_name)
}

fn process_load_funds_cmd(borrower_id: u64,
                          load_funds_matches: &clap::ArgMatches,
                          transaction_file_name: &str)
                          -> Result<(), PlatformError> {
  let issuer_id = if let Some(issuer_arg) = load_funds_matches.value_of("issuer") {
    if let Ok(id) = issuer_arg.parse::<u64>() {
      id
    } else {
      println!("Improperly formatted issuer id.");
      return Err(PlatformError::InputsError);
    }
  } else {
    println!("Issuer id is required to load funds. Use --issuer.");
    return Err(PlatformError::InputsError);
  };
  let amount = if let Some(amount_arg) = load_funds_matches.value_of("amount") {
    parse_to_u64(amount_arg)?
  } else {
    println!("Amount is required to load funds. Use --amount.");
    return Err(PlatformError::InputsError);
  };
  let protocol = if load_funds_matches.is_present("http") {
    // Allow HTTP which may be useful for running a ledger locally.
    "http"
  } else {
    // Default to HTTPS
    "https"
  };
  let host = if load_funds_matches.is_present("localhost") {
    // Use localhost
    run_ledger_standalone()?;
    "localhost"
  } else {
    // Default to testnet.findora.org
    "testnet.findora.org"
  };

  load_funds(issuer_id,
             borrower_id,
             amount,
             transaction_file_name,
             protocol,
             host)
}

fn process_pay_loan_cmd(pay_loan_matches: &clap::ArgMatches,
                        transaction_file_name: &str)
                        -> Result<(), PlatformError> {
  let loan_id = if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
    parse_to_u64(loan_arg)?
  } else {
    println!("Loan id is required to pay the loan. Use --loan.");
    return Err(PlatformError::InputsError);
  };
  let amount = if let Some(amount_arg) = pay_loan_matches.value_of("amount") {
    parse_to_u64(amount_arg)?
  } else {
    println!("Amount is required to pay the loan. Use --amount.");
    return Err(PlatformError::InputsError);
  };
  let protocol = if pay_loan_matches.is_present("http") {
    // Allow HTTP which may be useful for running a ledger locally.
    "http"
  } else {
    // Default to HTTPS
    "https"
  };
  let host = if pay_loan_matches.is_present("localhost") {
    // Use localhost
    run_ledger_standalone()?;
    "localhost"
  } else {
    // Default to testnet.findora.org
    "testnet.findora.org"
  };

  pay_loan(loan_id, amount, transaction_file_name, protocol, host)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::str::from_utf8;

  const PROTOCOL: &str = "http";
  const HOST: &str = "localhost";

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
    if let Err(e) = fs::write(input, "txn_builder_cli next_path() test detritus") {
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

    let expected_txo_refs = vec![TxoRef::Absolute(TxoSID(1)),
                                 TxoRef::Absolute(TxoSID(2)),
                                 TxoRef::Absolute(TxoSID(4))];

    // Verify that load_sids_from_file succeeds with correctly formatted input
    assert_eq!(load_sids_from_file(paths[0]).unwrap(), expected_txo_refs);
    assert_eq!(load_sids_from_file(paths[1]).unwrap(), expected_txo_refs);

    paths.into_iter()
         .map(|path| fs::remove_file(path).unwrap())
         .collect()
  }

  #[test]
  fn test_store_and_load_blind_asset_records() {
    // Set fields for constructing blind asset records
    let paths = vec!["file1", "file2", "file3"];
    let paths_str = "file1,file2, file3";
    let pub_key_paths = vec!["pub1", "pub2", "pub3"];
    let amounts = vec![100, 200, 300];
    let asset_types = vec![[0u8; 16], [0u8; 16], [0u8; 16]];
    let confidential_amount_bools = vec![false, false, true];
    let confidential_asset_bools = vec![false, true, false];

    // Store each blind asset record
    for i in 0..3 {
      store_pub_key_to_file(pub_key_paths[i]).unwrap();
      store_blind_asset_record(paths[i],
                               amounts[i],
                               from_utf8(&asset_types[i]).unwrap(),
                               pub_key_paths[i],
                               confidential_amount_bools[i],
                               confidential_asset_bools[i]).unwrap();
    }

    // Load all the blind asset records
    let blind_asset_records = load_blind_asset_records_from_files(&paths_str).unwrap();

    // Verify the field of the first blind asset record
    assert_eq!(blind_asset_records[0].amount, Some(amounts[0]));
    assert_eq!(blind_asset_records[0].asset_type, Some(asset_types[0]));
    fs::remove_file(paths[0]).unwrap();
    fs::remove_file(pub_key_paths[0]).unwrap();

    // Verify the field of the second blind asset record
    // Asset type should be None because it's set as confidential
    assert_eq!(blind_asset_records[1].amount, Some(amounts[1]));
    assert_eq!(blind_asset_records[1].asset_type, None);
    fs::remove_file(paths[1]).unwrap();
    fs::remove_file(pub_key_paths[1]).unwrap();

    // Verify the field of the third blind asset record
    // Amount should be None because the it's set as confidential
    assert_eq!(blind_asset_records[2].amount, None);
    assert_eq!(blind_asset_records[2].asset_type, Some(asset_types[2]));
    fs::remove_file(paths[2]).unwrap();
    fs::remove_file(pub_key_paths[2]).unwrap();
  }

  #[test]
  fn test_get_amounts() {
    let amounts_arg = "1, 2,4";
    let expected_amounts = vec![1, 2, 4];

    assert_eq!(get_amounts(amounts_arg).unwrap(), expected_amounts);
  }

  #[test]
  fn test_issue_and_transfer() {
    // Create txn builder and key pairs
    let txn_builder_path = "tb_issue_and_transfer";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let issuer_key_pair = XfrKeyPair::generate(&mut prng);
    let recipient_key_pair = XfrKeyPair::generate(&mut prng);

    // Issue and transfer asset
    let code = AssetTypeCode::gen_random();
    let amount = 1000;
    assert!(issue_and_transfer(&issuer_key_pair,
                               &recipient_key_pair,
                               amount,
                               code,
                               txn_builder_path).is_ok());

    let _ = fs::remove_file(DATA_FILE);
    fs::remove_file(txn_builder_path).unwrap();
  }

  #[test]
  fn test_merge_records() {
    // Create txn builder and key pair
    let txn_builder_path = "tb_merge";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let key_pair = XfrKeyPair::generate(&mut prng);

    // Build blind asset records
    let code = AssetTypeCode::gen_random();
    let bar1 = get_blind_asset_record(key_pair.get_pk(), 1000, code, false, false).unwrap();
    let bar2 = get_blind_asset_record(key_pair.get_pk(), 500, code, false, false).unwrap();

    // Merge records
    assert!(merge_records(&key_pair,
                          TxoRef::Absolute(TxoSID(1)),
                          TxoRef::Absolute(TxoSID(2)),
                          bar1,
                          bar2,
                          code,
                          txn_builder_path).is_ok());

    fs::remove_file(txn_builder_path).unwrap();
  }

  #[test]
  fn test_submit() {
    let txn_builder_path = "tb_submit";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    run_ledger_standalone().unwrap();
    let res = submit(PROTOCOL, HOST, txn_builder_path);
    fs::remove_file(txn_builder_path).unwrap();
    assert!(res.is_ok());
  }

  #[test]
  // Define fiat asset and submit the transaction
  fn test_define_fiat_asset_and_submit() {
    let txn_builder_path = "tb_define_and_submit";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut txn_builder = load_txn_builder_from_file(&txn_builder_path).unwrap();

    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let key_pair = XfrKeyPair::generate(&mut prng);
    let token_code = AssetTypeCode::gen_random();

    // Define fiat asset
    define_asset(true,
                 0,
                 token_code,
                 "Define fiat asset",
                 false,
                 false,
                 txn_builder_path).unwrap();

    txn_builder.add_operation_create_asset(&key_pair, Some(token_code), false, false, "")
               .unwrap();
    store_txn_builder_to_file(&txn_builder_path, &txn_builder).unwrap();

    // Submit
    run_ledger_standalone().unwrap();
    let res = submit(PROTOCOL, HOST, txn_builder_path);
    fs::remove_file(txn_builder_path).unwrap();
    assert!(res.is_ok());
  }

  #[test]
  // 1. The issuer defines the asset
  // 2. The issuer issues and transfers two assets to the recipient
  // 3. Merge the two records for the recipient
  // 4. Submit the transaction
  fn test_merge_and_submit() {
    run_ledger_standalone().unwrap();

    // Create txn builder and key pairs
    let txn_builder_path = "tb_merge_and_submit";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let issuer_key_pair = XfrKeyPair::generate(&mut prng);
    let recipient_key_pair = XfrKeyPair::generate(&mut prng);

    // Define amounts and token code
    let amount1 = 1000;
    let amount2 = 500;
    let code = AssetTypeCode::gen_random();

    // Define asset
    let mut txn_builder = load_txn_builder_from_file(&txn_builder_path).unwrap();
    txn_builder.add_operation_create_asset(&issuer_key_pair, Some(code), false, false, "")
               .unwrap()
               .transaction();
    store_txn_builder_to_file(&txn_builder_path, &txn_builder).unwrap();
    let res = submit(PROTOCOL, HOST, txn_builder_path);
    assert!(res.is_ok());

    // Issue and transfer the first asset
    issue_and_transfer(&issuer_key_pair,
                       &recipient_key_pair,
                       amount1,
                       code,
                       txn_builder_path).unwrap();

    let sid1 = submit_and_get_sids(PROTOCOL, HOST, txn_builder_path).unwrap()[0];
    let res = query(PROTOCOL,
                    HOST,
                    QUERY_PORT,
                    "utxo_sid",
                    &format!("{}", sid1.0)).unwrap();
    let blind_asset_record_1 =
      serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                      Err(PlatformError::DeserializationError)
                                                    })
                                                    .unwrap();

    // Issue and transfer the second asset
    issue_and_transfer(&issuer_key_pair,
                       &recipient_key_pair,
                       amount2,
                       code,
                       txn_builder_path).unwrap();
    let sid2 = submit_and_get_sids(PROTOCOL, HOST, txn_builder_path).unwrap()[0];
    let res = query(PROTOCOL,
                    HOST,
                    QUERY_PORT,
                    "utxo_sid",
                    &format!("{}", sid2.0)).unwrap();
    let blind_asset_record_2 =
      serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                      Err(PlatformError::DeserializationError)
                                                    })
                                                    .unwrap();

    // Merge records
    merge_records(&recipient_key_pair,
                  TxoRef::Absolute(sid1),
                  TxoRef::Absolute(sid2),
                  blind_asset_record_1,
                  blind_asset_record_2,
                  code,
                  txn_builder_path).unwrap();

    // Submit transactions
    let res = submit(PROTOCOL, HOST, txn_builder_path);

    let _ = fs::remove_file(DATA_FILE);
    fs::remove_file(txn_builder_path).unwrap();

    assert!(res.is_ok());
  }

  #[test]
  #[ignore]
  fn test_load_funds() {
    run_ledger_standalone().unwrap();

    let data = load_data().unwrap();
    let balance_pre = data.borrowers[0].balance;

    // Create txn builder
    let txn_builder_path = "tb_load_funds";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();

    // Define amount
    let amount = 1000;

    // Load funds
    load_funds(0, 0, amount, txn_builder_path, PROTOCOL, HOST).unwrap();
    fs::remove_file(txn_builder_path).unwrap();

    assert_eq!(load_data().unwrap().borrowers[0].balance,
               balance_pre + amount);

    let _ = fs::remove_file(DATA_FILE);
  }

  #[test]
  fn test_prove() {
    let mut prng: ChaChaRng = ChaChaRng::from_seed([0u8; 32]);
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, 1);
    let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer_pk.clone());

    let value: u64 = 200;
    let attributes = [value.to_le_bytes()].to_vec();
    let signature = ac_sign(&mut prng, &issuer_sk, &user_pk, &attributes);
    let zei_credential = ZeiCredential { signature,
                                         attributes,
                                         issuer_pk: issuer_pk.clone() };

    let reveal_sig =
      ac_reveal(&mut prng, &user_sk, &zei_credential, &[true]).or_else(|error| {
                                                                Err(PlatformError::ZeiError(error))
                                                              })
                                                              .unwrap();

    assert!(prove(&reveal_sig,
                  &issuer_pk.clone(),
                  value,
                  150,
                  RelationType::AtLeast).is_ok());
    assert_eq!(prove(&reveal_sig, &issuer_pk, value, 300, RelationType::AtLeast),
               Err(PlatformError::InputsError));
  }

  #[test]
  #[ignore]
  // Create, activate and pay a loan
  fn test_create_activate_and_pay_loan() {
    run_ledger_standalone().unwrap();

    // Load data
    let mut data = load_data().unwrap();
    let loan_id = data.loans.len();

    // Create loan
    let amount = 1200;
    data.add_loan(0, 0, amount, 100, 8).unwrap();
    assert_eq!(data.loans.len(), loan_id + 1);

    // Create txn builder
    let txn_builder_path = "tb_loan";
    store_txn_builder_to_file(&txn_builder_path, &TransactionBuilder::default()).unwrap();

    // Activate loan
    activate_loan(loan_id as u64, 0, txn_builder_path, PROTOCOL, HOST).unwrap();
    let data = load_data().unwrap();
    assert_eq!(data.loans[loan_id].active, true);
    assert_eq!(data.loans[loan_id].balance, amount);

    // Pay loan
    let payment_amount = 200;
    pay_loan(loan_id as u64,
             payment_amount,
             txn_builder_path,
             PROTOCOL,
             HOST).unwrap();

    let data = load_data().unwrap();
    assert_eq!(data.loans[loan_id].payments, 1);

    let _ = fs::remove_file(DATA_FILE);
    fs::remove_file(txn_builder_path).unwrap();
  }
}
