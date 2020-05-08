#![deny(warnings)]
pub mod txn_lib {
  use credentials::{
    credential_issuer_key_gen, credential_keygen_commitment, credential_reveal, credential_sign,
    credential_user_key_gen, credential_verify, u8_slice_to_u32_vec, CredCommitment,
    CredCommitmentKey, CredIssuerPublicKey, CredIssuerSecretKey, CredPoK, CredUserPublicKey,
    CredUserSecretKey, Credential as WrapperCredential,
  };
  use curve25519_dalek::ristretto::CompressedRistretto;
  use curve25519_dalek::scalar::Scalar;
  use env_logger::{Env, Target};
  use ledger::data_model::errors::PlatformError;
  use ledger::data_model::{
    AccountAddress, AssetRules, AssetTypeCode, TransferType, TxOutput, TxoRef, TxoSID,
  };
  use ledger::error_location;
  use ledger::policies::{DebtMemo, Fraction};
  use ledger_standalone::LedgerStandalone;
  use log::trace; // Other options: debug, info, warn
  use rand_chacha::ChaChaRng;
  use rand_core::{CryptoRng, RngCore, SeedableRng};
  use serde::{Deserialize, Serialize};
  use std::env;
  use std::fs;
  use std::path::{Path, PathBuf};
  use std::process::exit;
  use submission_server::{TxnHandle, TxnStatus};
  use txn_builder::{
    BuildsTransactions, PolicyChoice, TransactionBuilder, TransferOperationBuilder,
  };
  use zei::api::anon_creds::{ac_confidential_open_commitment, Credential as ZeiCredential};
  use zei::serialization::ZeiFromToBytes;
  use zei::setup::PublicParams;
  use zei::xfr::asset_record::{
    build_blind_asset_record, open_blind_asset_record, AssetRecordType,
  };
  use zei::xfr::asset_tracer::gen_asset_tracer_keypair;
  use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
  use zei::xfr::structs::{
    AssetRecordTemplate, AssetTracerKeyPair, AssetTracerMemo, AssetTracingPolicy, BlindAssetRecord,
    IdentityRevealPolicy, OpenAssetRecord, OwnerMemo, XfrAmount, XfrAssetType,
  };

  extern crate exitcode;

  /// Initial data when the program starts.
  // TODO (Keyao):
  // Make this data driven, not embedded in the Rust code.
  // The attribute names will be determined by the customer's application and will differ from customer to customer.
  // Or we'll develop a standard registry or dictionary of attributes.
  // Either way, this will be stored externally.
  const INIT_DATA: &str = r#"
{
  "asset_issuers": [
    {
      "id": 0,
      "name": "Izzie",
      "key_pair": "6bcd6c7421aca7df38f5be361ba2fad5affff287da0730ca14235abd23269991ed11ea990324e0f573cd11c5ad0af2be94d8856600e626b62e49b526eb36ca08",
      "tracer_key_pair": "7b22656e635f6b6579223a7b227265636f72645f646174615f656e635f6b6579223a5b3135362c3130322c3136332c35372c3230302c35322c37392c3134362c34372c3139352c33322c3130382c3138312c3231382c3233322c32302c3136352c3134382c3139322c32332c3132352c3231312c33352c39322c33372c37372c3135362c36342c3135342c3130312c3138342c385d2c2261747472735f656e635f6b6579223a22755874716e546532556474444d575a4c4779546336736c4d4439393136476c6d45324c615f373356574f5942345f6c63455254456c7956305966417176304758227d2c226465635f6b6579223a7b227265636f72645f646174615f6465635f6b6579223a5b37342c38332c3139352c3235312c3138382c38392c3135312c31342c3232392c3234382c39302c3234382c31392c3133352c39332c3235352c3139332c35382c3134342c37342c34362c38332c3137342c3132362c3130312c3235302c31332c3233342c3131302c39382c3230312c315d2c2261747472735f6465635f6b6579223a225a67356b4543754b735f2d7a784a54616a535f67336643574b52506234387443597a3037746c46623133383d227d7d"
    }
  ],
  "credential_issuers": [
    {
      "id": 0,
      "name": "Ivy",
      "key_pair": "5b7b2261635f7075625f6b6579223a7b2267656e32223a227136457444736c6f78684f5f757075704669444e6836774f77384b6134375a316663706b794a4c56486c52785454444d75673934787432636e4b5f35584c4b424247305679326370384b53686f5a4b48784a46505f78563837663239363358446f475447616a62717130382d336f5265597970756a6e2d776754565832555836222c22787832223a226c5a7557726f70446b30563835347149754377696f7564466a7275584144336d7158454e4559566a3249302d46717367537335655653646d367235326562375646336c5f46654831734436774b347a67383274576946485a714245317456485731705649454e4d57783652324d38594a476b324355314b6f36636f334e597179222c227a7a31223a22746e553179643133744a4742386b5469782d30785647573754717a31472d51667a726a5944746f6a74755f49494b6a69784237456553616e6274665a714c3930222c227a7a32223a2267474558547672795f39506d376461685442387134654e755037344f307a572d304b73747a3755456233415f616b3230764c5a624969474d37653579556e3636457939335673515a5056505976544c736a4d634f36775f42505a3142372d4b4d594d5f70356f4841445953632d6f3565636968423141487047774542365f357a222c22797932223a5b227044365265596a684d7071354f354934787038417a435a32594a634d49564545664138575972665371612d64615354574b756d414e6363456f4a31315a616a564446517275714c424866314c5a344e6c6b636b5534705a4656466c62766955745432364f61305272717a495563725652485750506645625247667665734a6a35225d7d2c226d6170223a7b226d696e5f6372656469745f73636f7265223a5b5b302c315d2c335d7d2c226e756d5f696e7465726e616c5f6174747273223a317d2c7b2261635f7365635f6b6579223a7b2267656e31223a22714c305a7a446c5a4f30683571487743376b4b2d57796e615a4d754b4152416c2d37424f31434f413475596b5f5538463349776871754e346b324168625f6637222c2278223a227533706f7071506979646e50627a4c69426b395935535a45514a727568766c6e4948504a305953587353453d222c2279223a5b226e615f3552763146706264384856454f57766e387a382d38747646354337546a416575376856467a674b513d225d7d2c226d6170223a7b226d696e5f6372656469745f73636f7265223a5b5b302c315d2c335d7d2c226e756d5f696e7465726e616c5f6174747273223a317d5d"
    }
  ],
  "lenders": [
    {
      "id": 0,
      "name": "Lenny",
      "key_pair": "023f37203a2476c42566a61cc55c3ca875dbb4cc41c0deb789f8e7bf881836384d4b18062f8502598de045ca7b69f067f59f93b16e3af8733a988adc2341f5c8",
      "requirements": [
        "500",
        null,
        null
      ],
      "loans": []
    },
    {
      "id": 1,
      "name": "Luna",
      "key_pair": "65efc6564f1c5ee79f65635f249bb082ef5a89d077026c27479ae37db91e48dfe1e2cc04de1ba50705cb9cbba130ddc80f3c2646ddc865b7ab514e8ab77c2e7f",
      "requirements": [
        "680",
        null,
        null
      ],
      "loans": []
    }
  ],
  "borrowers": [
    {
      "id": 0,
      "name": "Ben",
      "key_pair": "f6a12ca8ffc30a66ca140ccc7276336115819361186d3f535dd99f8eaaca8fce7d177f1e71b490ad0ce380f9578ab12bb0fc00a98de8f6a555c81d48c2039249",
      "credentials": 0,
      "loans": [],
      "balance": 0,
      "fiat_utxo": null,
      "fiat_txn_file": null
    }
  ],
  "credentials": [
    {
      "id": 0,
      "borrower": 0,
      "credential_issuer": 0,
      "values": [
          "650",
          null,
          null
      ]
    }
  ],
  "loans": [],
  "fiat_code": null,
  "sequence_number": 1
}"#;
  /// Path to the data file.
  const DATA_FILE: &str = "data.json";
  /// Arbitrary choice of the maximum backup extension number.
  const BACKUP_COUNT_MAX: i32 = 10000;
  /// Port for querying values.
  const QUERY_PORT: &str = "8668";
  /// Port for submitting transactions.
  const SUBMIT_PORT: &str = "8669";

  /// Tuple of blind asset record and associated tracer and owner memos. Memos are optional.
  pub(crate) type BlindAssetRecordAndMemos =
    (BlindAssetRecord, Option<AssetTracerMemo>, Option<OwnerMemo>);
  /// Tuple of tracer and owner memos, optional.
  pub(crate) type TracerAndOwnerMemos = (Option<AssetTracerMemo>, Option<OwnerMemo>);

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
  pub(crate) enum CredentialIndex {
    /// Lower bound of the credit score
    MinCreditScore = 0,
    /// lower bound of the income
    MinIncome = 1,
    /// Country code of citizenship
    // TODO (Keyao): Add a reference for the country code definition.
    Citizenship = 2,
  }

  impl CredentialIndex {
    /// Gets the attribute name.
    pub(crate) fn get_name(self) -> String {
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

  #[derive(Clone, Deserialize, Debug, Serialize)]
  /// Borrower's credential records.
  pub(crate) struct Credential {
    /// Credential ID
    id: u64,
    /// Borrower ID
    borrower: u64,
    /// Credential issuer ID
    credential_issuer: u64,
    /// Credential values, in the order defined in the enum `CredentialIndex`.
    /// Null value indicates the credential value isn't provided yet.
    /// # Examples
    /// * `"attributes": ["630", null, "1"]` indicates:
    /// * Lower bound of the borrower's credit score is 630.
    /// * Lower bound of the borrower's income isn't provided.
    /// * The country code of the borrower's citizenship is 1.
    values: Vec<Option<String>>,
  }

  impl Credential {
    /// Conpub(crate) structs a credential
    /// # Arguments
    /// `id`: credential ID
    /// `borrower`: borrower ID
    /// `credential_issuer`: credential issuer ID
    /// `values`: credential values, in the order defined in the enum `CredentialIndex`.
    pub(crate) fn new(id: u64,
                      borrower: u64,
                      credential_issuer: u64,
                      values: Vec<Option<String>>)
                      -> Self {
      Credential { id,
                   borrower,
                   credential_issuer,
                   values }
    }
  }

  //
  // Users
  //
  #[derive(Clone, Deserialize, Serialize)]
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
    pub(crate) fn new(id: usize, name: String) -> Result<Self, PlatformError> {
      // Generate asset issuer key pair
      let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
      let key_pair_str = hex::encode(key_pair.zei_to_bytes());

      // Generate asset tracer key pair
      let tracer_key_pair = gen_asset_tracer_keypair(&mut ChaChaRng::from_entropy());
      let tracer_key_pair_str =
        serde_json::to_string(&tracer_key_pair).or_else(|_| {
                                                 Err(PlatformError::SerializationError)
                                               })?;

      Ok(AssetIssuer { id: id as u64,
                       name,
                       key_pair: key_pair_str,
                       tracer_key_pair: hex::encode(tracer_key_pair_str) })
    }
  }

  #[derive(Clone, Deserialize, Serialize)]
  /// Credential issuer's account information.
  pub(crate) struct CredentialIssuer {
    /// Credential issuer ID
    id: u64,
    /// Name
    name: String,
    /// Serialized key pair
    key_pair: String,
  }

  impl CredentialIssuer {
    /// Conpub(crate) structs a credential issuer for the credit score attribute.
    pub(crate) fn new(id: usize, name: String) -> Result<Self, PlatformError> {
      let key_pair =
        credential_issuer_key_gen(&mut ChaChaRng::from_entropy(),
                                  &[CredentialIndex::MinCreditScore.get_name_and_length(),
                                    CredentialIndex::MinIncome.get_name_and_length(),
                                    CredentialIndex::Citizenship.get_name_and_length()]);
      let key_pair_str =
        serde_json::to_vec(&key_pair).or_else(|_| Err(PlatformError::SerializationError))?;
      Ok(CredentialIssuer { id: id as u64,
                            name,
                            key_pair: hex::encode(key_pair_str) })
    }
  }

  #[derive(Clone, Deserialize, Serialize)]
  /// Lender's account information.
  pub(crate) struct Lender {
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
    requirements: Vec<Option<String>>,
    /// List of loan IDs
    loans: Vec<u64>,
  }

  impl Lender {
    pub(crate) fn new(id: usize, name: String) -> Self {
      let key_pair = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
      let key_pair_str = hex::encode(key_pair.zei_to_bytes());
      Lender { id: id as u64,
               name,
               key_pair: key_pair_str,
               requirements: vec![None, None, None],
               loans: Vec::new() }
    }
  }

  #[derive(Clone, Deserialize, Serialize)]
  /// Borrower's account information.
  pub(crate) struct Borrower {
    /// Borrower ID
    id: u64,
    /// Name
    name: String,
    /// Serialized key pair
    key_pair: String,
    /// Credential ID, if exists
    credentials: Option<u64>,
    /// List of loan IDs
    loans: Vec<u64>,
    /// Balance
    balance: u64,
    /// Fiat asset UTXO (unspent transaction output) SIDs, if any
    fiat_utxo: Option<TxoSID>,
    /// Path to the most recent fiat asset transaction file, if any
    fiat_txn_file: Option<String>,
  }

  impl Borrower {
    pub(crate) fn new(id: usize, name: String) -> Self {
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
                 fiat_utxo: None,
                 fiat_txn_file: None }
    }
  }

  //
  // Loan
  //
  #[derive(Clone, Deserialize, Debug, PartialEq, Serialize)]
  /// Loan statuses.
  pub(crate) enum LoanStatus {
    /// The borrower has requested the loan, but the lender hasn't fulfill it
    Requested,
    /// The lender has declined the loan
    Declined,
    /// The lender has fulfilled the loan, but the borrower hasn't paid it off
    Active,
    /// The borrower has paid off the loan
    Complete,
  }

  #[derive(Clone, Deserialize, Debug, Serialize)]
  /// Loan information.
  // TODO (Keyao): Remove unused fields
  pub(crate) struct Loan {
    /// Loan ID
    id: u64,
    /// Issuer ID, null if the loan isn't fulfilled          
    issuer: Option<u64>,
    /// Lender ID           
    lender: u64,
    /// Borrower ID          
    borrower: u64,
    /// Loan status, possible values defined in the enum `LoanStatus`
    status: LoanStatus,
    /// Total amount
    amount: u64,
    /// Outstanding balance
    balance: u64,
    /// Interest per 1000
    /// # Examples
    /// * `120`: interest rate is 0.12        
    interest_per_mille: u64,
    /// Loan duration
    duration: u64,
    /// Number of payments that have been made
    payments: u64,
    /// Serialized debt token code, null if the loan isn't fulfilled     
    code: Option<String>,
    /// Debt asset UTXO (unspent transaction output) SIDs, null if the loan isn't fulfilled     
    debt_utxo: Option<TxoSID>,
    /// Path to the most recent debt asset transaction file, null if the loan isn't fulfilled     
    debt_txn_file: Option<String>,
    /// Serialized anon_creds::Credential, null if the loan isn't fulfilled     
    credential: Option<String>,
    /// Serialized credential user secret key, if exists
    user_secret_key: Option<String>,
    /// Serialized credential signature, if exists
    signature: Option<String>,
    /// Serialized credential commitment key, if exists
    commitment_key: Option<String>,
  }

  impl Loan {
    pub(crate) fn new(id: usize,
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
             debt_utxo: None,
             debt_txn_file: None,
             credential: None,
             user_secret_key: None,
             signature: None,
             commitment_key: None }
    }
  }

  //
  // Data
  //
  #[derive(Clone, Deserialize, Serialize)]
  /// Information of users, loans, fiat token code, and sequence number.
  pub(crate) struct Data {
    /// List of user records
    asset_issuers: Vec<AssetIssuer>,
    credential_issuers: Vec<CredentialIssuer>,
    lenders: Vec<Lender>,
    borrowers: Vec<Borrower>,

    /// List of loan records
    loans: Vec<Loan>,

    /// List of credential records
    credentials: Vec<Credential>,

    /// Serialized token code of fiat asset, if defined
    fiat_code: Option<String>,

    /// Sequence number of the next transaction
    sequence_number: u64,
  }

  impl Data {
    pub(crate) fn add_loan(&mut self,
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

    pub(crate) fn add_asset_issuer(&mut self, name: String) -> Result<(), PlatformError> {
      let id = self.asset_issuers.len();
      self.asset_issuers.push(AssetIssuer::new(id, name.clone())?);
      println!("{}'s id is {}.", name, id);
      store_data_to_file(self.clone())
    }

    pub(crate) fn get_asset_issuer_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
      let key_pair_str = &self.asset_issuers[id as usize].key_pair;
      Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
    }

    pub(crate) fn get_asset_tracer_key_pair(&self,
                                            id: u64)
                                            -> Result<AssetTracerKeyPair, PlatformError> {
      let tracer_key_pair_str = &self.asset_issuers[id as usize].tracer_key_pair;
      let tracer_key_pair_decode =
        hex::decode(tracer_key_pair_str).or_else(|_| Err(PlatformError::DeserializationError))?;
      let tracer_key_pair =
        serde_json::from_slice(&tracer_key_pair_decode).or_else(|_| {
                                                         Err(PlatformError::DeserializationError)
                                                       })?;
      Ok(tracer_key_pair)
    }

    pub(crate) fn add_credential_issuer(&mut self, name: String) -> Result<(), PlatformError> {
      let id = self.credential_issuers.len();
      self.credential_issuers
          .push(CredentialIssuer::new(id, name.clone())?);
      println!("{}'s id is {}.", name, id);
      store_data_to_file(self.clone())
    }

    pub(crate) fn get_credential_issuer_key_pair(
      &self,
      id: u64)
      -> Result<(CredIssuerPublicKey, CredIssuerSecretKey), PlatformError> {
      let key_pair_str = &self.credential_issuers[id as usize].key_pair;
      let key_pair_decode =
        hex::decode(key_pair_str).or_else(|_| Err(PlatformError::DeserializationError))?;
      let key_pair =
        serde_json::from_slice(&key_pair_decode).or_else(|_| {
                                                  Err(PlatformError::DeserializationError)
                                                })?;
      Ok(key_pair)
    }

    pub(crate) fn add_lender(&mut self, name: String) -> Result<(), PlatformError> {
      let id = self.lenders.len();
      self.lenders.push(Lender::new(id, name.clone()));
      println!("{}'s id is {}.", name, id);
      store_data_to_file(self.clone())
    }

    pub(crate) fn get_lender_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
      let key_pair_str = &self.lenders[id as usize].key_pair;
      Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
    }

    /// Creates or overwrites a credential requirement.
    /// * If the requirement attribute doesn't exist, add it to the requirements.
    /// * Otherwise, overwrite the value.
    ///
    /// # Arguments
    /// * `lender_id`: lender ID.
    /// * `attribute`: credential attribute, possible names defined in the enum `CredentialIndex`.
    /// * `requirement`: required value.
    pub(crate) fn create_or_overwrite_requirement(&mut self,
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
      store_data_to_file(self.clone())
    }

    pub(crate) fn add_borrower(&mut self, name: String) -> Result<(), PlatformError> {
      let id = self.borrowers.len();
      self.borrowers.push(Borrower::new(id, name.clone()));
      println!("{}'s id is {}.", name, id);
      store_data_to_file(self.clone())
    }

    pub(crate) fn get_borrower_key_pair(&self, id: u64) -> Result<XfrKeyPair, PlatformError> {
      let key_pair_str = &self.borrowers[id as usize].key_pair;
      Ok(XfrKeyPair::zei_from_bytes(&hex::decode(key_pair_str).or_else(|_| {
                                     Err(PlatformError::DeserializationError)
                                   })?))
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
    pub(crate) fn create_or_overwrite_credential(&mut self,
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
        self.credentials[credential_id as usize].values[attribute as usize] =
          Some(value.to_string());
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
      store_data_to_file(self.clone())
    }
  }

  /// Gets the initial data for the CLI.
  pub(crate) fn get_init_data() -> Result<Data, PlatformError> {
    serde_json::from_str::<Data>(INIT_DATA).or(Err(PlatformError::DeserializationError))
  }

  /// Gets the sequence number and increments it.
  pub(crate) fn get_and_update_sequence_number() -> Result<u64, PlatformError> {
    // Get the sequence number
    let mut data = load_data()?;
    let sequence_number = data.sequence_number;
    println!("Sequence number: {}", sequence_number);

    // Increment the sequence number
    data.sequence_number += 1;
    store_data_to_file(data)?;

    Ok(sequence_number)
  }

  //
  // Load functions
  //
  /// Loads data.
  /// * If the data file exists, loads data from it.
  /// * Otherwise, stores the initial data to file and returns the data.
  pub(crate) fn load_data() -> Result<Data, PlatformError> {
    let data = match fs::read_to_string(DATA_FILE) {
      Ok(data) => data,
      Err(_) => {
        let init_data = get_init_data()?;
        store_data_to_file(init_data.clone())?;
        return Ok(init_data);
      }
    };
    serde_json::from_str::<Data>(&data).or(Err(PlatformError::DeserializationError))
  }

  /// Loads transaction record from file
  /// # Arguments
  /// * `file_path`: file path.
  pub(crate) fn load_txn_from_file(file_path: &str) -> Result<TransactionBuilder, PlatformError> {
    let txn = fs::read_to_string(file_path).or_else(|_| {
                Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
              })?;
    println!("Parsing builder from file contents: \"{}\"", &txn);
    match serde_json::from_str(&txn) {
      Ok(builder) => Ok(builder),
      Err(_) => Err(PlatformError::DeserializationError),
    }
  }

  /// Split a string by comma (`,`).
  /// # Arguments
  /// * `string`: string to split
  pub(crate) fn split_arg(string: &str) -> Vec<&str> {
    string.split(',').collect::<Vec<&str>>()
  }

  /// Loads UTXO (unspent transaction output) SIDs from file.
  /// # Arguments
  /// * `file_path`: file path
  pub(crate) fn load_sids_from_file(file_path: &str) -> Result<Vec<u64>, PlatformError> {
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
    println!("Parsing builder from file contents: \"{}\"", &txn);
    match serde_json::from_str::<TransactionBuilder>(&txn) {
      Ok(builder) => Ok(((builder.get_owner_record_and_memo(0).unwrap().0.clone()).0,
                         builder.get_owner_record_and_memo(0).unwrap().1.clone())),
      Err(_) => Err(PlatformError::DeserializationError),
    }
  }

  /// Loads blind asset records and optional owner memos from transaction files.
  /// # Arguments
  /// * `file_paths`: file paths to transaction records.
  pub(crate) fn load_blind_asset_records_and_owner_memos_from_files(
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

  /// Loads the open asset record by getting the blind asset record and owner memo from transaction file.
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
  pub(crate) fn load_tracer_memo_from_file(file_path: &str)
                                           -> Result<AssetTracerMemo, PlatformError> {
    let tracer_memo = fs::read_to_string(file_path).or_else(|_| {
                        Err(PlatformError::IoError(format!("Failed to read file: {}", file_path)))
                      })?;
    println!("Parsing tracer memo from file contents: \"{}\"",
             &tracer_memo);
    serde_json::from_str::<AssetTracerMemo>(&tracer_memo).or_else(|_| {
                                                           Err(PlatformError::DeserializationError)
                                                         })
  }

  /// Loads tracer and owner memos from memo files
  /// # Arguments
  /// * `file_paths`: file paths to the tracer and owner memos.
  pub(crate) fn load_tracer_and_owner_memos_from_files(
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
        Err(_) => {
          return Err(PlatformError::DeserializationError);
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
  pub(crate) fn store_data_to_file(data: Data) -> Result<(), PlatformError> {
    if let Ok(as_json) = serde_json::to_string(&data) {
      if let Err(error) = fs::write(DATA_FILE, &as_json) {
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
  pub(crate) fn store_txn_to_file(path_str: &str,
                                  txn: &TransactionBuilder)
                                  -> Result<(), PlatformError> {
    if let Ok(as_json) = serde_json::to_string(txn) {
      if let Err(error) = fs::write(path_str, &as_json) {
        return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                  path_str, error)));
      };
    }
    Ok(())
  }

  /// Stores SIDs to file.
  /// # Arguments
  /// * `path_str`: file path to store the key pair.
  /// * `sids`: SIDs to store, separated by comma (`,`).
  pub(crate) fn store_sids_to_file(path_str: &str, sids: &str) -> Result<(), PlatformError> {
    if let Err(error) = fs::write(path_str, sids) {
      return Err(PlatformError::IoError(format!("Failed to create file {}: {}.",
                                                path_str, error)));
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
  pub(crate) fn store_tracer_and_owner_memos_to_file(path_str: &str,
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

  /// Gets and stores tracer and owner memos to file.
  /// # Arguments
  /// * `path_str`: file path to store the tracer and owner memos.
  /// * `pub_key`: issuer public key.
  /// * `amount`: asset amount.
  /// * `token_code`: asset token code.
  /// * `record_type`: booleans representing whether the amount and asset are confidential.
  pub(crate) fn get_and_store_memos_to_file(path_str: &str,
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
  pub(crate) fn rename_existing_path(path: &Path) -> Result<(), PlatformError> {
    let next = next_path(path)?;
    trace!("Next path for {:?} is {:?}", &path, &next);
    if let Err(error) = fs::rename(path, next.as_path()) {
      return Err(PlatformError::IoError(format!("Failed to rename path: {}", error)));
    }
    Ok(())
  }

  /// Parses a string to u64.
  /// # Arguments
  /// * `val_str`: string representation of a value.
  pub(crate) fn parse_to_u64(val_str: &str) -> Result<u64, PlatformError> {
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
  pub(crate) fn parse_to_u64_vec(vals_str: &str) -> Result<Vec<u64>, PlatformError> {
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

  pub(crate) fn air_assign(issuer_id: u64,
                           address: &str,
                           data: &str,
                           issuer_pk: &str,
                           pok: &str,
                           txn_file: &str)
                           -> Result<(), PlatformError> {
    let issuer_data = load_data()?;
    let xfr_key_pair = issuer_data.get_asset_issuer_key_pair(issuer_id)?;
    let mut txn_builder = TransactionBuilder::default();
    let address = serde_json::from_str::<CredUserPublicKey>(address)?;
    let data = serde_json::from_str::<CredCommitment>(data)?;
    let issuer_pk = serde_json::from_str::<CredIssuerPublicKey>(issuer_pk)?;
    let pok = serde_json::from_str::<CredPoK>(pok)?;
    txn_builder.add_operation_air_assign(&xfr_key_pair, address, data, issuer_pk, pok)?;
    store_txn_to_file(&txn_file, &txn_builder)?;
    Ok(())
  }

  /// Defines an asset.
  ///
  /// Note: the transaction isn't submitted until `submit` or `submit_and_get_sids` is called.
  ///
  /// # Arguments
  /// * `fiat_asset`: whether the asset is a fiat asset.
  /// * `issuer_key_pair`: asset issuer's key pair.
  /// * `token_code`: asset token code.
  /// * `memo`: memo for defining the asset.
  /// * `asset_rules`: simple asset rules (e.g. traceable, transferable)
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn define_asset(fiat_asset: bool,
                             issuer_key_pair: &XfrKeyPair,
                             token_code: AssetTypeCode,
                             memo: &str,
                             asset_rules: AssetRules,
                             txn_file: &str)
                             -> Result<TransactionBuilder, PlatformError> {
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation_create_asset(issuer_key_pair,
                                           Some(token_code),
                                           asset_rules,
                                           &memo,
                                           PolicyChoice::Fungible())?;
    store_txn_to_file(&txn_file, &txn_builder)?;

    // Update data
    let mut data = load_data()?;
    if fiat_asset {
      data.fiat_code = Some(token_code.to_base64());
      store_data_to_file(data)?;
    };
    Ok(txn_builder)
  }

  /// Defines an asset and submits the transaction with the standalone ledger.
  pub fn define_and_submit(issuer_key_pair: &XfrKeyPair,
                           code: AssetTypeCode,
                           rules: AssetRules,
                           ledger_standalone: &LedgerStandalone)
                           -> Result<(), PlatformError> {
    // Define the asset
    let mut txn_builder = TransactionBuilder::default();
    let txn = txn_builder.add_operation_create_asset(issuer_key_pair,
                                                     Some(code),
                                                     rules,
                                                     "",
                                                     PolicyChoice::Fungible())?
                         .transaction();

    // Submit the transaction
    ledger_standalone.submit_transaction(&txn);

    Ok(())
  }

  #[allow(clippy::too_many_arguments)]
  /// Issues and transfers asset.
  /// # Arguments
  /// * `issuer_key_pair`: asset issuer's key pair.
  /// * `recipient_key_pair`: rercipient's key pair.
  /// * `amount`: amount to issue and transfer.
  /// * `token_code`: asset token code.
  /// * `record_type`: booleans representing whether the amount and asset transfer are confidential.
  ///   Asset issuance is always nonconfidential.
  /// * `memo_file`: path to store the tracer and owner memos, optional.
  /// * `txn_file`: path to the transaction file.
  /// * `tracing_policy`: asset tracing policy, if any.
  #[allow(clippy::too_many_arguments)]
  pub(crate) fn issue_and_transfer_asset(issuer_key_pair: &XfrKeyPair,
                                         recipient_key_pair: &XfrKeyPair,
                                         amount: u64,
                                         token_code: AssetTypeCode,
                                         record_type: AssetRecordType,
                                         credential_record: Option<(&CredUserSecretKey,
                                                 &ZeiCredential,
                                                 &CredCommitmentKey)>,
                                         txn_file: &str,
                                         tracing_policy: Option<AssetTracingPolicy>)
                                         -> Result<TransactionBuilder, PlatformError> {
    // Asset issuance is always nonconfidential
    let (blind_asset_record, _, owner_memo) =
    get_blind_asset_record_and_memos(issuer_key_pair.get_pk(),
                                     amount,
                                     token_code,
                                     AssetRecordType::from_booleans(record_type.is_confidential_amount(), false),
                                     tracing_policy.clone())?;

    // Transfer Operation
    let output_template = if let Some(policy) = tracing_policy.clone() {
      AssetRecordTemplate::with_asset_tracking(amount,
                                               token_code.val,
                                               record_type,
                                               recipient_key_pair.get_pk(),
                                               policy)
    } else {
      AssetRecordTemplate::with_no_asset_tracking(amount,
                                                  token_code.val,
                                                  record_type,
                                                  recipient_key_pair.get_pk())
    };
    let xfr_op =
    TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                              open_blind_asset_record(&blind_asset_record,
                                                                &owner_memo,
                                                                issuer_key_pair.get_sk_ref())
                                              .map_err(|e| PlatformError::ZeiError(error_location!(),e))?,
                                              None,
                                              amount)?
                                   .add_output(&output_template, credential_record)?
                                   .balance()?
                                   .create(TransferType::Standard)?
                                   .sign(issuer_key_pair)?
                                   .transaction()?;

    // Issue and Transfer transaction
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation_issue_asset(issuer_key_pair,
                                          &token_code,
                                          get_and_update_sequence_number()?,
                                          &[(TxOutput(blind_asset_record), owner_memo)],
                                          tracing_policy)?
               .add_operation(xfr_op)
               .transaction();

    store_txn_to_file(txn_file, &txn_builder)?;
    Ok(txn_builder)
  }

  /// Issues and transfers an asset, submits the transactio with the standalone ledger, and get the UTXO SID, amount blinds and type blind.
  #[allow(clippy::too_many_arguments)]
  pub fn issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore>(
    issuer_key_pair: &XfrKeyPair,
    recipient_key_pair: &XfrKeyPair,
    amount: u64,
    code: AssetTypeCode,
    record_type: AssetRecordType,
    sequence_number: u64,
    mut prng: &mut R,
    ledger_standalone: &LedgerStandalone)
    -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError> {
    // Issue and transfer the asset
    let pc_gens = PublicParams::new().pc_gens;
    let input_template = AssetRecordTemplate::with_no_asset_tracking(amount, code.val, AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, issuer_key_pair.get_pk());
    let input_blind_asset_record =
      build_blind_asset_record(&mut prng, &pc_gens, &input_template, None).0;
    let output_template = AssetRecordTemplate::with_no_asset_tracking(amount,
                                                                      code.val,
                                                                      record_type,
                                                                      recipient_key_pair.get_pk());
    let blinds = &mut ((Scalar::default(), Scalar::default()), Scalar::default());
    let xfr_op = TransferOperationBuilder::new().add_input(TxoRef::Relative(0),
                                                           open_blind_asset_record(&input_blind_asset_record,
                                                                                   &None,
                                                                                   issuer_key_pair.get_sk_ref()).map_err(|e| {
                                                                                     PlatformError::ZeiError(error_location!(), e)
                                                                                   })?,
                                                           None,
                                                           amount)?
                                                .add_output_and_store_blinds(&output_template, None, prng, blinds)?.balance()?
                                                .create(TransferType::Standard)?
                                                .sign(issuer_key_pair)?
                                                .transaction()?;

    let mut txn_builder = TransactionBuilder::default();
    let txn = txn_builder.add_operation_issue_asset(issuer_key_pair,
                                                    &code,
                                                    sequence_number,
                                                    &[(TxOutput(input_blind_asset_record), None)],
                                                    None)?
                         .add_operation(xfr_op)
                         .transaction();

    // Submit the transaction, and get the UTXO and asset type blind
    Ok((ledger_standalone.submit_transaction_and_fetch_utxos(&txn)[0].0, blinds.0, blinds.1))
  }

  /// Defines, issues and transfers an asset, and submits the transactions with the standalone ledger.
  /// Returns the UTXO SID, the blinding factors for the asset amount, and the blinding factor for the asset type code.
  #[allow(clippy::too_many_arguments)]
  pub fn define_issue_transfer_and_get_utxo_and_blinds<R: CryptoRng + RngCore>(
    issuer_key_pair: &XfrKeyPair,
    recipient_key_pair: &XfrKeyPair,
    amount: u64,
    code: AssetTypeCode,
    rules: AssetRules,
    record_type: AssetRecordType,
    ledger_standalone: &LedgerStandalone,
    prng: &mut R)
    -> Result<(u64, (Scalar, Scalar), Scalar), PlatformError> {
    // Define the asset
    define_and_submit(issuer_key_pair, code, rules, ledger_standalone)?;

    // Issue and transfer the asset, and get the UTXO SID and asset type blind
    issue_transfer_and_get_utxo_and_blinds(issuer_key_pair,
                                           recipient_key_pair,
                                           amount,
                                           code,
                                           record_type,
                                           1,
                                           prng,
                                           ledger_standalone)
  }

  /// Queries a value.
  ///
  /// # Arguments
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `localhost`.
  /// * `port`: either `QUERY_PORT` or `SUBMIT_PORT`.
  /// * `route`: route to query.
  /// * `value`: value to look up.
  ///
  /// # Examples
  /// * To query the BlindAssetRecord with utxo_sid 100 from https://testnet.findora.org:
  /// use txn_cli::txn_lib::query;
  /// query("https", "testnet.findora.org", QUERY_PORT, "utxo_sid", "100").unwrap();
  fn query(protocol: &str,
           host: &str,
           port: &str,
           route: &str,
           value: &str)
           -> Result<String, PlatformError> {
    let mut res = if let Ok(response) =
      reqwest::get(&format!("{}://{}:{}/{}/{}", protocol, host, port, route, value))
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

  /// Queries the UTXO SID and gets the asset type commitment.
  /// Asset should be confidential, otherwise the commitmemt will be null.
  pub fn query_utxo_and_get_type_commitment(utxo: u64,
                                            protocol: &str,
                                            host: &str)
                                            -> Result<CompressedRistretto, PlatformError> {
    let res = query(protocol, host, QUERY_PORT, "utxo_sid", &format!("{}", utxo))?;
    let blind_asset_record =
      serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                      Err(PlatformError::DeserializationError)
                                                    })?;
    match blind_asset_record.asset_type {
      XfrAssetType::Confidential(commitment) => Ok(commitment),
      _ => {
        println!("Found nonconfidential asset.");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Queries the UTXO SID to get the amount, either confidential or nonconfidential.
  pub fn query_utxo_and_get_amount(utxo: u64,
                                   protocol: &str,
                                   host: &str)
                                   -> Result<XfrAmount, PlatformError> {
    let res = query(protocol, host, QUERY_PORT, "utxo_sid", &format!("{}", utxo))?;
    let blind_asset_record =
      serde_json::from_str::<BlindAssetRecord>(&res).or_else(|_| {
                                                      Err(PlatformError::DeserializationError)
                                                    })?;
    Ok(blind_asset_record.amount)
  }

  /// Submits a transaction.
  ///
  /// Either this function or `submit_and_get_sids` should be called after a transaction is composed by any of the following:
  /// * `air_assign`
  /// * `define_asset`
  /// * `issue_asset`
  /// * `transfer_asset`
  /// * `issue_and_transfer_asset`
  ///
  /// # Arguments
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `localhost`.
  /// * `txn_builder`: transation builder.
  pub(crate) fn submit(protocol: &str,
                       host: &str,
                       txn_builder: TransactionBuilder)
                       -> Result<(), PlatformError> {
    // Submit transaction
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
    let txt = res.text().expect("no response");
    let handle = serde_json::from_str::<TxnHandle>(&txt).unwrap_or_else(|e| {
                                                          panic!("<Invalid JSON> ({}): \"{}\"",
                                                                 &e, &txt)
                                                        });
    println!("Submission response: {}", handle);
    println!("Submission status: {}", res.status());

    Ok(())
  }

  /// Submits a transaction and gets the UTXO (unspent transaction output) SIDs.
  ///
  /// Either this function or `submit` should be called after a transaction is composed by any of the following:
  /// * `air_assign`
  /// * `define_asset`
  /// * `issue_asset`
  /// * `transfer_asset`
  /// * `issue_and_transfer_asset`
  ///
  /// # Arguments
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `localhost`.
  /// * `txn_builder`: transation builder.
  pub(crate) fn submit_and_get_sids(protocol: &str,
                                    host: &str,
                                    txn_builder: TransactionBuilder)
                                    -> Result<Vec<TxoSID>, PlatformError> {
    // Submit transaction
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
    let txt = res.text().expect("no response");
    let handle = serde_json::from_str::<TxnHandle>(&txt).unwrap_or_else(|e| {
                                                          panic!("<Invalid JSON> ({}): \"{}\"",
                                                                 &e, &txt)
                                                        });
    println!("Submission response: {}", handle);
    println!("Submission status: {}", res.status());

    // Return sid
    let res = query(protocol, host, SUBMIT_PORT, "txn_status", &handle.0)?;
    match serde_json::from_str::<TxnStatus>(&res).or_else(|_| {
                                                   Err(PlatformError::DeserializationError)
                                                 })? {
      TxnStatus::Committed((_sid, txos)) => Ok(txos),
      _ => Err(PlatformError::DeserializationError),
    }
  }

  /// Gets the blind asset record and associated memos.
  /// # Arguments
  /// * `pub_key`: public key of the asset record.
  /// * `amount`: amount of the asset record.
  /// * `token_code`: token code of the asset rercord.
  /// * `asset_record_type`: booleans representing whether the amount and asset are confidential.
  /// * `tracing_policy`: asset tracing policy, optional.
  pub(crate) fn get_blind_asset_record_and_memos(
    pub_key: XfrPublicKey,
    amount: u64,
    token_code: AssetTypeCode,
    asset_record_type: AssetRecordType,
    tracing_policy: Option<AssetTracingPolicy>)
    -> Result<BlindAssetRecordAndMemos, PlatformError> {
    let template = if let Some(policy) = tracing_policy {
      AssetRecordTemplate::with_asset_tracking(amount,
                                               token_code.val,
                                               asset_record_type,
                                               pub_key,
                                               policy)
    } else {
      AssetRecordTemplate::with_no_asset_tracking(amount,
                                                  token_code.val,
                                                  asset_record_type,
                                                  pub_key)
    };
    let mut prng = ChaChaRng::from_entropy();
    let params = PublicParams::new();
    Ok(build_blind_asset_record(&mut prng, &params.pc_gens, &template, None))
  }

  /// Merges two asset records.
  /// # Arguments
  /// * `key_pair`: key pair of the two records.
  /// * `sid1`: SID of the first record.
  /// * `sid2`: SID of the second record.
  /// * `blind_asset_record1`: blind asset record of the first record.
  /// * `blind_asset_record2`: blind asset record of the second record.
  /// * `token_code`: asset token code of the two records.
  /// * `tracing_policy`: asset tracing policy, optional.
  pub(crate) fn merge_records(key_pair: &XfrKeyPair,
                              sid1: TxoRef,
                              sid2: TxoRef,
                              blind_asset_record1: (BlindAssetRecord, Option<OwnerMemo>),
                              blind_asset_record2: (BlindAssetRecord, Option<OwnerMemo>),
                              token_code: AssetTypeCode,
                              tracing_policy: Option<AssetTracingPolicy>)
                              -> Result<TransactionBuilder, PlatformError> {
    let oar1 =
      open_blind_asset_record(&blind_asset_record1.0,
                              &blind_asset_record1.1,
                              key_pair.get_sk_ref()).map_err(|e| {
                                                      PlatformError::ZeiError(error_location!(), e)
                                                    })?;
    let oar2 =
      open_blind_asset_record(&blind_asset_record2.0,
                              &blind_asset_record2.1,
                              key_pair.get_sk_ref()).map_err(|e| {
                                                      PlatformError::ZeiError(error_location!(), e)
                                                    })?;
    if oar1.get_record_type() != oar2.get_record_type() {
      return Err(PlatformError::InputsError(error_location!()));
    }
    let amount1 = *oar1.get_amount();
    let amount2 = *oar2.get_amount();

    // Transfer Operation
    let template = if let Some(policy) = tracing_policy {
      AssetRecordTemplate::with_asset_tracking(amount1 + amount2,
                                               token_code.val,
                                               oar1.get_record_type(),
                                               key_pair.get_pk(),
                                               policy)
    } else {
      AssetRecordTemplate::with_no_asset_tracking(amount1 + amount2,
                                                  token_code.val,
                                                  oar1.get_record_type(),
                                                  key_pair.get_pk())
    };
    let xfr_op = TransferOperationBuilder::new().add_input(sid1, oar1, None, amount1)?
                                                .add_input(sid2, oar2, None, amount2)?
                                                .add_output(&template, None)?
                                                .create(TransferType::Standard)?
                                                .sign(key_pair)?
                                                .transaction()?;

    // Merge records
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation(xfr_op).transaction();
    Ok(txn_builder)
  }

  /// Loads funds.
  /// # Arguments
  /// * `issuer_id`: issuer ID.
  /// * `recipient_id`: recipient's ID.
  /// * `amount`: amount to load.
  /// * `memo_file`: path to store the tracer and owner memos, optional.
  /// * `txn_file`: path to store the transaction file.
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `localhost`.
  pub(crate) fn load_funds(issuer_id: u64,
                           recipient_id: u64,
                           amount: u64,
                           txn_file: &str,
                           protocol: &str,
                           host: &str)
                           -> Result<(), PlatformError> {
    // Get data
    let data = load_data()?;
    let issuer_key_pair = &data.get_asset_issuer_key_pair(issuer_id)?;
    let recipient = &data.borrowers[recipient_id as usize];
    let recipient_key_pair = &data.clone().get_borrower_key_pair(recipient_id)?;

    // Get or define fiat asset
    let token_code = if let Some(code) = &data.fiat_code {
      AssetTypeCode::new_from_base64(code)?
    } else {
      let fiat_code = AssetTypeCode::gen_random();
      let txn_builder = define_asset(true,
                                     issuer_key_pair,
                                     fiat_code,
                                     "Fiat asset",
                                     AssetRules::default(),
                                     txn_file)?;
      // Store data before submitting the transaction to avoid data overwriting
      let data = load_data()?;
      submit(protocol, host, txn_builder)?;
      store_data_to_file(data)?;
      fiat_code
    };

    // Issue and transfer asset
    let txn_builder =
      issue_and_transfer_asset(issuer_key_pair,
                               recipient_key_pair,
                               amount,
                               token_code,
                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                               None,
                               txn_file,
                               None)?;

    // Submit transaction and get the new record
    let sid_new = submit_and_get_sids(protocol, host, txn_builder)?[0];
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
    let sid_merged = if let Some(sid_pre) = recipient.fiat_utxo {
      let res_pre = query(protocol,
                          host,
                          QUERY_PORT,
                          "utxo_sid",
                          &format!("{}", sid_pre.0))?;
      let blind_asset_record_pre =
        serde_json::from_str::<BlindAssetRecord>(&res_pre).or_else(|_| {
                                                            Err(PlatformError::DeserializationError)
                                                          })?;
      let txn_builder = merge_records(recipient_key_pair,
                                      TxoRef::Absolute(sid_pre),
                                      TxoRef::Absolute(sid_new),
                                      (blind_asset_record_pre, None), // no associated owner memo with blind asset record
                                      (blind_asset_record_new, None), // no associated owner memo with blind asset record
                                      token_code,
                                      None)?;

      // Store the transaction to file so that the merged blind asset record and owner memo can be retrieved
      store_txn_to_file(txn_file, &txn_builder)?;
      submit_and_get_sids(protocol, host, txn_builder)?[0]
    } else {
      sid_new
    };

    // Update data
    let mut data = load_data()?;
    data.borrowers[recipient_id as usize].balance = recipient.balance + amount;
    data.borrowers[recipient_id as usize].fiat_utxo = Some(sid_merged);
    store_data_to_file(data)
  }

  /// Querys the blind asset record by querying the UTXO (unspent transaction output) SID.
  /// # Arguments
  /// * `txn_file`: path to the transaction file.
  /// * `key_pair`: key pair of the asset record.
  /// * `owner_memo`: Memo associated with utxo.
  pub(crate) fn query_open_asset_record(protocol: &str,
                                        host: &str,
                                        sid: TxoSID,
                                        key_pair: &XfrKeyPair,
                                        owner_memo: &Option<OwnerMemo>)
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
    open_blind_asset_record(&blind_asset_record, owner_memo, key_pair.get_sk_ref()).or_else(|error| {
                                                                                    Err(PlatformError::ZeiError(error_location!(), error))
                                                                                  })
  }

  /// Fulfills a loan.
  /// # Arguments
  /// * `loan_id`: loan ID.
  /// * `issuer_id`: issuer ID.
  /// * `txn_file`: path to store the transaction file.
  /// * `memo_file`: path to store the asset tracer memo and owner memo, optional.
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `locaohost`.
  pub(crate) fn fulfill_loan(loan_id: u64,
                             issuer_id: u64,
                             txn_file: &str,
                             memo_file: Option<&str>,
                             protocol: &str,
                             host: &str)
                             -> Result<(), PlatformError> {
    // Get data
    let mut data = load_data()?;
    let issuer_key_pair = &data.get_asset_issuer_key_pair(issuer_id)?;
    let loan = &data.loans[loan_id as usize].clone();

    // Check if loan has been fulfilled
    match loan.status {
      LoanStatus::Declined => {
        println!("Loan {} has already been declined.", loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      LoanStatus::Active => {
        println!("Loan {} has already been fulfilled.", loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      LoanStatus::Complete => {
        println!("Loan {} has already been paid off.", loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      _ => {}
    }

    let tracer_enc_keys = data.get_asset_tracer_key_pair(issuer_id)?.enc_key;
    let lender_id = loan.lender;
    let lender = &data.lenders[lender_id as usize];
    let lender_key_pair = &data.get_lender_key_pair(loan.lender)?;
    let borrower_id = loan.borrower;
    let borrower = &data.borrowers[borrower_id as usize];
    let borrower_key_pair = &data.get_borrower_key_pair(borrower_id)?;
    let amount = loan.amount;

    // Credential check
    let credential_id = if let Some(id) = borrower.credentials {
      id as usize
    } else {
      println!("Credential is required. Use create_or_overwrite_credential.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let credential = &data.credentials[credential_id as usize];
    let credential_issuer_id = credential.credential_issuer;

    // Check if the credential values meet the requirements
    let values = credential.values.clone();
    let mut value_iter = values.iter();
    let requirements = lender.requirements.clone();
    let mut requirement_iter = requirements.iter();
    let mut count = 0;
    let mut attributes = Vec::new();
    let mut attribute_names = Vec::new();
    let mut attibutes_with_value_as_vec = Vec::new();
    let mut reveal_map = Vec::new();

    // For each credential attribute:
    // If the lender doesn't have a requirement, skip it
    // Otherwise:
    // * If the borrower doesn't provide the corresponding attribute value, return an error
    // * Otherwise, check if the value meets the requirement
    while count < 3 {
      if let Some(requirement_next) = requirement_iter.next() {
        if let Some(requirement) = requirement_next {
          if let Some(value_next) = value_iter.next() {
            if let Some(value) = value_next {
              let requirement_u64 = parse_to_u64(requirement)?;
              let requirement_type = CredentialIndex::get_requirement_type(count);
              match requirement_type {
                ComparisonType::AtLeast => {
                  if parse_to_u64(value)? < requirement_u64 {
                    // Update loans data
                    data.loans[loan_id as usize].status = LoanStatus::Declined;
                    store_data_to_file(data)?;
                    println!("Credential value should be at least: {}.", requirement_u64);
                    return Err(PlatformError::InputsError(error_location!()));
                  }
                }
                _ => {
                  if parse_to_u64(value)? != requirement_u64 {
                    // Update loans data
                    data.loans[loan_id as usize].status = LoanStatus::Declined;
                    store_data_to_file(data)?;
                    println!("Credit score should be: {}.", requirement_u64);
                    return Err(PlatformError::InputsError(error_location!()));
                  }
                }
              }
              let attribute = CredentialIndex::get_credential_index(count)?;
              let value_bytes = value.as_bytes();
              attributes.push((attribute.get_name().to_string(), value_bytes));
              attribute_names.push(attribute.get_name().to_string());
              attibutes_with_value_as_vec.push((attribute.get_name().to_string(),
                                                value_bytes.to_vec()));
              reveal_map.push(true);
            } else {
              println!("Missing credential value. Use subcommand borrower create_or_overwrite_credential.");
              return Err(PlatformError::InputsError(error_location!()));
            }
          } else {
            println!("More credential value expected.");
            return Err(PlatformError::InputsError(error_location!()));
          }
        }
      } else {
        println!("More credential requirement expected.");
        return Err(PlatformError::InputsError(error_location!()));
      }
      count += 1;
    }

    // Prove and attest the credential
    let (credential_issuer_public_key, credential_issuer_secret_key) =
      data.get_credential_issuer_key_pair(credential_issuer_id)?;
    let mut prng: ChaChaRng = ChaChaRng::from_entropy();
    let (user_pk, user_secret_key) =
      credential_user_key_gen(&mut prng, &credential_issuer_public_key);
    let user_sk_str =
      serde_json::to_vec(&user_secret_key).or_else(|_| Err(PlatformError::SerializationError))?;
    let signature = credential_sign(&mut prng,
                                    &credential_issuer_secret_key,
                                    &user_pk,
                                    &attributes).unwrap();
    let signature_str =
      serde_json::to_string(&signature).or_else(|_| Err(PlatformError::SerializationError))?;
    let wrapper_credential = WrapperCredential { attributes: attibutes_with_value_as_vec,
                                                 issuer_pub_key:
                                                   credential_issuer_public_key.clone(),
                                                 signature };
    let ac_credential =
      wrapper_credential.to_ac_credential()
                        .or_else(|e| Err(PlatformError::ZeiError(error_location!(), e)))?;
    let reveal_sig =
      credential_reveal(&mut prng,
                        &user_secret_key,
                        &wrapper_credential,
                        &attribute_names).or_else(|error| {
                                           Err(PlatformError::ZeiError(error_location!(), error))
                                         })?;
    credential_verify(&credential_issuer_public_key,
                      &attributes,
                      &reveal_sig.sig_commitment,
                      &reveal_sig.pok).or_else(|error| {
                                        Err(PlatformError::ZeiError(error_location!(), error))
                                      })?;
    let commitment_key = credential_keygen_commitment(&mut prng);
    let commitment_key_str =
      serde_json::to_vec(&commitment_key).or_else(|_| Err(PlatformError::SerializationError))?;

    // Update credential data
    data.loans[loan_id as usize].user_secret_key = Some(hex::encode(user_sk_str));
    data.loans[loan_id as usize].signature = Some(signature_str);
    data.loans[loan_id as usize].commitment_key = Some(hex::encode(commitment_key_str));
    store_data_to_file(data.clone())?;

    // Store the tracer memo to file
    if let Some(file) = memo_file {
      let ciphertext =
        ac_confidential_open_commitment(&mut prng,
                                        &user_secret_key.get_ref(),
                                        &ac_credential,
                                        &commitment_key,
                                        &tracer_enc_keys.attrs_enc_key,
                                        &reveal_map,
                                        &[]).or_else(|e| {
                                              Err(PlatformError::ZeiError(error_location!(), e))
                                            })?
                                            .ctexts;
      let tracer_memo = AssetTracerMemo { enc_key: tracer_enc_keys.clone(),
                                          lock_amount: None,
                                          lock_asset_type: None,
                                          lock_attributes: Some(ciphertext) };
      store_tracer_memo_to_file(file, tracer_memo)?;
    }

    // Get or define fiat asset
    let fiat_code = if let Some(code) = data.fiat_code.clone() {
      println!("Fiat code: {}", code);
      AssetTypeCode::new_from_base64(&code)?
    } else {
      let fiat_code = AssetTypeCode::gen_random();
      let txn_builder = define_asset(true,
                                     issuer_key_pair,
                                     fiat_code,
                                     "Fiat asset",
                                     AssetRules::default(),
                                     txn_file)?;
      // Store data before submitting the transaction to avoid data overwriting
      let data = load_data()?;
      submit(protocol, host, txn_builder)?;
      store_data_to_file(data)?;
      fiat_code
    };

    // Get tracing policies
    let identity_policy = IdentityRevealPolicy { cred_issuer_pub_key:
                                                   credential_issuer_public_key.get_ref().clone(),
                                                 reveal_map };
    let debt_tracing_policy = AssetTracingPolicy { enc_keys: tracer_enc_keys,
                                                   asset_tracking: true,
                                                   identity_tracking: Some(identity_policy) };

    // Issue and transfer fiat token
    let credential_record = Some((&user_secret_key, &ac_credential, &commitment_key));
    let mut fiat_txn_file = txn_file.to_owned();
    fiat_txn_file.push_str(&format!(".fiat.{}", borrower_id));
    let txn_builder =
      issue_and_transfer_asset(issuer_key_pair,
                               lender_key_pair,
                               amount,
                               fiat_code,
                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                               None,
                               &fiat_txn_file,
                               None)?;
    let fiat_sid = submit_and_get_sids(protocol, host, txn_builder)?[0];
    println!("Fiat sid: {}", fiat_sid.0);
    let (_, owner_memo) = load_blind_asset_record_and_owner_memo_from_file(&fiat_txn_file)?;
    let fiat_open_asset_record =
      query_open_asset_record(protocol, host, fiat_sid, lender_key_pair, &owner_memo)?;

    // Define debt asset
    let debt_code = AssetTypeCode::gen_random();
    println!("Generated debt code: {}",
             serde_json::to_string(&debt_code.val).or_else(|_| {
                                                    Err(PlatformError::SerializationError)
                                                  })?);
    let memo = DebtMemo { interest_rate: Fraction::new(loan.interest_per_mille, 1000),
                          fiat_code,
                          loan_amount: amount };
    let memo_str =
      serde_json::to_string(&memo).or_else(|_| Err(PlatformError::SerializationError))?;
    let txn_builder = define_asset(false,
                                   borrower_key_pair,
                                   debt_code,
                                   &memo_str,
                                   AssetRules::default().set_traceable(true).clone(),
                                   txn_file)?;
    // Store data before submitting the transaction to avoid data overwriting
    let data = load_data()?;
    submit(protocol, host, txn_builder)?;
    store_data_to_file(data)?;

    // Issue and transfer debt token
    let mut debt_txn_file = txn_file.to_owned();
    debt_txn_file.push_str(&format!(".debt.{}", loan_id));
    let txn_builder =
      issue_and_transfer_asset(borrower_key_pair,
                               borrower_key_pair,
                               amount,
                               debt_code,
                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                               credential_record,
                               &debt_txn_file,
                               Some(debt_tracing_policy.clone()))?;
    let debt_sid = submit_and_get_sids(protocol, host, txn_builder)?[0];
    println!("Debt sid: {}", debt_sid.0);
    let debt_open_asset_record =
      load_open_asset_record_from_file(&debt_txn_file, borrower_key_pair)?;

    // Initiate loan
    let lender_template =
      AssetRecordTemplate::with_asset_tracking(amount,
                                               debt_code.val,
                                               AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                               lender_key_pair.get_pk(),
                                               debt_tracing_policy);
    let borrower_template =
      AssetRecordTemplate::with_no_asset_tracking(amount,
                                                  fiat_code.val,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  borrower_key_pair.get_pk());
    let xfr_op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(fiat_sid),
                                                           fiat_open_asset_record,
                                                           None,
                                                           amount)?
                                                .add_input(TxoRef::Absolute(debt_sid),
                                                           debt_open_asset_record,
                                                           None,
                                                           amount)?
                                                .add_output(&lender_template, credential_record)?
                                                .add_output(&borrower_template, None)?
                                                .create(TransferType::Standard)?
                                                .sign(lender_key_pair)?
                                                .sign(borrower_key_pair)?
                                                .transaction()?;
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation(xfr_op);
    store_txn_to_file(&debt_txn_file, &txn_builder)?;

    // Submit transaction
    let sids_new = submit_and_get_sids(protocol, host, txn_builder)?;

    // Merge records
    let fiat_sid_merged = if let Some(sid_pre) = borrower.fiat_utxo {
      // Get the original fiat record
      let res_pre = query(protocol,
                          host,
                          QUERY_PORT,
                          "utxo_sid",
                          &format!("{}", sid_pre.0))?;
      let blind_asset_record_pre =
        serde_json::from_str::<BlindAssetRecord>(&res_pre).or_else(|_| {
                                                            Err(PlatformError::DeserializationError)
                                                          })?;
      // Get the new fiat record
      let res_new = query(protocol,
                          host,
                          QUERY_PORT,
                          "utxo_sid",
                          &format!("{}", sids_new[1].0))?;
      let blind_asset_record_new =
        serde_json::from_str::<BlindAssetRecord>(&res_new).or_else(|_| {
                                                            Err(PlatformError::DeserializationError)
                                                          })?;
      let txn_builder = merge_records(borrower_key_pair,
                                      TxoRef::Absolute(sid_pre),
                                      TxoRef::Absolute(sids_new[1]),
                                      (blind_asset_record_pre, None),
                                      (blind_asset_record_new, None),
                                      fiat_code,
                                      None)?;
      store_txn_to_file(&fiat_txn_file, &txn_builder)?;
      submit_and_get_sids(protocol, host, txn_builder)?[0]
    } else {
      sids_new[1]
    };
    println!("New debt utxo sid: {}, fiat utxo sid: {}.",
             sids_new[0].0, fiat_sid_merged.0);

    // Update data
    let credential_str =
      serde_json::to_string(&ac_credential).or_else(|_| Err(PlatformError::SerializationError))?;
    let mut data = load_data()?;
    data.loans[loan_id as usize].issuer = Some(issuer_id);
    data.fiat_code = Some(fiat_code.to_base64());
    data.loans[loan_id as usize].status = LoanStatus::Active;
    data.loans[loan_id as usize].code = Some(debt_code.to_base64());
    data.loans[loan_id as usize].debt_utxo = Some(sids_new[0]);
    data.loans[loan_id as usize].debt_txn_file = Some(debt_txn_file);
    data.loans[loan_id as usize].credential = Some(credential_str);
    data.borrowers[borrower_id as usize].balance = borrower.balance + amount;
    data.borrowers[borrower_id as usize].fiat_utxo = Some(fiat_sid_merged);
    data.borrowers[borrower_id as usize].fiat_txn_file = Some(fiat_txn_file);
    store_data_to_file(data)
  }

  /// Pays loan.
  /// # Arguments
  /// * `loan_id`: loan ID.
  /// * `amount`: amount to pay.
  /// * `protocol`: either `https` or `http`.
  /// * `host`: either `testnet.findora.org` or `localhost`.
  pub(crate) fn pay_loan(loan_id: u64,
                         amount: u64,
                         protocol: &str,
                         host: &str)
                         -> Result<(), PlatformError> {
    // Get data
    let data = load_data()?;
    let loan = &data.loans[loan_id as usize];

    // Check if it's valid to pay
    match loan.status {
      LoanStatus::Requested => {
        println!("Loan {} hasn't been fulfilled yet. Use issuer fulfill_loan.",
                 loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      LoanStatus::Declined => {
        println!("Loan {} has been declined.", loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      LoanStatus::Complete => {
        println!("Loan {} has been paid off.", loan_id);
        return Err(PlatformError::InputsError(error_location!()));
      }
      _ => {}
    }

    let lender_id = loan.lender;
    let borrower_id = loan.borrower;
    let borrower = &data.borrowers[borrower_id as usize];
    let lender_key_pair = &data.get_lender_key_pair(lender_id)?;
    let borrower_key_pair = &data.get_borrower_key_pair(borrower_id)?;

    // Check if funds are sufficient
    if amount > borrower.balance {
      println!("Insufficient funds. Use --load_funds to load more funds.");
      return Err(PlatformError::InputsError(error_location!()));
    }

    // Check if the amount meets the minimum requirement, i.e., the fee
    let fee =
      ledger::policies::calculate_fee(loan.balance, Fraction::new(loan.interest_per_mille, 1000));
    if amount < fee {
      println!("Payment amount should be at least: {}", fee);
      return Err(PlatformError::InputsError(error_location!()));
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
    let fiat_sid = if let Some(sid) = borrower.fiat_utxo {
      sid
    } else {
      println!("Missing fiat utxo in the borrower record. Try --fulfill_loan.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let debt_sid = if let Some(sid) = loan.debt_utxo {
      sid
    } else {
      println!("Missing debt utxo in the loan record. Try --fulfill_loan.");
      return Err(PlatformError::InputsError(error_location!()));
    };

    // Get fiat and debt open asset records
    let fiat_open_asset_record =
      query_open_asset_record(protocol, host, fiat_sid, lender_key_pair, &None)?;
    let debt_open_asset_record =
      query_open_asset_record(protocol, host, debt_sid, borrower_key_pair, &None)?;

    // Get fiat and debt codes
    let fiat_code = if let Some(code) = data.clone().fiat_code {
      AssetTypeCode::new_from_base64(&code)?
    } else {
      println!("Missing fiat code. Try --active_loan.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let debt_code = if let Some(code) = &loan.code {
      AssetTypeCode::new_from_base64(&code)?
    } else {
      println!("Missing debt code in the loan record. Try --fulfill_loan.");
      return Err(PlatformError::InputsError(error_location!()));
    };

    println!("Fiat code: {}", serde_json::to_string(&fiat_code.val)?);
    println!("Debt code: {}", serde_json::to_string(&debt_code.val)?);

    // Get templates
    let spend_template =
      AssetRecordTemplate::with_no_asset_tracking(amount_to_spend,
                                                  fiat_code.val,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  lender_key_pair.get_pk());
    let burn_template =
      AssetRecordTemplate::with_no_asset_tracking(amount_to_burn,
                                                  debt_code.val,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  XfrPublicKey::zei_from_bytes(&[0; 32]));
    let lender_template =
      AssetRecordTemplate::with_no_asset_tracking(loan.balance - amount_to_burn,
                                                  debt_code.val,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  lender_key_pair.get_pk());
    let borrower_template =
      AssetRecordTemplate::with_no_asset_tracking(borrower.balance - amount_to_spend,
                                                  fiat_code.val,
                                                  AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                                                  borrower_key_pair.get_pk());
    let op = TransferOperationBuilder::new().add_input(TxoRef::Absolute(debt_sid),
                                                       debt_open_asset_record,
                                                       None,
                                                       amount_to_burn)?
                                            .add_input(TxoRef::Absolute(fiat_sid),
                                                       fiat_open_asset_record,
                                                       None,
                                                       amount_to_spend)?
                                            .add_output(&spend_template, None)?
                                            .add_output(&burn_template, None)?
                                            .add_output(&lender_template, None)?
                                            .add_output(&borrower_template, None)?
                                            .create(TransferType::DebtSwap)?
                                            .sign(borrower_key_pair)?
                                            .transaction()?;
    let mut txn_builder = TransactionBuilder::default();
    txn_builder.add_operation(op).transaction();

    // Submit transaction and update data
    let sids = submit_and_get_sids(protocol, host, txn_builder)?;

    let mut data = load_data()?;
    let balance = loan.balance - amount_to_burn;
    if balance == 0 {
      data.loans[loan_id as usize].status = LoanStatus::Complete;
    }
    data.loans[loan_id as usize].balance = balance;
    data.loans[loan_id as usize].payments = loan.payments + 1;
    data.loans[loan_id as usize].debt_utxo = Some(sids[2]);
    data.borrowers[borrower_id as usize].balance = borrower.balance - amount_to_spend;
    data.borrowers[borrower_id as usize].fiat_utxo = Some(sids[3]);

    store_data_to_file(data)
  }

  /// Uses environment variable RUST_LOG to select log level and filters output by module or regex.
  ///
  /// By default, log everything "trace" level or greater to stdout.
  ///
  /// # Examples
  /// RUST_LOG=ledger::data_model=info,main=trace/rec[ie]+ve ./main
  // TODO Verify that this comment is correct.
  // TODO switch to using from_default_env()
  pub fn init_logging() {
    env_logger::from_env(Env::default().default_filter_or("trace")).target(Target::Stdout)
                                                                   .init();
  }

  /// Matches the PlatformError with an exitcode and exits.
  /// * SerializationError: exits with code `DATAERR`.
  /// * DeserializationError: exits with code `DATAERR`.
  /// * IoError:
  ///   * If the input file doesn't exist: exits with code `NOINPUT`.
  ///     * Note: make sure the error message contains "File doesn't exist:" when conpub(crate) structing the PlatformError.
  ///   * If the input file isn't readable: exits with code `NOINPUT`.
  ///     * Note: make sure the error message contains "Failed to read" when conpub(crate) structing the PlatformError.
  ///   * If the output file or directory can't be created: exits with code `CANTCREAT`.
  ///     * Note: make sure the error message contains "Failed to create" when conpub(crate) structing the PlatformError.
  ///   * Otherwise: exits with code `IOERR`.
  /// * SubmissionServerError: exits with code `UNAVAILABLE`.
  /// * Otherwise: exits with code `USAGE`.
  pub fn match_error_and_exit(error: PlatformError) {
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

  /// Processes the `asset_issuer` subcommand.
  ///
  /// Subcommands under `asset_issuer`
  /// * `sign_up`
  /// * `store_sids`
  /// * `define_asset`
  /// * `issue_asset`
  /// * `transfer_asset`
  /// * `issue_and_transfer_asset`
  ///
  /// # Arguments
  /// * `asset_issuer_matches`: subcommands and arguments under the `asset_issuer` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_asset_issuer_cmd(asset_issuer_matches: &clap::ArgMatches,
                                         txn_file: &str)
                                         -> Result<(), PlatformError> {
    match asset_issuer_matches.subcommand() {
      ("sign_up", Some(sign_up_matches)) => {
        let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
          name_arg.to_owned()
        } else {
          println!("Name is required to sign up an asset issuer account. Use --name.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let mut data = load_data()?;
        data.add_asset_issuer(name)
      }
      ("store_sids", Some(store_sids_matches)) => {
        let file = if let Some(file_arg) = store_sids_matches.value_of("file") {
          file_arg
        } else {
          println!("Path is required to store the sids. Use --path.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let sids = if let Some(indices_arg) = store_sids_matches.value_of("indices") {
          indices_arg
        } else {
          println!("Indices are required to store the sids. Use --indices.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        store_sids_to_file(file, sids)
      }
      ("store_memos", Some(store_bar_and_memos_matches)) => {
        let data = load_data()?;
        let (issuer_pub_key, policy) = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          let issuer_id = parse_to_u64(id_arg)?;
          let issuer_pub_key = data.get_asset_issuer_key_pair(issuer_id)?.get_pk();
          let tracer_enc_keys = data.get_asset_tracer_key_pair(issuer_id)?.enc_key;
          let policy = AssetTracingPolicy { enc_keys: tracer_enc_keys,
                                            asset_tracking: true,
                                            identity_tracking: None };
          (issuer_pub_key, policy)
        } else {
          println!("Asset issuer id is required to store the tracer and owner memos. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let amount = if let Some(amount_arg) = store_bar_and_memos_matches.value_of("amount") {
          parse_to_u64(amount_arg)?
        } else {
          println!("Asset amount is required to store the tracer and owner memos. Use --amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let confidential_amount = store_bar_and_memos_matches.is_present("confidential_amount");
        let record_type = AssetRecordType::from_booleans(confidential_amount, false);
        let token_code = if let Some(token_code) =
          store_bar_and_memos_matches.value_of("token_code")
        {
          AssetTypeCode::new_from_base64(token_code)?
        } else {
          println!("Asset token code is required to store the tracer and owner memos. Use --token_code.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let file = if let Some(file_arg) = store_bar_and_memos_matches.value_of("file") {
          file_arg
        } else {
          println!("Path is required to store the tracer and owner memos. Use --path.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        get_and_store_memos_to_file(file,
                                    issuer_pub_key,
                                    amount,
                                    token_code,
                                    record_type,
                                    Some(policy))
      }
      ("air_assign", Some(air_assign_matches)) => {
        let issuer_id = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Asset issuer id is required for AIR assigning. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        match (air_assign_matches.value_of("address"),
               air_assign_matches.value_of("data"),
               air_assign_matches.value_of("issuer_pk"),
               air_assign_matches.value_of("pok"))
        {
          (Some(address), Some(data), Some(issuer_pk), Some(pok)) => {
            air_assign(issuer_id, address, data, issuer_pk, pok, txn_file)
          }
          (_, _, _, _) => {
            println!("Missing address, data, issuer_pk, or proof.");
            Err(PlatformError::InputsError(error_location!()))
          }
        }
      }
      ("define_asset", Some(define_asset_matches)) => {
        let fiat_asset = define_asset_matches.is_present("fiat");
        let data = load_data()?;
        let issuer_key_pair = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          let issuer_id = parse_to_u64(id_arg)?;
          data.get_asset_issuer_key_pair(issuer_id)?
        } else {
          println!("Asset issuer id is required to define an asset. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let token_code = define_asset_matches.value_of("token_code");
        let memo = if let Some(memo) = define_asset_matches.value_of("memo") {
          memo
        } else {
          "{}"
        };
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
        match define_asset(fiat_asset,
                           &issuer_key_pair,
                           asset_token,
                           &memo,
                           AssetRules::default().set_traceable(traceable).clone(),
                           txn_file)
        {
          Ok(_) => Ok(()),
          Err(error) => Err(error),
        }
      }
      ("issue_asset", Some(issue_asset_matches)) => {
        let data = load_data()?;
        let (key_pair, tracer_enc_keys) = if let Some(id_arg) = asset_issuer_matches.value_of("id")
        {
          let issuer_id = parse_to_u64(id_arg)?;
          (data.get_asset_issuer_key_pair(issuer_id)?,
           data.get_asset_tracer_key_pair(issuer_id)?.enc_key)
        } else {
          println!("Asset issuer id is required to issue asset. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let token_code = if let Some(token_code_arg) = issue_asset_matches.value_of("token_code") {
          AssetTypeCode::new_from_base64(token_code_arg)?
        } else {
          println!("Token code is required to issue asset. Use --token_code.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let amount = if let Some(amount_arg) = issue_asset_matches.value_of("amount") {
          parse_to_u64(amount_arg)?
        } else {
          println!("Amount is required to issue asset. Use --amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let confidential_amount = issue_asset_matches.is_present("confidential_amount");
        let mut txn_builder = TransactionBuilder::default();
        let policy = if issue_asset_matches.is_present("traceable") {
          Some(AssetTracingPolicy { enc_keys: tracer_enc_keys,
                                    asset_tracking: true,
                                    identity_tracking: None })
        } else {
          None
        };
        if let Err(e) =
          txn_builder.add_basic_issue_asset(&key_pair,
                                            policy,
                                            &token_code,
                                            get_and_update_sequence_number()?,
                                            amount,
                                            AssetRecordType::from_booleans(confidential_amount,
                                                                           false))
        {
          println!("Failed to add basic issue asset.");
          return Err(e);
        }
        store_txn_to_file(&txn_file, &txn_builder)
      }
      ("transfer_asset", Some(transfer_asset_matches)) => {
        let data = load_data()?;
        let (issuer_key_pair, tracer_enc_keys) =
          if let Some(id_arg) = asset_issuer_matches.value_of("id") {
            let issuer_id = parse_to_u64(id_arg)?;
            (data.get_asset_issuer_key_pair(issuer_id)?,
             data.get_asset_tracer_key_pair(issuer_id)?.enc_key)
          } else {
            println!("Asset issuer id is required to transfer asset. Use asset_issuer --id.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        // Compose transfer_from for add_basic_transfer_asset
        let mut txo_refs = Vec::new();
        if let Some(sids_file_arg) = transfer_asset_matches.value_of("sids_file") {
          for sid in load_sids_from_file(sids_file_arg)? {
            txo_refs.push(TxoRef::Absolute(TxoSID(sid)));
          }
        } else {
          println!("Sids are required to transfer asset. Use --sids_file.");
          return Err(PlatformError::InputsError(error_location!()));
        }
        let bars_and_owner_memos = if let Some(issuance_txn_files_arg) =
          transfer_asset_matches.value_of("issuance_txn_files")
        {
          load_blind_asset_records_and_owner_memos_from_files(issuance_txn_files_arg)?
        } else {
          println!("Blind asset records and associated memos are required to transfer asset. Use --issuance_txn_files.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let input_amounts =
          if let Some(input_amounts_arg) = transfer_asset_matches.value_of("input_amounts") {
            parse_to_u64_vec(input_amounts_arg)?
          } else {
            println!("Input amounts are required to transfer asset. Use --input_amounts.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let mut count = txo_refs.len();
        if input_amounts.len() != count || bars_and_owner_memos.len() != count {
          println!("Size of input sids and input amounts should match.");
          return Err(PlatformError::InputsError(error_location!()));
        }
        let mut transfer_from = Vec::new();
        let mut txo_refs_iter = txo_refs.iter();
        let mut bars_and_owner_memos_iter = bars_and_owner_memos.iter();
        let mut input_amounts_iter = input_amounts.iter();
        while count > 0 {
          let txo_refs_next = if let Some(txo_ref) = txo_refs_iter.next() {
            txo_ref
          } else {
            println!("More txo ref expected.");
            return Err(PlatformError::InputsError(error_location!()));
          };
          let (blind_asset_record_next, owner_memo_next) =
            if let Some(bar_and_owner_memo) = bars_and_owner_memos_iter.next() {
              bar_and_owner_memo
            } else {
              println!("More blind asset record and owner memo expected.");
              return Err(PlatformError::InputsError(error_location!()));
            };
          let input_amount_next = if let Some(input_amount) = input_amounts_iter.next() {
            *input_amount
          } else {
            println!("More input amount expected.");
            return Err(PlatformError::InputsError(error_location!()));
          };
          let transfer_from_next =
            (txo_refs_next, blind_asset_record_next, input_amount_next, owner_memo_next);
          transfer_from.push(transfer_from_next);
          count -= 1;
        }

        // Compose transfer_to for add_basic_transfer_asset
        let mut recipient_addresses = Vec::new();
        if let Some(recipients) = transfer_asset_matches.value_of("recipients") {
          let recipient_ids = parse_to_u64_vec(recipients)?;
          for id in recipient_ids {
            let recipient_pub_key = data.get_borrower_key_pair(id)?.get_pk();
            recipient_addresses.push(AccountAddress { key: recipient_pub_key });
          }
        } else {
          println!("Recipient ids are required to transfer asset. Use --recipients.");
          return Err(PlatformError::InputsError(error_location!()));
        }
        let output_amounts =
          if let Some(output_amounts_arg) = transfer_asset_matches.value_of("output_amounts") {
            parse_to_u64_vec(output_amounts_arg)?
          } else {
            println!("Output amounts are required to transfer asset. Use --output_amounts.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let mut count = output_amounts.len();
        if recipient_addresses.len() != count {
          println!("Size of output amounts and addresses should match.");
          return Err(PlatformError::InputsError(error_location!()));
        }
        let mut transfer_to = Vec::new();
        let mut output_amounts_iter = output_amounts.iter();
        let mut addresses_iter = recipient_addresses.iter();
        while count > 0 {
          let output_amount_next = if let Some(output_amount) = output_amounts_iter.next() {
            *output_amount
          } else {
            println!("More output amount expected.");
            return Err(PlatformError::InputsError(error_location!()));
          };
          let address_next = if let Some(address) = addresses_iter.next() {
            address
          } else {
            println!("More address expected.");
            return Err(PlatformError::InputsError(error_location!()));
          };
          transfer_to.push((output_amount_next, address_next));
          count -= 1;
        }

        // Transfer asset
        let mut txn_builder = TransactionBuilder::default();
        if let Err(e) =
          txn_builder.add_basic_transfer_asset(&issuer_key_pair,
                                               &Some(AssetTracingPolicy { enc_keys:
                                                                            tracer_enc_keys,
                                                                          asset_tracking: true,
                                                                          identity_tracking:
                                                                            None }),
                                               &transfer_from[..],
                                               &transfer_to[..])
        {
          println!("Failed to add operation to transaction.");
          return Err(e);
        };
        store_txn_to_file(&txn_file, &txn_builder)
      }
      ("issue_and_transfer_asset", Some(issue_and_transfer_matches)) => {
        let data = load_data()?;
        let issuer_key_pair = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          let issuer_id = parse_to_u64(id_arg)?;
          data.get_asset_issuer_key_pair(issuer_id)?
        } else {
          println!("Asset issuer id is required to issue and transfer asset. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let recipient_key_pair =
          if let Some(id_arg) = issue_and_transfer_matches.value_of("recipient") {
            let recipient_id = parse_to_u64(id_arg)?;
            data.get_borrower_key_pair(recipient_id)?
          } else {
            println!("Recipient id is required to issue and transfer asset. Use --recipient.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let amount = if let Some(amount_arg) = issue_and_transfer_matches.value_of("amount") {
          parse_to_u64(amount_arg)?
        } else {
          println!("Amount is required to issue and transfer asset. Use --amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let token_code =
          if let Some(token_code_arg) = issue_and_transfer_matches.value_of("token_code") {
            AssetTypeCode::new_from_base64(token_code_arg)?
          } else {
            println!("Token code is required to issue asset. Use --token_code.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let confidential_amount = issue_and_transfer_matches.is_present("confidential_amount");
        let record_type = AssetRecordType::from_booleans(confidential_amount, false);

        issue_and_transfer_asset(&issuer_key_pair,
                                 &recipient_key_pair,
                                 amount,
                                 token_code,
                                 record_type,
                                 None,
                                 txn_file,
                                 None)?;
        Ok(())
      }
      ("trace_and_verify_asset", Some(trace_and_verify_asset_matches)) => {
        let data = load_data()?;
        let tracer_dec_keys = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          let issuer_id = parse_to_u64(id_arg)?;
          data.get_asset_tracer_key_pair(issuer_id)?
              .dec_key
              .record_data_dec_key
        } else {
          println!("Asset issuer id is required to trace the asset. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let tracer_and_owner_memos =
          if let Some(memo_file_arg) = trace_and_verify_asset_matches.value_of("memo_file") {
            load_tracer_and_owner_memos_from_files(memo_file_arg)?
          } else {
            println!("Owner memo is required to trace the asset. Use --memo_file.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let tracer_memo = if let Some(memo) = tracer_and_owner_memos[0].clone().0 {
          memo
        } else {
          println!("The asset isn't traceable.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let expected_amount = if let Some(expected_amount_arg) =
          trace_and_verify_asset_matches.value_of("expected_amount")
        {
          parse_to_u64(expected_amount_arg)?
        } else {
          println!("Expected amount is required to verify the asset. Use --expected_amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        tracer_memo.verify_amount(&tracer_dec_keys, expected_amount)
                   .or_else(|error| Err(PlatformError::ZeiError(error_location!(), error)))
      }
      ("trace_credential", Some(trace_credential_matches)) => {
        let data = load_data()?;
        let attrs_dec_key = if let Some(id_arg) = asset_issuer_matches.value_of("id") {
          let issuer_id = parse_to_u64(id_arg)?;
          let asset_tracer_key_pair = data.get_asset_tracer_key_pair(issuer_id)?;
          asset_tracer_key_pair.dec_key.attrs_dec_key
        } else {
          println!("Asset issuer id is required to trace the asset. Use asset_issuer --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let tracer_memo =
          if let Some(memo_file_arg) = trace_credential_matches.value_of("memo_file") {
            load_tracer_memo_from_file(memo_file_arg)?
          } else {
            println!("Tracer memo is required to trace the credential. Use --memo_file.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        let len = if let Some(attribute_arg) = trace_credential_matches.value_of("attribute") {
          let credential_issuer_public_key = data.get_credential_issuer_key_pair(0)?.0;
          credential_issuer_public_key.get_len(attribute_arg)
                                      .or_else(|e| {
                                        Err(PlatformError::ZeiError(error_location!(), e))
                                      })?
        } else {
          println!("Credential attribute is required to verify the credential. Use --attribute.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let expected_value =
          if let Some(expected_value_arg) = trace_credential_matches.value_of("expected_value") {
            u8_slice_to_u32_vec(expected_value_arg.as_bytes(), len)
          } else {
            println!("Expected value is required to verify the credential. Use --expected_value.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        match tracer_memo.verify_identity_attributes(&attrs_dec_key, &expected_value) {
          Ok(res) => {
            if res[0] {
              println!("Credential verification succeeded.");
            } else {
              println!("Credential value isn't as expected.");
              return Err(PlatformError::InputsError(error_location!()));
            }
            Ok(())
          }
          Err(e) => Err(PlatformError::ZeiError(error_location!(), e)),
        }
      }
      _ => {
        println!("Subcommand missing or not recognized. Try asset_issuer --help");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Sets the protocol and host.
  ///
  /// Environment variables `PROTOCOL` and `SERVER_HOST` set the protocol and host,
  /// which can be overwritten by CLI subcommands.
  ///
  /// By default, the protocol is `https` and the host is `testnet.findora.org`.
  pub(crate) fn protocol_host(matches: &clap::ArgMatches) -> (&'static str, &'static str) {
    let protocol = if matches.is_present("http") {
      "http"
    } else {
      std::option_env!("PROTOCOL").unwrap_or("https")
    };
    let host = if matches.is_present("localhost") {
      // Use localhost
      "localhost"
    } else {
      // Default to testnet.findora.org
      std::option_env!("SERVER_HOST").unwrap_or("testnet.findora.org")
    };
    (protocol, host)
  }

  /// Processes the `credential_issuer` subcommand.
  ///
  /// Subcommands under `credential_issuer`
  /// * `sign_up`
  ///
  /// # Arguments
  /// * `credential_issuer_matches`: subcommands and arguments under the `credential_issuer` subcommand.
  pub(crate) fn process_credential_issuer_cmd(credential_issuer_matches: &clap::ArgMatches)
                                              -> Result<(), PlatformError> {
    match credential_issuer_matches.subcommand() {
      ("sign_up", Some(sign_up_matches)) => {
        let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
          name_arg.to_owned()
        } else {
          println!("Name is required to sign up a credential issuer account. Use --name.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let mut data = load_data()?;
        data.add_credential_issuer(name)
      }
      _ => {
        println!("Subcommand missing or not recognized. Try credential_issuer --help");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Processes the `lender` subcommand.
  ///
  /// Subcommands under `lender`
  /// * `sign_up`
  /// * `view_loan`
  /// * `fulfill_loan`
  ///
  /// # Arguments
  /// * `lender_matches`: subcommands and arguments under the `lender` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_lender_cmd(lender_matches: &clap::ArgMatches,
                                   txn_file: &str)
                                   -> Result<(), PlatformError> {
    let mut data = load_data()?;
    match lender_matches.subcommand() {
      ("sign_up", Some(sign_up_matches)) => {
        let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
          name_arg.to_owned()
        } else {
          println!("Name is required to sign up a lender account. Use --name.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        data.add_lender(name)
      }
      ("view_loan", Some(view_loan_matches)) => {
        let lender_id = if let Some(id_arg) = lender_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Lender id is required to get loan information. Use lender --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        if let Some(loan_arg) = view_loan_matches.value_of("loan") {
          let loan_id = parse_to_u64(loan_arg)?;
          let loan = data.loans[loan_id as usize].clone();
          if loan.lender != lender_id {
            println!("Lender {} doesn't own loan {}.", lender_id, loan_id);
            return Err(PlatformError::InputsError(error_location!()));
          }
          println!("Displaying loan {}: {:?}.", loan_id, loan);
          return Ok(());
        }
        let mut loans = Vec::new();
        let loan_ids = data.lenders[lender_id as usize].loans.clone();
        if let Some(filter) = view_loan_matches.value_of("filter") {
          for id in loan_ids {
            match filter {
              "requested" => {
                if data.loans[id as usize].status == LoanStatus::Requested {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "fulfilled" => {
                if data.loans[id as usize].status == LoanStatus::Active
                   || data.loans[id as usize].status == LoanStatus::Complete
                {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "declined" => {
                if data.loans[id as usize].status == LoanStatus::Declined {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "active" => {
                if data.loans[id as usize].status == LoanStatus::Active {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "complete" => {
                if data.loans[id as usize].status == LoanStatus::Complete {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              _ => {
                loans.push(data.loans[id as usize].clone());
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
      ("fulfill_loan", Some(fulfill_loan_matches)) => {
        let loan_id = if let Some(loan_arg) = fulfill_loan_matches.value_of("loan") {
          parse_to_u64(loan_arg)?
        } else {
          println!("Loan id is required to fulfill the loan. Use --loan.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        if let Some(id_arg) = lender_matches.value_of("id") {
          let lender_id = parse_to_u64(id_arg)?;
          let loan = data.loans[loan_id as usize].clone();
          if loan.lender != lender_id {
            println!("Lender {} doesn't own loan {}.", lender_id, loan_id);
            return Err(PlatformError::InputsError(error_location!()));
          }
        } else {
          println!("Lender id is required to fulfill a loan. Use lender --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let issuer_id = if let Some(issuer_arg) = fulfill_loan_matches.value_of("issuer") {
          parse_to_u64(issuer_arg)?
        } else {
          println!("Asset issuer id is required to fulfill the loan. Use --issuer.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let memo_file = fulfill_loan_matches.value_of("memo_file");
        let (protocol, host) = protocol_host(fulfill_loan_matches);
        fulfill_loan(loan_id, issuer_id, txn_file, memo_file, protocol, host)
      }
      ("create_or_overwrite_requirement", Some(create_or_overwrite_requirement_matches)) => {
        let lender_id = if let Some(id_arg) = lender_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Lender id is required to get credential requirement information. Use lender --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let attribute = if let Some(attribute_arg) =
          create_or_overwrite_requirement_matches.value_of("attribute")
        {
          match attribute_arg {
            "min_credit_score" => CredentialIndex::MinCreditScore,
            "min_income" => CredentialIndex::MinIncome,
            _ => CredentialIndex::Citizenship,
          }
        } else {
          println!("Credential attribute is required to create or overwrite the credential requirement. Use --attribute.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let requirement = if let Some(requirement_arg) =
          create_or_overwrite_requirement_matches.value_of("requirement")
        {
          requirement_arg
        } else {
          println!("Credential value is required to create or overwrite the credential requirement. Use --requirement.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let mut data = load_data()?;
        data.create_or_overwrite_requirement(lender_id, attribute, requirement)
      }
      _ => {
        println!("Subcommand missing or not recognized. Try lender --help");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Processes the `borrower` subcommand.
  ///
  /// Subcommands under `borrower`
  /// * `sign_up`
  /// * `load_funds`
  /// * `view_loan`
  /// * `request_loan`
  /// * `pay_loan`
  /// * `view_credential`
  /// * `create_or_overwrite_credential`
  /// * `get_asset_record`
  ///
  /// # Arguments
  /// * `borrower_matches`: subcommands and arguments under the `borrower` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_borrower_cmd(borrower_matches: &clap::ArgMatches,
                                     txn_file: &str)
                                     -> Result<(), PlatformError> {
    let mut data = load_data()?;
    match borrower_matches.subcommand() {
      ("sign_up", Some(sign_up_matches)) => {
        let name = if let Some(name_arg) = sign_up_matches.value_of("name") {
          name_arg.to_owned()
        } else {
          println!("Name is required to sign up a lender account. Use --name.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        data.add_borrower(name)
      }
      ("load_funds", Some(load_funds_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to load funds. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        process_load_funds_cmd(borrower_id, load_funds_matches, txn_file)
      }
      ("view_loan", Some(view_loan_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to get loan information. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        if let Some(loan_arg) = view_loan_matches.value_of("loan") {
          let loan_id = parse_to_u64(loan_arg)?;
          let loan = data.loans[loan_id as usize].clone();
          if loan.borrower != borrower_id {
            println!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
            return Err(PlatformError::InputsError(error_location!()));
          }
          println!("Displaying loan {}: {:?}.", loan_id, loan);
          return Ok(());
        }
        let mut loans = Vec::new();
        let loan_ids = data.borrowers[borrower_id as usize].loans.clone();
        if let Some(filter) = view_loan_matches.value_of("filter") {
          for id in loan_ids {
            match filter {
              "requested" => {
                if data.loans[id as usize].status == LoanStatus::Requested {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "fulfilled" => {
                if data.loans[id as usize].status == LoanStatus::Active
                   || data.loans[id as usize].status == LoanStatus::Complete
                {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "declined" => {
                if data.loans[id as usize].status == LoanStatus::Declined {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "active" => {
                if data.loans[id as usize].status == LoanStatus::Active {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              "complete" => {
                if data.loans[id as usize].status == LoanStatus::Complete {
                  loans.push(data.loans[id as usize].clone());
                }
              }
              _ => {
                loans.push(data.loans[id as usize].clone());
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
      ("request_loan", Some(request_loan_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to request a loan. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let lender_id = if let Some(lender_arg) = request_loan_matches.value_of("lender") {
          parse_to_u64(lender_arg)?
        } else {
          println!("Lender id is required to request the loan. Use --lender.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let amount = if let Some(amount_arg) = request_loan_matches.value_of("amount") {
          parse_to_u64(amount_arg)?
        } else {
          println!("Amount is required to request the loan. Use --amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let interest_per_mille = if let Some(interest_per_mille_arg) =
          request_loan_matches.value_of("interest_per_mille")
        {
          parse_to_u64(interest_per_mille_arg)?
        } else {
          println!("Interest per mille is required to request the loan. Use --interest_per_mille.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let duration = if let Some(duration_arg) = request_loan_matches.value_of("duration") {
          parse_to_u64(duration_arg)?
        } else {
          println!("Duration is required to request the loan. Use --amount.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let mut data = load_data()?;
        data.add_loan(lender_id, borrower_id, amount, interest_per_mille, duration)
      }
      ("pay_loan", Some(pay_loan_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to pay off the loan. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
          let loan_id = parse_to_u64(loan_arg)?;
          let loan = data.loans[loan_id as usize].clone();
          if loan.borrower != borrower_id {
            println!("Borrower {} doesn't own loan {}.", borrower_id, loan_id);
            return Err(PlatformError::InputsError(error_location!()));
          }
        } else {
          println!("Loan id is required to pay the loan.");
          return Err(PlatformError::InputsError(error_location!()));
        }
        process_pay_loan_cmd(pay_loan_matches)
      }
      ("view_credential", Some(view_credential_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to get credential information. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let credential_id = if let Some(id) = data.borrowers[borrower_id as usize].credentials {
          id
        } else {
          println!("No credential is found. Use create_or_overwrite_credential to create a credential record.");
          return Ok(());
        };
        if let Some(attribute_arg) = view_credential_matches.value_of("attribute") {
          let attribute = match attribute_arg {
            "min_credit_score" => CredentialIndex::MinCreditScore,
            "min_income" => CredentialIndex::MinIncome,
            _ => CredentialIndex::Citizenship,
          };
          let value = data.credentials[credential_id as usize].values[attribute as usize].clone();
          println!("Displaying {:?}: {:?}", attribute.get_name(), value);
        } else {
          println!("Displaying credentials:");
          let values = data.credentials[credential_id as usize].values.clone();
          for attribute in [CredentialIndex::MinCreditScore,
                            CredentialIndex::MinIncome,
                            CredentialIndex::Citizenship].iter()
          {
            if let Some(value) = values[*attribute as usize].clone() {
              println!("{}: {}.", attribute.get_name(), value);
            }
          }
        };
        Ok(())
      }
      ("create_or_overwrite_credential", Some(create_or_overwrite_credential_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to get credential information. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let credential_issuer_id = if let Some(credential_issuer_arg) =
          create_or_overwrite_credential_matches.value_of("credential_issuer")
        {
          parse_to_u64(credential_issuer_arg)?
        } else {
          println!("Credential issuer id is required to get credential information. Use --credential_issuer.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let attribute = if let Some(attribute_arg) =
          create_or_overwrite_credential_matches.value_of("attribute")
        {
          match attribute_arg {
            "min_credit_score" => CredentialIndex::MinCreditScore,
            "min_income" => CredentialIndex::MinIncome,
            _ => CredentialIndex::Citizenship,
          }
        } else {
          println!("Credential attribute is required to create or overwrite the credential. Use --attribute.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let value = if let Some(value_arg) =
          create_or_overwrite_credential_matches.value_of("value")
        {
          value_arg
        } else {
          println!("Credential value is required to create or overwrite the credential. Use --value.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let mut data = load_data()?;
        data.create_or_overwrite_credential(borrower_id, credential_issuer_id, attribute, value)
      }
      ("get_asset_record", Some(get_asset_record_matches)) => {
        let borrower_id = if let Some(id_arg) = borrower_matches.value_of("id") {
          parse_to_u64(id_arg)?
        } else {
          println!("Borrower id is required to get the asset record. Use borrower --id.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let data = load_data()?;
        let borrower_name = data.borrowers[borrower_id as usize].name.clone();
        let key_pair = data.get_borrower_key_pair(borrower_id)?;
        let sid = if let Some(sid_arg) = get_asset_record_matches.value_of("sid") {
          TxoSID(parse_to_u64(sid_arg)?)
        } else {
          println!("Sid is required to get the asset record. Use borrower --sid.");
          return Err(PlatformError::InputsError(error_location!()));
        };
        let tracer_and_owner_memos =
          if let Some(memo_file_arg) = get_asset_record_matches.value_of("memo_file") {
            load_tracer_and_owner_memos_from_files(memo_file_arg)?
          } else {
            println!("Owner memo is required to get the asset record. Use --memo_file.");
            return Err(PlatformError::InputsError(error_location!()));
          };
        // Get protocol and host.
        let (protocol, host) = protocol_host(get_asset_record_matches);
        let asset_record =
          query_open_asset_record(protocol, host, sid, &key_pair, &tracer_and_owner_memos[0].1)?;
        println!("{} owns {} of asset {:?}.",
                 borrower_name,
                 asset_record.get_amount(),
                 asset_record.get_asset_type());
        Ok(())
      }
      _ => {
        println!("Subcommand missing or not recognized. Try borrower --help");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  /// Creates the directory for the file, and renames the file with the same path if it exists.
  /// # Arguments
  /// * `path_str`: string representation of the file path.
  /// * `overwrite`: whether to overwrite or find the available path if the file exists.
  pub(crate) fn create_directory_and_rename_path(path_str: &str,
                                                 overwrite: bool)
                                                 -> Result<(), PlatformError> {
    let path = Path::new(&path_str);
    create_directory_if_missing(&path_str)?;
    if path.exists() && !overwrite {
      rename_existing_path(&path)?;
    }
    Ok(())
  }

  /// Processes the `create_txn_builder` subcommand.
  /// # Arguments
  /// * `create_matches`: subcommands and arguments under the `create_txn_builder` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_create_txn_builder_cmd(create_matches: &clap::ArgMatches,
                                               txn_file: &str)
                                               -> Result<(), PlatformError> {
    let name = create_matches.value_of("name");
    let overwrite = create_matches.is_present("overwrite");
    let file_str = if let Some(name) = name {
      name.to_string()
    } else {
      txn_file.to_string()
    };
    let expand_str = shellexpand::tilde(&file_str).to_string();
    create_directory_and_rename_path(&expand_str, overwrite)?;
    let txn_builder = TransactionBuilder::default();
    store_txn_to_file(&expand_str, &txn_builder)
  }

  /// Processes the `submit` subcommand.
  /// # Arguments
  /// * `submit_matches`: subcommands and arguments under the `submit` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_submit_cmd(submit_matches: &clap::ArgMatches,
                                   txn_file: &str)
                                   -> Result<(), PlatformError> {
    let (protocol, host) = protocol_host(submit_matches);
    let txn_builder = load_txn_from_file(txn_file)?;
    if submit_matches.is_present("get_sids") || submit_matches.is_present("sids_file") {
      let sids = submit_and_get_sids(protocol, host, txn_builder)?;
      println!("Utxo: {:?}", sids);
      if let Some(path) = submit_matches.value_of("sids_file") {
        let mut sids_str = "".to_owned();
        for sid in sids {
          sids_str.push_str(&format!("{},", sid.0));
        }
        store_sids_to_file(path, &sids_str)?;
      }
      Ok(())
    } else {
      submit(protocol, host, txn_builder)
    }
  }

  /// Processes the `borrower load_funds` subcommand.
  /// # Arguments
  /// * `borrower_id`: borrower ID.
  /// * `load_funds_matches`: subcommands and arguments under the `load_funds` subcommand.
  /// * `txn_file`: path to store the transaction file.
  pub(crate) fn process_load_funds_cmd(borrower_id: u64,
                                       load_funds_matches: &clap::ArgMatches,
                                       txn_file: &str)
                                       -> Result<(), PlatformError> {
    let issuer_id = if let Some(issuer_arg) = load_funds_matches.value_of("issuer") {
      if let Ok(id) = issuer_arg.parse::<u64>() {
        id
      } else {
        println!("Improperly formatted issuer id.");
        return Err(PlatformError::InputsError(error_location!()));
      }
    } else {
      println!("Asset issuer id is required to load funds. Use --issuer.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let amount = if let Some(amount_arg) = load_funds_matches.value_of("amount") {
      parse_to_u64(amount_arg)?
    } else {
      println!("Amount is required to load funds. Use --amount.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let (protocol, host) = protocol_host(load_funds_matches);
    load_funds(issuer_id, borrower_id, amount, txn_file, protocol, host)
  }

  /// Processes the `borrower pay_loan` subcommand.
  /// # Arguments
  /// * `pay_loan_matches`: subcommands and arguments under the `pay_loan` subcommand.
  pub(crate) fn process_pay_loan_cmd(pay_loan_matches: &clap::ArgMatches)
                                     -> Result<(), PlatformError> {
    let loan_id = if let Some(loan_arg) = pay_loan_matches.value_of("loan") {
      parse_to_u64(loan_arg)?
    } else {
      println!("Loan id is required to pay the loan. Use --loan.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let amount = if let Some(amount_arg) = pay_loan_matches.value_of("amount") {
      parse_to_u64(amount_arg)?
    } else {
      println!("Amount is required to pay the loan. Use --amount.");
      return Err(PlatformError::InputsError(error_location!()));
    };
    let (protocol, host) = protocol_host(pay_loan_matches);

    pay_loan(loan_id, amount, protocol, host)
  }

  /// Processes input commands and arguments.
  /// # Arguments
  /// * `inputs`: input subcommands and arguments.
  pub fn process_inputs(inputs: clap::ArgMatches) -> Result<(), PlatformError> {
    let _config_file_path: String;
    let txn_file: String;
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
      txn_file = txn_store.to_string();
    } else {
      txn_file = format!("{}/txn/default.txn", findora_dir);
    }

    match inputs.subcommand() {
      ("asset_issuer", Some(asset_issuer_matches)) => {
        process_asset_issuer_cmd(asset_issuer_matches, &txn_file)
      }
      ("credential_issuer", Some(credential_issuer_matches)) => {
        process_credential_issuer_cmd(credential_issuer_matches)
      }
      ("lender", Some(issuer_matches)) => process_lender_cmd(issuer_matches, &txn_file),
      ("borrower", Some(issuer_matches)) => process_borrower_cmd(issuer_matches, &txn_file),
      ("create_txn_builder", Some(create_txn_builder_matches)) => {
        process_create_txn_builder_cmd(create_txn_builder_matches, &txn_file)
      }
      ("serialize", Some(_serialize_matches)) => {
        let txn_builder = load_txn_from_file(&txn_file).or_else(|e| {
                            println!("Failed to load txn builder from file {}.", txn_file);
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
      ("drop", Some(_drop_matches)) => match std::fs::remove_file(&txn_file) {
        Ok(_) => {
          println!("Deleted transaction file {}", txn_file);
          Ok(())
        }
        Err(e) => Err(PlatformError::IoError(format!("Error deleting file: {:?} ", e))),
      },
      ("submit", Some(submit_matches)) => process_submit_cmd(submit_matches, &txn_file),
      _ => {
        println!("Subcommand missing or not recognized. Try --help");
        Err(PlatformError::InputsError(error_location!()))
      }
    }
  }

  #[cfg(test)]
  mod tests {
    use super::*;
    use ledger_standalone::LedgerStandalone;

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

    #[test]
    fn test_define_asset() {
      // Create txn builder and key pair
      let txn_builder_path = "tb_define";
      let mut prng: ChaChaRng = ChaChaRng::from_entropy();
      let issuer_key_pair = XfrKeyPair::generate(&mut prng);

      // Define asset
      let res = define_asset(false,
                             &issuer_key_pair,
                             AssetTypeCode::gen_random(),
                             "Define asset",
                             AssetRules::default(),
                             txn_builder_path);

      let _ = fs::remove_file(DATA_FILE);
      fs::remove_file(txn_builder_path).unwrap();

      assert!(res.is_ok());
    }

    #[test]
    fn test_issue_and_transfer_asset() {
      // Create txn builder and key pairs
      let txn_builder_path = "tb_issue_and_transfer";
      let mut prng: ChaChaRng = ChaChaRng::from_entropy();
      let issuer_key_pair = XfrKeyPair::generate(&mut prng);
      let recipient_key_pair = XfrKeyPair::generate(&mut prng);

      // Issue and transfer asset
      let code = AssetTypeCode::gen_random();
      let amount = 1000;
      assert!(issue_and_transfer_asset(&issuer_key_pair,
                                     &recipient_key_pair,
                                     amount,
                                     code,
                                     AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType, None,
                                     txn_builder_path,
                                     None).is_ok());

      let _ = fs::remove_file(DATA_FILE);
      fs::remove_file(txn_builder_path).unwrap();
    }

    #[test]
    fn test_merge_records() {
      // Create key pair
      let mut prng: ChaChaRng = ChaChaRng::from_entropy();
      let key_pair = XfrKeyPair::generate(&mut prng);

      // Build blind asset records
      let code = AssetTypeCode::gen_random();
      let asset_record_type = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;
      let (bar1, _, memo1) = get_blind_asset_record_and_memos(key_pair.get_pk(),
                                                              1000,
                                                              code,
                                                              asset_record_type,
                                                              None).unwrap();
      let (bar2, _, memo2) = get_blind_asset_record_and_memos(key_pair.get_pk(),
                                                              500,
                                                              code,
                                                              asset_record_type,
                                                              None).unwrap();

      // Merge records
      assert!(merge_records(&key_pair,
                            TxoRef::Absolute(TxoSID(1)),
                            TxoRef::Absolute(TxoSID(2)),
                            (bar1, memo1),
                            (bar2, memo2),
                            code,
                            None).is_ok());
    }

    #[test]
    #[ignore]
    // Test funds loading, loan request, fulfilling and repayment
    fn test_request_fulfill_and_pay_loan() {
      let ledger_standalone = LedgerStandalone::new();
      ledger_standalone.poll_until_ready().unwrap();

      // Create txn builder
      let txn_builder_path = "tb_load_funds";

      // Load funds
      let funds_amount = 1000;
      load_funds(0, 0, funds_amount, txn_builder_path, PROTOCOL, HOST).unwrap();
      let data = load_data().unwrap();

      assert_eq!(data.borrowers[0].balance, funds_amount);

      fs::remove_file(txn_builder_path).unwrap();
      let _ = fs::remove_file(DATA_FILE);

      // Request a loan
      let txn_builder_path = "tb_loan";
      let loan_amount = 1200;
      let mut data = load_data().unwrap();
      data.add_loan(0, 0, loan_amount, 100, 8).unwrap();

      assert_eq!(data.loans.len(), 1);

      // Fulfill the loan request
      fulfill_loan(0, 0, txn_builder_path, None, PROTOCOL, HOST).unwrap();
      data = load_data().unwrap();

      assert_eq!(data.loans[0].status, LoanStatus::Active);
      assert_eq!(data.loans[0].balance, loan_amount);

      // Pay loan
      let payment_amount = 200;
      pay_loan(0, payment_amount, PROTOCOL, HOST).unwrap();
      data = load_data().unwrap();

      let _ = fs::remove_file(DATA_FILE);
      fs::remove_file(txn_builder_path).unwrap();
      fs::remove_file("tb_loan.debt.0").unwrap();
      fs::remove_file("tb_loan.fiat.0").unwrap();

      assert_eq!(data.loans[0].payments, 1);
    }
  }
}
