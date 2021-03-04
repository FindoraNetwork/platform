#![allow(clippy::type_complexity)]
#![allow(clippy::field_reassign_with_default)]

use ledger::data_model::*;
use promptly::prompt_default;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::env;
use std::fmt;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;
use submission_server::{TxnHandle, TxnStatus};
use txn_builder::TransactionBuilder;
use utils::{HashOf, SignatureOf};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};
use zei::xfr::structs::{OpenAssetRecord, OwnerMemo};

pub mod actions;
pub mod display_functions;
pub mod helpers;
pub mod kv;

use crate::actions::*;
use crate::helpers::compute_findora_dir;
use kv::{HasEncryptedTable, HasTable, KVStore, MixedPair};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LedgerStateCommitment(
    pub  (
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    ),
);

pub struct FreshNamer {
    base: String,
    i: u64,
    delim: String,
}

impl FreshNamer {
    pub fn new(base: String, delim: String) -> Self {
        Self { base, i: 0, delim }
    }
}

impl Iterator for FreshNamer {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        let ret = if self.i == 0 {
            self.base.clone()
        } else {
            format!("{}{}{}", self.base, self.delim, self.i - 1)
        };
        self.i += 1;
        Some(ret)
    }
}

fn default_sub_server() -> String {
    "https://testnet.findora.org:8669".to_string()
}

fn default_ledger_server() -> String {
    "https://testnet.findora.org:8668".to_string()
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct CliConfig {
    #[serde(default = "default_sub_server")]
    pub submission_server: String,
    #[serde(default = "default_ledger_server")]
    pub ledger_server: String,
    pub open_count: u64,
    #[serde(default)]
    pub ledger_sig_key: Option<XfrPublicKey>,
    #[serde(default)]
    pub ledger_state: Option<LedgerStateCommitment>,
    #[serde(default)]
    pub active_txn: Option<TxnBuilderName>,
}

impl HasTable for CliConfig {
    const TABLE_NAME: &'static str = "config";
    type Key = String;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct AssetTypeName(pub String);

impl HasTable for AssetTypeEntry {
    const TABLE_NAME: &'static str = "asset_types";
    type Key = AssetTypeName;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct KeypairName(pub String);

// NOTE(Nathan M): Due to zei not having (to my knowledge) a method for combining a
// public and a private key back into a key pair, the encrypted section of the
// MixedPair currently contains the entire key pair.
impl HasEncryptedTable for XfrKeyPair {
    const TABLE_NAME: &'static str = "enc_key_pairs";
    type Key = KeypairName;
    type Clear = XfrPublicKey;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct PubkeyName(pub String);

impl HasTable for XfrPublicKey {
    const TABLE_NAME: &'static str = "public_keys";
    type Key = PubkeyName;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct TxnName(pub String);

impl HasTable for (Transaction, TxnMetadata) {
    const TABLE_NAME: &'static str = "transactions";
    type Key = TxnName;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct TxnBuilderName(pub String);

impl HasTable for TxnBuilderEntry {
    const TABLE_NAME: &'static str = "transaction_builders";
    type Key = TxnBuilderName;
}

#[derive(
    Ord, PartialOrd, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, Hash, Default,
)]
pub struct TxoName(pub String);

impl HasTable for TxoCacheEntry {
    const TABLE_NAME: &'static str = "txo_cache";
    type Key = TxoName;
}

#[derive(Debug)]
pub enum CliError {
    KV,
    NickName(String),
    Reqwest,
    Serialization,
    RustyLine,
    UserFile(PathBuf),
    HomeDir,
    NewPublicKeyFetch,
    Password,
    NoneValue,
    NoTransactionInProgress,
    FindoraPlatformError,
    ZeiError,
    IOError(String),
    InconsistentLedger,
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CliError::KV => write!(f, "{}", stringify!(CliError::KV)),
            CliError::NickName(n) => {
                write!(f, "{}: {}", stringify!(CliError::NickName), n)
            }
            CliError::Reqwest => write!(f, "{}", stringify!(CliError::Reqwest)),
            CliError::Serialization => {
                write!(f, "{}", stringify!(CliError::Serialization))
            }
            CliError::RustyLine => write!(f, "{}", stringify!(CliError::RustyLine)),
            CliError::UserFile(n) => write!(
                f,
                "{}: {}",
                stringify!(CliError::UserFile),
                n.to_string_lossy()
            ),
            CliError::HomeDir => write!(f, "{}", stringify!(CliError::HomeDir)),
            CliError::NewPublicKeyFetch => {
                write!(f, "{}", stringify!(CliError::NewPublicKeyFetch))
            }
            CliError::Password => write!(f, "{}", stringify!(CliError::Password)),
            CliError::NoneValue => write!(f, "{}", stringify!(CliError::NoneValue)),
            CliError::NoTransactionInProgress => {
                write!(f, "{}", stringify!(CliError::NoTransactionInProgress))
            }
            CliError::FindoraPlatformError => {
                write!(f, "{}", stringify!(CliError::FindoraPlatformError))
            }
            CliError::ZeiError => write!(f, "{}", stringify!(CliError::ZeiError)),
            CliError::IOError(n) => {
                write!(f, "{}: {}", stringify!(CliError::IoError), n)
            }
            CliError::InconsistentLedger => {
                write!(f, "{}", stringify!(CliError::InconsistentLedger))
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct TxnMetadata {
    handle: Option<TxnHandle>,
    status: Option<TxnStatus>,
    new_asset_types: BTreeMap<AssetTypeName, AssetTypeEntry>,
    #[serde(default)]
    operations: Vec<OpMetadata>,
    #[serde(default)]
    signers: Vec<KeypairName>,
    // TODO
    #[serde(default)]
    new_txos: Vec<(String, TxoCacheEntry)>,
    #[serde(default)]
    finalized_txos: Option<Vec<TxoName>>,
    #[serde(default)]
    spent_txos: Vec<TxoName>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TxoCacheEntry {
    sid: Option<TxoSID>,
    owner: Option<PubkeyName>,
    #[serde(default)]
    asset_type: Option<AssetTypeName>,
    // What has this Txo been authenticated against?
    ledger_state: Option<LedgerStateCommitment>,
    record: TxOutput,
    owner_memo: Option<OwnerMemo>,
    opened_record: Option<OpenAssetRecord>,
    unspent: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AssetTypeEntry {
    asset: Box<Asset>,
    issuer_nick: Option<PubkeyName>,
    issue_seq_num: u64,
}

fn indent_of(indent_level: u64) -> String {
    let mut ret: String = Default::default();
    for _ in 0..indent_level {
        ret = format!("{}{}", ret, " ");
    }
    ret
}

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OpMetadata {
    DefineAsset {
        issuer_nick: PubkeyName,
        asset_nick: AssetTypeName,
    },
    IssueAsset {
        issuer_nick: PubkeyName,
        asset_nick: AssetTypeName,
        output_name: String,
        output_amt: u64,
        issue_seq_num: u64,
    },
    TransferAssets {
        inputs: Vec<(String, TxoCacheEntry)>,
        outputs: Vec<(String, TxoCacheEntry)>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TxnBuilderEntry {
    builder: TransactionBuilder,
    #[serde(default)]
    operations: Vec<OpMetadata>,
    #[serde(default)]
    new_asset_types: BTreeMap<AssetTypeName, AssetTypeEntry>,
    #[serde(default)]
    signers: Vec<KeypairName>,
    // TODO
    #[serde(default)]
    new_txos: Vec<(String, TxoCacheEntry)>,
    #[serde(default)]
    spent_txos: Vec<TxoName>,
}

pub trait CliDataStore {
    fn get_config(&self) -> Result<CliConfig>;
    fn update_config<F: FnOnce(&mut CliConfig) -> Result<()>>(
        &mut self,
        f: F,
    ) -> Result<()>;

    fn get_keypairs(&self) -> Result<Vec<KeypairName>>;
    fn get_keypair_pubkey(&self, k: &KeypairName) -> Result<Option<XfrPublicKey>>;
    fn delete_keypair(&mut self, k: &KeypairName) -> Result<()>;
    fn with_keypair<F: FnOnce(Option<&XfrKeyPair>) -> Result<()>>(
        &mut self,
        k: &KeypairName,
        f: F,
    ) -> Result<()>;
    fn get_encrypted_keypair(
        &self,
        k: &KeypairName,
    ) -> Result<Option<MixedPair<XfrPublicKey, XfrKeyPair>>>;
    fn get_pubkeys(&self) -> Result<BTreeMap<PubkeyName, XfrPublicKey>>;
    fn get_local_pubkeys(
        &self,
    ) -> Result<BTreeMap<crate::PubkeyName, zei::xfr::sig::XfrPublicKey>>;
    fn get_pubkey(&self, k: &PubkeyName) -> Result<Option<XfrPublicKey>>;
    fn exists_keypair(&self, k: &str) -> Result<bool>;
    fn delete_pubkey(&mut self, k: &PubkeyName) -> Result<Option<XfrPublicKey>>;
    fn add_key_pair(&mut self, k: &KeypairName, kp: XfrKeyPair) -> Result<()>;
    fn add_encrypted_keypair(
        &mut self,
        k: &KeypairName,
        kp: MixedPair<XfrPublicKey, XfrKeyPair>,
    ) -> Result<()>;
    fn add_public_key(&mut self, k: &PubkeyName, pk: XfrPublicKey) -> Result<()>;

    fn get_built_transactions(
        &self,
    ) -> Result<BTreeMap<TxnName, (Transaction, TxnMetadata)>>;
    fn get_built_transaction(
        &self,
        k: &TxnName,
    ) -> Result<Option<(Transaction, TxnMetadata)>>;
    fn build_transaction(
        &mut self,
        k_orig: &TxnBuilderName,
        k_new: &TxnName,
        metadata: TxnMetadata,
    ) -> Result<(Transaction, TxnMetadata)>;
    fn update_txn_metadata<F: FnOnce(&mut TxnMetadata) -> Result<()>>(
        &mut self,
        k: &TxnName,
        f: F,
    ) -> Result<()>;

    fn prepare_transaction(&mut self, k: &TxnBuilderName, seq_id: u64) -> Result<()>;
    fn get_txn_builder(&self, k: &TxnBuilderName) -> Result<Option<TxnBuilderEntry>>;
    fn get_txn_builders(&self) -> Result<BTreeMap<TxnBuilderName, TxnBuilderEntry>>;
    fn with_txn_builder<F: FnOnce(&mut TxnBuilderEntry) -> Result<()>>(
        &mut self,
        k: &TxnBuilderName,
        f: F,
    ) -> Result<()>;

    fn get_cached_txos(&self) -> Result<BTreeMap<TxoName, TxoCacheEntry>>;
    fn get_cached_txo(&self, k: &TxoName) -> Result<Option<TxoCacheEntry>>;
    fn delete_cached_txo(&mut self, k: &TxoName) -> Result<()>;
    fn cache_txo(&mut self, k: &TxoName, ent: TxoCacheEntry) -> Result<()>;

    fn get_asset_types(&self) -> Result<BTreeMap<AssetTypeName, AssetTypeEntry>>;
    fn get_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>>;
    fn update_asset_type<F: FnOnce(&mut AssetTypeEntry) -> Result<()>>(
        &mut self,
        k: &AssetTypeName,
        f: F,
    ) -> Result<()>;
    fn delete_asset_type(&self, k: &AssetTypeName) -> Result<Option<AssetTypeEntry>>;
    fn add_asset_type(&self, k: &AssetTypeName, ent: AssetTypeEntry) -> Result<()>;
}

fn prompt_for_config(prev_conf: Option<CliConfig>) -> Result<CliConfig> {
    let default_sub_server = prev_conf
        .as_ref()
        .map(|x| x.submission_server.clone())
        .unwrap_or_else(default_sub_server);
    let default_ledger_server = prev_conf
        .as_ref()
        .map(|x| x.ledger_server.clone())
        .unwrap_or_else(default_ledger_server);
    Ok(CliConfig {
        submission_server: prompt_default("Submission Server?", default_sub_server)
            .c(d!())?,
        ledger_server: prompt_default("Ledger Access Server?", default_ledger_server)
            .c(d!())?,
        open_count: 0,
        ledger_sig_key: prev_conf.as_ref().and_then(|x| x.ledger_sig_key),
        ledger_state: prev_conf.as_ref().and_then(|x| x.ledger_state.clone()),
        active_txn: prev_conf.as_ref().and_then(|x| x.active_txn.clone()),
    })
}

const VERSION: &str = concat!(
    env!("VERGEN_SEMVER"),
    "-",
    env!("VERGEN_SHA_SHORT"),
    " ",
    env!("VERGEN_BUILD_DATE"),
);

#[derive(StructOpt, Debug)]
#[structopt(about = "Build and manage transactions and assets on a findora ledger",
            rename_all = "kebab-case",
            version = VERSION)]
enum Actions {
    //////////////////// Simple API  /////////////////////////////////////////////////////////////////
    /// Initialize or change your local database configuration
    Setup {},

    /// Define and issue FRA
    InitFra {
        /// The nick name of issuer
        #[structopt(long)]
        nick: String,
    },

    /// Generate bash/zsh/fish/powershell completion files for this CLI
    GenCompletions {
        /// Output directory
        #[structopt(parse(from_os_str))]
        outdir: Option<PathBuf>,
        /// bash
        #[structopt(long)]
        bash: bool,
        /// zsh
        #[structopt(long)]
        zsh: bool,
        /// fish
        #[structopt(long)]
        /// pow
        fish: bool,
        /// poweshell
        #[structopt(long)]
        powershell: bool,
        /// elvish
        #[structopt(long)]
        elvish: bool,
    },

    /// Display the current configuration and ledger state
    ListConfig {},

    /// Get the latest state commitment data from the ledger
    QueryLedgerState {
        /// Whether to forget the old ledger public key
        #[structopt(short, long)]
        forget_old_key: bool,
    },

    /// Generate a new key pair for <nick>
    KeyGen {
        /// Identity nickname
        nick: String,
    },

    /// Load an existing key pair for <nick>
    LoadKeypair {
        /// Identity nickname
        nick: String,
    },

    /// Load a public key for <nick>
    LoadPublicKey {
        /// Identity nickname
        nick: String,
    },

    /// Restore keypair from passphrase bip44, setting the name to <nick>
    RestoreFromMnemonicBip44 {
        /// Identity
        nick: String,
    },

    /// List all the addresses present in the database.
    ListAddresses {},

    /// List all the key pairs present in the database.
    ListKeys {},

    /// Display information about the public key for <nick>
    ListPublicKey {
        /// Identity nickname
        nick: String,
    },

    /// Display information about the key pair for <nick>
    ListKeypair {
        /// Identity nickname
        nick: String,

        /// Also display the secret key
        #[structopt(short, long)]
        show_secret: bool,
    },

    /// Permanently delete the key pair for <nick>
    DeleteKeypair {
        /// Identity nickname
        nick: String,
    },

    /// Permanently delete the public key for <nick>
    DeletePublicKey {
        /// Identity nickname
        nick: String,
    },

    /// Exports the keypair for <nick>, encrypted with its existing password
    ExportKeypair {
        /// Identity nickname
        nick: String,
    },

    /// Changes the keypair password for <nick>
    ChangeKeypairPassword {
        /// Identity nickname
        nick: String,
    },

    /// Imports an encrypted keypair, setting the name to <nick>
    ImportEncryptedKeypair {
        /// Identity
        nick: String,
    },

    /// Define an asset in a single step
    SimpleDefineAsset {
        /// Issuer key
        issuer_nick: String,
        /// Name for the asset type
        asset_nick: String,
        /// When define FRA, set this field to true
        #[structopt(long)]
        is_fra: bool,
    },

    /// Issue an asset in a single step
    SimpleIssueAsset {
        /// Name for the asset type
        asset_nick: String,
        /// Number of coins to issue
        amount: u64,
    },

    /// Display the amounts available for each asset types and local key pair
    Balances {},

    //////////////////// Advanced API  ///////////////////////////////////////////////////////////////
    /// List all the asset types
    ListAssetTypes {},

    /// Display the information about a specific asset type
    ListAssetType {
        /// Asset type nickname
        nick: String,
    },

    /// Fetch information about an asset type from the blockchain
    QueryAssetType {
        /// Replace the existing asset type entry (if it exists)
        #[structopt(short, long)]
        replace: bool,
        /// Asset type nickname
        nick: String,
        /// Asset type code (b64)
        code: String,
    },

    /// Initialize a transaction builder
    InitializeTransaction {
        /// Optional transaction name
        nick: String,
    },

    /// List the details of the current transaction
    ListTxn {},

    /// Finalize the current transaction, preparing it for submission
    BuildTransaction {},

    /// Create the definition of an asset and put it in a transaction builder
    DefineAsset {
        /// Which transaction?
        txn_nick: String,
        /// Issuer key
        issuer_nick: String,
        /// Name for the asset type
        asset_nick: String,
        /// When define FRA, set this field to true
        #[structopt(long)]
        is_fra: bool,
    },

    /// Create a transaction part corresponding to the issuance of an asset
    IssueAsset {
        /// Which transaction?
        txn_nick: String,
        /// Name for the asset type
        asset_nick: String,
        /// Sequence number of this issuance
        issue_seq_num: u64,
        /// Amount to issue
        amount: u64,
    },

    /// Create a transaction part corresponding to the transfer of an asset
    TransferAssets {
        #[structopt(short, long)]
        /// Which builder?
        builder: Option<String>,
    },

    /// Show the details of a built transaction
    ListBuiltTransaction {
        /// Nickname of the transaction
        nick: String,
    },

    /// Show the details of all built transactions
    ListBuiltTransactions {
        // TODO: options?
    },

    /// Submit a built transaction
    Submit {
        /// Which txn?
        nick: String,
    },

    /// Fetch the status of a transaction from the blockchain
    Status {
        // TODO: how are we indexing in-flight transactions?
        /// Which txn?
        txn: String,
    },

    /// Fetch the status of a transaction from the blockchain and update the local store accordingly
    StatusCheck {
        // TODO: how are we indexing in-flight transactions?
        /// Which txn?
        txn: String,
    },

    /// Display information about a specific transaction output
    ListTxo {
        /// nickname
        id: String,
    },

    /// Unlock a TXO for its owner
    UnlockTxo {
        /// nickname
        id: String,
    },

    /// Display a serialized owner memo for a TXO
    ShowOwnerMemo {
        /// TXO nickname
        id: String,
    },

    /// Display a serialized owner memo for a TXO
    LoadOwnerMemo {
        /// Overwrite an existing owner memo we have?
        #[structopt(short, long)]
        overwrite: bool,
        /// TXO nickname
        id: String,
    },

    /// Display the locally stored transaction outputs according to some filter
    ListTxos {
        /// Only unspent?
        #[structopt(short, long)]
        unspent: bool,
        /// Filter by owner id
        #[structopt(short, long)]
        id: Option<String>,
    },

    /// Fetch information of a transaction output from the blockchain
    QueryTxo {
        /// Local nickname?
        nick: String,
        /// Which SID?
        sid: Option<u64>,
    },

    /// Fetch information of all locally stored transaction outputs from the blockchain
    QueryTxos {},
}

fn serialize_or_str<T: Serialize>(x: &Option<T>, s: &str) -> String {
    x.as_ref()
        .map(|x| serde_json::to_string(&x).unwrap())
        .unwrap_or_else(|| s.to_string())
}

fn print_conf(conf: &CliConfig) {
    println!("Submission server: {}", conf.submission_server);
    println!("Ledger access server: {}", conf.ledger_server);
    println!(
        "Ledger public signing key: {}",
        serialize_or_str(&conf.ledger_sig_key, "<UNKNOWN>")
    );
    println!(
        "Ledger state commitment: {}",
        conf.ledger_state
            .as_ref()
            .map(|x| b64enc(&((x.0).0).0.hash))
            .unwrap_or_else(|| "<UNKNOWN>".to_string())
    );
    println!(
        "Ledger block idx: {}",
        conf.ledger_state
            .as_ref()
            .map(|x| format!("{}", (x.0).1))
            .unwrap_or_else(|| "<UNKNOWN>".to_string())
    );
    println!(
        "Current focused transaction builder: {}",
        conf.active_txn
            .as_ref()
            .map(|x| x.0.clone())
            .unwrap_or_else(|| "<NONE>".to_string())
    );

    let path = compute_findora_dir().unwrap();

    println!("Directory of wallet: {}", path.display());
}

fn run_action<S: CliDataStore>(action: Actions, store: &mut S) -> Result<()> {
    // println!("{:?}", action);

    use Actions::*;
    let ret = match action {
        //////////////////// Simple API  ///////////////////////////////////////////////////////////////
        Setup {} => setup(store),

        InitFra { nick } => init_fra(store, nick),

        GenCompletions { .. } => panic!("GenCompletions should've been handle already!"),

        ListConfig {} => list_config(store),

        KeyGen { nick } => key_gen(store, nick),

        ListAddresses {} => list_addresses(store),

        ListKeys {} => list_keys(store),

        ListKeypair { nick, show_secret } => list_keypair(store, nick, show_secret),

        ListPublicKey { nick } => list_public_key(store, nick),

        LoadKeypair { nick } => load_key_pair(store, nick),

        LoadPublicKey { nick } => load_public_key(store, nick),

        DeleteKeypair { nick } => delete_keypair(store, nick),

        DeletePublicKey { nick } => delete_public_key(store, nick),

        RestoreFromMnemonicBip44 { nick } => restore_from_mnemonic_bip44(store, nick),

        SimpleDefineAsset {
            issuer_nick,
            asset_nick,
            is_fra,
        } => simple_define_asset(store, issuer_nick, asset_nick, is_fra),

        SimpleIssueAsset { asset_nick, amount } => {
            simple_issue_asset(store, asset_nick, amount)
        }

        Balances {} => compute_balances(store),

        //////////////////// Advanced API  /////////////////////////////////////////////////////////////
        QueryLedgerState { forget_old_key } => query_ledger_state(store, forget_old_key),

        ListTxos { unspent, id } => list_txos(store, unspent, id),

        ListTxo { id } => list_txo(store, id),

        UnlockTxo { id } => unlock_txo(store, id),

        ShowOwnerMemo { id } => show_owner_memo(store, id),

        LoadOwnerMemo { overwrite, id } => load_owner_memo(store, overwrite, id),

        QueryTxo { nick, sid } => query_txo(store, nick, sid),

        QueryTxos {} => query_txos(store),

        // ListTxnBuilders {} => list_txn_builders(store),

        //ListTxnBuilder { nick } => list_txn_builder(store, nick),
        ListTxn {} => list_txn(store),

        ListAssetTypes {} => list_asset_types(store),

        ListAssetType { nick } => list_asset_type(store, nick),

        QueryAssetType {
            replace,
            nick,
            code,
        } => query_asset_type(store, replace, nick, code),

        InitializeTransaction { nick } => prepare_transaction(store, nick),

        ListBuiltTransaction { nick } => list_built_transaction(store, nick),

        ListBuiltTransactions {} => list_built_transactions(store),

        Status { txn } => status(store, txn),

        StatusCheck { txn } => status_check(store, txn),

        DefineAsset {
            txn_nick,
            issuer_nick,
            asset_nick,
            is_fra,
        } => define_asset(store, txn_nick, issuer_nick, asset_nick, is_fra),

        IssueAsset {
            txn_nick: builder,
            asset_nick,
            issue_seq_num,
            amount,
        } => issue_asset(store, builder, asset_nick, issue_seq_num, amount),

        TransferAssets { builder } => transfer_assets(store, builder),

        BuildTransaction {} => build_transaction(store),

        Submit { nick } => submit(store, nick),

        ExportKeypair { nick } => export_keypair(store, nick),

        ChangeKeypairPassword { nick } => change_keypair_password(store, nick),
        ImportEncryptedKeypair { nick } => import_encrypted_keypair(store, nick),
    };
    store
        .update_config(|conf| {
            // println!("Opened {} times before", conf.open_count);
            conf.open_count += 1;
            Ok(())
        })
        .c(d!())?;
    ret
}

fn main() {
    fn inner_main() -> Result<()> {
        let action = Actions::from_args();

        if let Actions::GenCompletions {
            outdir,
            bash,
            zsh,
            fish,
            powershell,
            elvish,
        } = action
        {
            let bin_path = PathBuf::from(std::env::args().next().unwrap());
            let bin_name = String::from(bin_path.file_name().unwrap().to_string_lossy());

            let mut shells = vec![];
            if bash {
                shells.push(clap::Shell::Bash);
            }
            if zsh {
                shells.push(clap::Shell::Zsh);
            }
            if fish {
                shells.push(clap::Shell::Fish);
            }
            if powershell {
                shells.push(clap::Shell::PowerShell);
            }
            if elvish {
                shells.push(clap::Shell::Elvish);
            }

            if shells.is_empty() {
                println!(
                    "Please specify one or more shells to generate completions for."
                );
                println!("See 'findora gen-completions --help' for supported options.");
                std::process::exit(-1);
            }

            if let Some(outdir) = outdir {
                fs::create_dir_all(&outdir).c(d!(CliError::UserFile(outdir.clone())))?;

                for s in shells {
                    Actions::clap().gen_completions(&bin_name, s, &outdir);
                }
            } else if shells.len() == 1 {
                Actions::clap().gen_completions_to(
                    &bin_name,
                    shells[0],
                    &mut std::io::stdout(),
                );
            } else {
                println!(
                    "Please select exactly one shell to print to stdout, or provide an output directory."
                );
                std::process::exit(-1);
            }

            return Ok(());
        }

        // use Actions::*;
        let mut home = compute_findora_dir().c(d!())?;

        fs::create_dir_all(&home).c(d!(CliError::UserFile(home.clone())))?;
        home.push("cli2_data.sqlite");
        let first_time = !std::path::Path::exists(&home);
        let mut db = KVStore::open(home.clone()).c(d!())?;
        if first_time {
            println!(
                "No config found at {:?} -- triggering first-time setup",
                &home
            );
            db.update_config(|conf| {
                *conf = prompt_for_config(None).c(d!())?;
                Ok(())
            })
            .c(d!())?;

            if let Actions::Setup { .. } = action {
                return Ok(());
            }
        }

        run_action(action, &mut db).c(d!())?;
        Ok(())
    }

    // Custom error handler logic.
    //
    // If the call to 'inner_main' encountered an error, display the error it
    // encountered, then make repeated calls to 'Error::source' to walk the source
    // list, displaying each error in the chain, in order.
    //
    // Finally, check to see if the error encountered has an associated backtrace, and,
    // if so, display it.
    pnk!(inner_main());
}

#[cfg(not(feature = "no-bugtracker"))]
pub mod messages {
    pub const PANIC_STRING: &str = concat!(
        "An unknown error occurred, this is a bug. Please help us fix it by reporting it at:\n",
        "https://bugtracker.findora.org/projects/testnet/issues/new?issue[subject]=findora%20CLI%20(build%20",
        env!("VERGEN_SHA_SHORT"),
        ")%3A\n",
        "\
        Please copy and paste the entire error message, as well as any preceding output\
        into the description field of the bug report.\n\
        \
        Here is what context is available:"
    );
}

#[cfg(feature = "no-bugtracker")]
pub mod messages {
    pub const PANIC_STRING: &'static str = "\
An unknown error occurred, this is a bug. Please help us fix it by reporting it to:
testnet@findora.org

Please copy and paste the entire error message, as well as any preceding output, at
the bottom of the email.

Here is what context is available:";
}
