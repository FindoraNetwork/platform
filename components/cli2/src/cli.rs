#![deny(warnings)]
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "Build and manage transactions and assets on a findora ledger",
            rename_all = "kebab-case")]
enum Actions {
  KeyGen {
    /// Identity nickname
    nick: String,
  },
  LoadKeyPair {
    /// Identity nickname
    nick: String,
    /// Keypair file
    #[structopt(parse(from_os_str))]
    kp_file: PathBuf,
  },
  AddPublicKey {
    /// Identity nickname
    nick: String,
    /// public key file
    #[structopt(parse(from_os_str))]
    key_file: PathBuf,
  },
  PrepareTransaction {
    /// Optional transaction name
    nick: Option<String>,
  },
  DefineAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
    /// Issuer key
    key_nick: String,
    /// Name for the asset type
    asset_name: String,
  },
  IssueAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
    /// Issuer key
    key_nick: String,
    /// Name for the asset type
    asset_name: String,
    /// Amount to issue
    amount: u64,
  },
  TransferAsset {
    #[structopt(short, long)]
    /// Which txn?
    txn: Option<String>,
  },
  ListTransaction {
    /// txn id
    txn: Option<String>,
  },
  ListTransactions {
    // TODO: options?
  },
  Submit {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    /// Which txn?
    txn: String,
  },
  Status {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    // TODO: how are we indexing in-flight transactions?
    /// Which txn?
    txn: String,
  },
  ListUtxos {
    #[structopt(short, long, default_value = "http://localhost:8669")]
    /// Base URL for the submission server
    server: String,
    /// Whose UTXOs?
    id: Option<String>,
  },
}

fn main() {
  let action = Actions::from_args();

  // use Actions::*;

  println!("{:?}", action);
}
