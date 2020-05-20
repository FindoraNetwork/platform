#![deny(warnings)]
use clap::{App, Arg, ArgMatches};
use credentials::{
  credential_commit, credential_issuer_key_gen, credential_sign, credential_user_key_gen,
  CredCommitment, CredCommitmentKey, CredIssuerPublicKey, CredIssuerSecretKey, Credential,
};
use ledger::data_model::Transaction;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use txn_builder::{BuildsTransactions, TransactionBuilder};
use utils::{protocol_host, SUBMIT_PORT};
use zei::xfr::sig::XfrKeyPair;

/// Represents a file that can be searched

struct AIR {
  prng: ChaChaRng,
  issuer_pk: CredIssuerPublicKey,
  issuer_sk: CredIssuerSecretKey,
  user_commitment: HashMap<String, (CredCommitment, CredCommitmentKey)>,
}

impl AIR {
  fn new() -> Self {
    let mut prng = ChaChaRng::from_entropy();
    let attrs = vec![(String::from("dob"), 8),
                     (String::from("pob"), 3),
                     (String::from("sex"), 1)];
    let (issuer_pk, issuer_sk) = credential_issuer_key_gen(&mut prng, &attrs);
    AIR { prng,
          issuer_pk,
          issuer_sk,
          user_commitment: HashMap::new() }
  }

  /// Return a JSON txn corresponding to this request
  pub fn add_assign_txn(&mut self, builder: &mut TransactionBuilder) {
    let (user_pk, user_sk) = credential_user_key_gen(&mut self.prng, &self.issuer_pk);
    let attr_vals: Vec<(String, &[u8])> = vec![(String::from("dob"), b"08221964"),
                                               (String::from("pob"), b"666"),
                                               (String::from("sex"), b"M")];
    let sig = credential_sign(&mut self.prng, &self.issuer_sk, &user_pk, &attr_vals).unwrap();
    let credential = Credential { signature: sig,
                                  attributes: attr_vals.iter()
                                                       .map(|(k, v)| (k.clone(), v.to_vec()))
                                                       .collect(),
                                  issuer_pub_key: self.issuer_pk.clone() };
    // let xfr_key_pair = XfrKeyPair::zei_from_bytes(&hex::decode(KEY_PAIR_STR).unwrap());
    let xfr_key_pair = XfrKeyPair::generate(&mut self.prng);
    let (commitment, proof, key) = credential_commit(&mut self.prng,
                                                     &user_sk,
                                                     &credential,
                                                     xfr_key_pair.get_pk_ref().as_bytes()).unwrap();
    let user_pk_s = serde_json::to_string(&user_pk).unwrap();
    self.user_commitment
        .insert(user_pk_s, (commitment.clone(), key));
    if let Err(e) = builder.add_operation_air_assign(&xfr_key_pair,
                                                     user_pk,
                                                     commitment,
                                                     self.issuer_pk.clone(),
                                                     proof)
    {
      panic!(format!("Something went wrong: {:#?}", e));
    }
  }
}

fn run_txns(n: usize, batch_size: usize) -> Result<(), Box<dyn std::error::Error>> {
  let mut air = AIR::new();
  let client = reqwest::blocking::Client::new();
  let (protocol, host) = protocol_host();
  let mut min: Duration = Duration::new(1_000_000_000, 999_999_999);
  let mut max: Duration = Duration::new(0, 0);
  let mut total: Duration = Duration::new(0, 0);
  for i in 0..n {
    let mut builder = TransactionBuilder::default();
    air.add_assign_txn(&mut builder);
    let txn = builder.transaction();
    let instant = Instant::now();
    let _ = client.post(&format!("{}://{}:{}/submit_transaction", protocol, host, SUBMIT_PORT))
                  .json::<Transaction>(&txn)
                  .send()?;

    if (i + 1) % batch_size == 0 {
      let _resp = client.post(&format!("{}://{}:{}/force_end_block", protocol, host, SUBMIT_PORT))
                        .send();
    }

    total += instant.elapsed();
    if instant.elapsed() < min {
      min = instant.elapsed();
    }
    if instant.elapsed() > max {
      max = instant.elapsed();
    }
  }
  println!("total = {:#?}, mean = {:#?}, min = {:#?}, max = {:#?}",
           total,
           total / (n as u32),
           min,
           max);
  Ok(()) // println!("{}", txn);
}

fn parse_args() -> ArgMatches<'static> {
  App::new("generate AIR assign transactions").version("0.1.0")
                       .author("Brian Rogoff <brian@findora.org>")
                       .about("REPL with argument parsing")
                       .arg(Arg::with_name("num_txns").short("n")
                            .long("num_txns")
                            .takes_value(true)
                            .help("number of txns generated"))
                       .arg(Arg::with_name("batch_size").short("bs")
                            .long("batch_size")
                            .takes_value(true)
                            .help("Number of txns before a force_end_block"))
                       .get_matches()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  flexi_logger::Logger::with_env().start().unwrap();
  let args = parse_args();
  let num_txns = args.value_of("num_txns")
                     .map_or(1, |s| s.parse::<usize>().unwrap());
  let batch_size = args.value_of("batch_size")
                       .map_or(1, |s| s.parse::<usize>().unwrap());
  run_txns(num_txns, batch_size)
}
