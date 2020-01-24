#![deny(warnings)]
#![feature(slice_patterns)]
// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials

use clap;
use clap::{Arg, App, ArgMatches};
use hex;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rmp_serde;
use sha2::{Digest, Sha256};
use serde::{Deserialize, Serialize};
use sparse_merkle_tree::{SmtMap256/*, check_merkle_proof*/};
// use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::io;
use std::io::{Error, ErrorKind};
use std::path::Path;
use zei::api::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey,
  ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};

// Default file path of the anonymous credential registry
const DEFAULT_REGISTRY_PATH: &str = "acreg.json";

trait TypeName {
  fn type_string(&self) -> &'static str;
}

impl TypeName for ACUserPublicKey {
  fn type_string(&self) -> &'static str {
    "ACUserPublicKey"
  }
}

impl TypeName for ACIssuerPublicKey {
  fn type_string(&self) -> &'static str {
    "ACIssuerPublicKey"
  }
}

/*
pub fn verify_credential(issuer_pub_key: &ACIssuerPublicKey<P::G1, P::G2>,
                         attrs: &[P::ScalarField],
                         bitmap: &[bool],
                         credential: &ACSignature<P::G1>)
                         -> Result<(), ZeiError> {
}
*/

// Test anonymous credentials on fixed inputs. Similar to
// Zei's credentials_tests.
fn test(_global_state: &mut GlobalState, _args: &ArgMatches) -> Result<(), std::io::Error> {
  let mut prng: ChaChaRng;
  // For a real application, the seed should be random.
  prng = ChaChaRng::from_seed([0u8; 32]);

  // Attributes to be revealed. For example, they might be:
  //    account balance, zip code, credit score, and timestamp
  // In this case, account balance (first) will not be revealed.
  let bitmap = [false, true, true, true];
  let attrs = [92_574_500u64.to_le_bytes(),
               95_050u64.to_le_bytes(),
               720u64.to_le_bytes(),
               20_190_820u64.to_le_bytes()];
  let att_count = bitmap.len();
  let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, att_count);

  println!("Issuer public key: {:?}", issuer_pk);
  println!("Issuer secret key: {:?}", issuer_sk);

  let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer_pk);
  println!("User public key: {:#?}", user_pk);
  println!("Address of user public key: {:?}", sha256(&user_pk));

  // The user secret key holds [u64; 6], but with more structure.
  println!("User secret key: {:?}", user_sk);

  // Issuer vouches for the user's attributes given above.
  let sig = ac_sign(&mut prng, &issuer_sk, &user_pk, &attrs[..]);
  println!("Credential signature: {:?}", sig);

  // The user presents this to the second party in a transaction as proof
  // attributes have been committed without revealing the values.
  let reveal_sig = ac_reveal(&mut prng, &user_sk, &issuer_pk, &sig, &attrs, &bitmap).unwrap();

  // Decision point. Does the second party agree to do business?
  // Sometimes this is presumed such as a syndicated investment
  // round, where you'll take money from everyone qualified. Other
  // times, there might be an off-chain negotiation to decide
  // whether to provisionally accept the deal.

  let mut revealed_attrs = vec![];
  for (attr, b) in attrs.iter().zip(&bitmap) {
    if *b {
      revealed_attrs.push(attr.clone());
    }
  }

  // Proves the attributes are what the user committed to. Anyone
  // with the revealed attributes and the reveal signature can do
  // this. But presumably, the reveal signature alone is insufficient to
  // derive the attributes. Presumably if the range of legal values were small,
  // exhaustive search would not be too exhausting. (?)
  if let Err(e) = ac_verify(&issuer_pk, revealed_attrs.as_slice(), &bitmap, &reveal_sig) {
    Err(Error::new(ErrorKind::InvalidInput, format!("{}", e)))
  } else {
    Ok(())
  }
}

// Return the SHA256 hash of T as a hexadecimal string.
fn sha256<T>(key: &T) -> String
  where T: Serialize + TypeName
{
  println!("ipk type: {}", key.type_string());
  let mut bytes = vec![];
  key.serialize(&mut rmp_serde::Serializer::new(&mut bytes))
     .unwrap();
  let mut hasher = Sha256::new();
  // Salt the hash to avoid leaking information about other uses of
  // sha256 on the user's public key.
  hasher.input(key.type_string());
  hasher.input(bytes.as_slice());
  hex::encode(hasher.result())
}

#[derive(Debug)]
struct GlobalState<'a> {
  registry: Vec<String>,
  smt_map: SmtMap256<&'a str>,
}

impl GlobalState<'_> {
  fn new() -> Self {
    GlobalState {
      registry: Vec::<String>::new(),
      smt_map: SmtMap256::<&str>::new(),
    }
  }
}

#[derive(Debug, Serialize, Deserialize)]
struct Issuer {
  public_key: ACIssuerPublicKey,
  secret_key: ACIssuerSecretKey,
}

#[derive(Debug, Serialize, Deserialize)]
struct User {
  public_key: ACUserPublicKey,
  secret_key: ACUserSecretKey,
}

// Generate a new issuer for anonymous credentials.
fn new_issuer() -> Issuer {
  let mut prng: ChaChaRng;
  // For a real application, the seed should be random.
  prng = ChaChaRng::from_seed([0u8; 32]);
  let att_count = 10;
  let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut prng, att_count);
  Issuer { public_key: issuer_pk,
           secret_key: issuer_sk }
}

#[derive(Debug, Serialize, Deserialize)]
struct AddrIssuer {
  address: String,
  value_type: u32,
  value: Issuer,
}

#[derive(Debug, Serialize, Deserialize)]
struct AddrUser {
  address: String,
  value_type: u32,
  value: User,
}
type Signature = ACSignature;

#[derive(Debug, Serialize, Deserialize)]
struct AddrValue<V> {
  address: String,
  value_type: i32,
  value: V,
}

// Generic key-value lookup and deserialization
fn lookup<T>(global_state: &mut GlobalState, address: &str) -> Option<T>
  where for<'d> T: Deserialize<'d>
{
  // println!("contents  = {}", &contents);
  global_state.registry.iter().rev().find_map(|x: &String| match serde_json::from_str::<AddrValue<T>>(x) {
    Ok(item) => {
      println!("item.address = {}, address  = {}", &item.address, &address);
      if item.address == address {
        Some(item.value)
      } else {
        None
      }
    }
    Err(_) => {
      // TODO Report errors other than "missing field" errors.
      None
    }
  })
}

fn append_to_registry<T: Serialize>(global_state: &mut GlobalState, item: T) -> Result<(), std::io::Error> {
  let item_json = serde_json::to_string(&item).unwrap();
  global_state.registry.push(item_json);
  Ok(())
}

// Generate a new issuer and append it to the registry.
fn add_issuer(global_state: &mut GlobalState) -> Result<(), std::io::Error> {
  println!("subcommand addissuer");
  let issuer = new_issuer();
  let address = sha256(&issuer.public_key);
  println!("Issuer address: {}", &address);

  let a = AddrIssuer { address,
                       value_type: 2,
                       value: issuer };
  append_to_registry::<AddrIssuer>(global_state, a)
}

fn add_user(global_state: &mut GlobalState, issuer: &str) -> Result<(), std::io::Error> {
  println!("Looking up issuer: {}", issuer);
  if let Some(issuer) = lookup::<Issuer>(global_state, issuer) {
    let mut prng: ChaChaRng;
    // For a real application, the seed should be random.
    prng = ChaChaRng::from_seed([0u8; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer.public_key);
    let au = AddrUser { address: sha256(&user_pk),
                        value_type: 2,
                        value: User { public_key: user_pk,
                                      secret_key: user_sk } };
    println!("Added user: {}", au.address);
    append_to_registry::<AddrUser>(global_state, au)
  } else {
    Err(Error::new(ErrorKind::InvalidInput, format!("lookup of issuer {} failed", issuer)))
  }
}


#[derive(Debug, Serialize, Deserialize)]
struct AddrSig {
  address: String,
  value_type: u32,
  value: ACSignature,
}

fn sign(global_state: &mut GlobalState, user: &str, issuer: &str) -> Result<(), std::io::Error> {
  match (lookup::<User>(global_state, user), lookup::<Issuer>(global_state, issuer)) {
    (Some(user_keys), Some(issuer_keys)) => {
      let mut prng: ChaChaRng;
      // TODO Share one prng across all invocations.
      prng = ChaChaRng::from_seed([0u8; 32]);

      let attrs = [0u64.to_le_bytes(),
                   1u64.to_le_bytes(),
                   2u64.to_le_bytes(),
                   3u64.to_le_bytes()];
      let sig = ac_sign(&mut prng,
                        &issuer_keys.secret_key,
                        &user_keys.public_key,
                        &attrs);

      // TODO Using the hash of the user's public key as the signature
      // address precludes multiple signatures
      let addr_sig = AddrSig { address: user.to_string(),
                               value_type: 2,
                               value: sig };

      append_to_registry::<AddrSig>(global_state, addr_sig)
    }
    (None, None) => {
      Err(Error::new(ErrorKind::InvalidInput, "Unable to find either issuer or user"))
    },
    (None, _) => {
      Err(Error::new(ErrorKind::InvalidInput, "Unable to find user"))
    },
    (_, None) => {
      Err(Error::new(ErrorKind::InvalidInput, "Unable to find issuer"))
    },
  }
}

type Proof = ACRevealSig;

#[derive(Debug, Serialize, Deserialize)]
struct AddrProof {
  address: String,
  value_type: u32,
  value: Proof,
}

fn reveal(global_state: &mut GlobalState, user: &str, issuer: &str) -> Result<(), std::io::Error> {
  match (lookup::<User>(global_state, user),
         lookup::<Issuer>(global_state, issuer),
         lookup::<Signature>(global_state, user)) {
    (Some(user_keys), Some(issuer_keys), Some(sig)) => {
      let mut prng: ChaChaRng;
      // TODO Share one prng across all invocations.
      prng = ChaChaRng::from_seed([0u8; 32]);

      let attrs = [0u64.to_le_bytes(),
                   1u64.to_le_bytes(),
                   2u64.to_le_bytes(),
                   3u64.to_le_bytes()];
      let bitmap = [false, false, false, false];
      // TODO handle the error
      let proof = ac_reveal(&mut prng,
                            &user_keys.secret_key,
                            &issuer_keys.public_key,
                            &sig,
                            &attrs,
                            &bitmap).unwrap();

      // TODO Using the hash of the user's public key as the proof
      // address precludes multiple proofs
      let addr_proof = AddrProof { address: user.to_string(),
                                   value_type: 2,
                                   value: proof };
      append_to_registry::<AddrProof>(global_state, addr_proof)
    },
    (None, _, _) => Err(Error::new(ErrorKind::InvalidInput, "Unable to find user")),
    (_, None, _) => Err(Error::new(ErrorKind::InvalidInput, "Unable to find issuer")),
    (_, _, None) => Err(Error::new(ErrorKind::InvalidInput, "Unable to find signature")),
  }
}

fn verify(global_state: &mut GlobalState, user: &str, issuer: &str) -> Result<(), std::io::Error> {
  println!("subcommand verify user: {} issuer: {}", user, issuer);
  match (lookup::<Issuer>(global_state, issuer), lookup::<Proof>(global_state, user)) {
    (Some(issuer_keys), Some(proof)) => {
      let attrs = [0u64.to_le_bytes(),
                   1u64.to_le_bytes(),
                   2u64.to_le_bytes(),
                   3u64.to_le_bytes()];
      let bitmap = [false, false, false, false];
      if let Err(e) = ac_verify(&issuer_keys.public_key, &attrs, &bitmap, &proof) {
        Err(Error::new(ErrorKind::InvalidInput, format!("{}", e)))
      } else {
        Ok(())
      }
    }
    (None, _) => Err(Error::new(ErrorKind::InvalidInput, "Unable to find issuer")),
    (_, None) => Err(Error::new(ErrorKind::InvalidInput, "Unable to find proof")),
  }
}

fn first_char(s: &str, c: char) -> bool {
  match s.chars().nth(0) {
    None => false,
    Some(c0) => c0.to_ascii_uppercase() == c.to_ascii_uppercase(),
  }
}

fn parse_args() -> ArgMatches<'static> {
  App::new("Test REPL")
    .version("0.1.0")
    .author("Brian Rogoff <brogoff@gmail.com>")
    .about("REPL with argument parsing")
    .arg(Arg::with_name("registry")
         .short("r")
         .long("registry")
         .takes_value(true)
         .help("the registry dude"))
    .arg(Arg::with_name("file")
         .short("f")
         .takes_value(true)
         .help("Name of the file"))
    .get_matches()
}

fn main() -> Result<(), std::io::Error> {
  let args = parse_args();

  let registry_path = Path::new(args.value_of("registry").unwrap_or(DEFAULT_REGISTRY_PATH));
  let mut _registry_file = OpenOptions::new()
    .read(true)
    .append(true)
    .create(true)
    .open(&registry_path)?;

  let mut global_state = GlobalState::new();

  println!("The registry path is {:?}", registry_path);

  println!("REPL");

  loop {
    print!(">>>");
    io::stdout().flush()?;

    let mut line = String::new();

    io::stdin().read_line(&mut line)
      .expect("Failed to read line");

    // let res: Vec<&str> = line.trim().split(' ').collect();
    // let res_slice: &[&str] = &res;
    match line.trim().split(' ').collect::<Vec<&str>>().as_slice() {
      ["test"] /* if x.as_bytes() == "test".as_bytes() */ =>
        test(&mut global_state, &args)?,
      ["addissuer"]  =>
        add_issuer(&mut global_state)?,
      ["adduser", issuer] =>
        add_user(&mut global_state, &issuer)?,
      ["sign", user, issuer]  =>
        sign(&mut global_state, &user, &issuer)?,
      ["reveal", user, issuer] =>
        reveal(&mut global_state, &user, &issuer)?,
      ["verify", user, issuer] =>
        verify(&mut global_state, &user, &issuer)?,
      [x, ..] if first_char(x, 'Q') /*x.chars().nth(0).to_uppercase() == 'Q' */ => break,
      _ => println!("Invalid line: {}", line),
    }
  };
  Ok(())
}
