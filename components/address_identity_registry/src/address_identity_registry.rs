//#![deny(warnings)]
#![feature(slice_patterns)]
// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials
#[macro_use]
extern crate lazy_static;

use clap;
use clap::{App, Arg, ArgMatches};
use colored::*;
use cryptohash::sha256;
use hex;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rmp_serde;
use rustyline::error::ReadlineError;
use rustyline::Editor;
// use sha2::{Digest, Sha256};
use serde::{Deserialize, Serialize};
use sha256::DIGESTBYTES;
use sparse_merkle_tree::SmtMap256;
use std::collections::HashMap;
use std::path::Path;
use zei::api::anon_creds::{
  ac_keygen_issuer,
  ac_keygen_user,
  ac_reveal,
  ac_reveal_with_rand, // need both variants here
  ac_sign,
  ac_verify,
  ACIssuerPublicKey,
  ACIssuerSecretKey,
  ACRevealSig,
  ACUserPublicKey,
  ACUserSecretKey,
};

// Default file path of the anonymous credential registry
const DEFAULT_REGISTRY_PATH: &str = "acreg.json";
const HELP_HELP_STRING: &str = "Prints a help message";
const HELP_ADDISSUER_STRING: &str =
  "Creates an issuer which can be referred to by the name \"issuer_name\"";
const HELP_ADDUSER_STRING: &str = "Creates an user named \"user_name\" bound to \"issuer_name\"";
const HELP_GETCREDS_STRING: &str = "Creates a signature";
const HELP_REVEAL_STRING: &str = "Creates a proof for a signature";
const HELP_VERIFY_STRING: &str = "Verifies the proof";
const HELP_SHOWISSUER_STRING: &str = "Print the internal representation of the issuer";
const HELP_SHOWUSER_STRING: &str = "Print the internal representation of the user";
const HELP_SHOWCREDS_STRING: &str = "Print the internal representation of the credentials";

const HELP_STRING: &str = r#"
  Commands:
    help:
      Prints this message
    addissuer <issuer_name>:
      Creates an issuer which can be referred to by the name "issuer_name"
    adduser <issuer_name> <user_name>:
      Creates an user named "user_name" bound to "issuer_name"
    getcreds <user_name> [<attr_name>]*:
      Creates a signature
    reveal <user_name> [<bool>]*:
      Creates a proof for a signature
    verify <user_name> [<bool>]*:
      Verifies the proof
    showissuer <issuer_name>
      Print the internal representation of the issuer
    showuser <issuer_name>
      Print the internal representation of the user
    showcreds <issuer_name>
      Print the internal representation of the credentials

Example of use
  >>> addissuer bank0
  >>> adduser bank0 user0
  >>> getcreds user0 foo bar baz
  >>> reveal user0 t t t
  >>> verify user0 t t t
  etc...
"#;
lazy_static! {
  static ref COMMANDS: HashMap<&'static str, &'static str> = {
    let mut m = HashMap::new();
    m.insert("help", HELP_HELP_STRING);
    m.insert("addissuer", HELP_ADDISSUER_STRING);
    m.insert("adduser", HELP_ADDUSER_STRING);
    m.insert("sign", HELP_GETCREDS_STRING);
    m.insert("reveal", HELP_REVEAL_STRING);
    m.insert("verify", HELP_VERIFY_STRING);
    m.insert("showissuer", HELP_SHOWISSUER_STRING);
    m.insert("showuser", HELP_SHOWUSER_STRING);
    m.insert("showcreds", HELP_SHOWCREDS_STRING);
    m
  };
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Hash256([u8; DIGESTBYTES]);

// const ZERO_DIGEST: Hash256 = Hash256([0; DIGESTBYTES]);

impl std::fmt::Display for Hash256 {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", hex::encode(&self.0))
  }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Issuer {
  public_key: ACIssuerPublicKey,
  secret_key: ACIssuerSecretKey,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct User {
  public_key: ACUserPublicKey,
  secret_key: ACUserSecretKey,
}

// TypeName is used for hash salting
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

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Credential {
  cred: ACRevealSig,
  attrs: Vec<String>,
}

#[derive(Debug)]
struct GlobalState {
  verbose: bool,
  prng: ChaChaRng,
  registry: Vec<String>, // Not used anymore, used to represent file storage
  smt: SmtMap256<String>,
  users: HashMap<String, User>,
  issuers: HashMap<String, Issuer>,
  user_issuer: HashMap<String, Issuer>, // Each user has a single issuer
  user_cred: HashMap<String, Credential>, // Each user has at most a single credential issued to it
}

impl GlobalState {
  fn new(args: &ArgMatches /*<'static>*/) -> Self {
    GlobalState { verbose: args.is_present("v"),
                  prng: ChaChaRng::from_seed([0u8; 32]),
                  registry: Vec::<String>::new(),
                  smt: SmtMap256::<String>::new(),
                  users: HashMap::new(),
                  issuers: HashMap::new(),
                  user_issuer: HashMap::new(),
                  user_cred: HashMap::new() }
  }
}

fn hash_256(value: impl AsRef<[u8]>) -> Hash256 {
  Hash256(sha256::hash(value.as_ref()).0)
}

// Return the SHA256 hash of T as a hexadecimal string.
fn sha256<T>(key: &T) -> Hash256
  where T: Serialize + TypeName
{
  println!("sha256: hashing type: {}",
           key.type_string().to_string().blue());
  // Salt the hash to avoid leaking information about other uses of
  // sha256 on the user's public key.
  let mut bytes = key.type_string().to_string().into_bytes();
  // TODO: Verify that when we do serialize into a non-empty vector that we
  //       do NOT overwrite the existing vec data but rather push data onto it.
  key.serialize(&mut rmp_serde::Serializer::new(&mut bytes))
     .unwrap();
  hash_256(bytes)
}

// Test anonymous credentials on fixed inputs. Similar to
// Zei's credentials_tests.

fn test(global_state: &mut GlobalState) -> Result<(), String> {
  // Attributes to be revealed. For example, they might be:
  //    account balance, zip code, credit score, and timestamp
  // In this case, account balance (first) will not be revealed.
  let bitmap = [false, true, true, true];
  let attrs = [92_574_500u64.to_le_bytes(),
               95_050u64.to_le_bytes(),
               720u64.to_le_bytes(),
               20_190_820u64.to_le_bytes()];
  let att_count = bitmap.len();
  let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut global_state.prng, att_count);

  println!("Issuer public key: {:?}", issuer_pk);
  println!("Issuer secret key: {:?}", issuer_sk);

  let (user_pk, user_sk) = ac_keygen_user::<_>(&mut global_state.prng, &issuer_pk);
  println!("User public key: {:#?}", user_pk);
  println!("Address of user public key: {:?}", sha256(&user_pk));

  // The user secret key holds [u64; 6], but with more structure.
  println!("User secret key: {:?}", user_sk);

  // Issuer vouches for the user's attributes given above.
  let sig = ac_sign(&mut global_state.prng, &issuer_sk, &user_pk, &attrs[..]);
  println!("Credential signature: {:?}", sig);

  // The user presents this to the second party in a transaction as proof
  // attributes have been committed without revealing the values.
  let reveal_sig = ac_reveal(&mut global_state.prng,
                             &user_sk,
                             &issuer_pk,
                             &sig,
                             &attrs,
                             &bitmap).unwrap();

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
  if let Err(e) = ac_verify(&issuer_pk,
                            revealed_attrs.as_slice(),
                            &bitmap,
                            &reveal_sig.sig,
                            &reveal_sig.pok)
  {
    Err(format!("{}", e))
  } else {
    Ok(())
  }
}

// Generate a new issuer and append it to the registry.
fn add_issuer(mut global_state: &mut GlobalState, issuer_name: &str) -> Result<(), String> {
  // Generate a new issuer for anonymous credentials.
  fn new_issuer(global_state: &mut GlobalState) -> Issuer {
    let att_count = 10;
    let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_>(&mut global_state.prng, att_count);
    Issuer { public_key: issuer_pk,
             secret_key: issuer_sk }
  }
  if let Some(_) = global_state.issuers.get(issuer_name) {
    Err(format!("issuer named {} already exists", issuer_name))
  } else {
    let issuer = new_issuer(&mut global_state);
    println!("New issuer {}", issuer_name.yellow());
    global_state.issuers.insert(issuer_name.to_string(), issuer);
    Ok(())
  }
}

// Generate a new issuer and append it to the registry.
fn show_issuer(global_state: &mut GlobalState, issuer_name: &str) -> Result<(), String> {
  if let Some(issuer) = global_state.issuers.get(issuer_name) {
    println!("{} is : {}",
             issuer_name.yellow(),
             serde_json::to_string_pretty(&issuer).unwrap().magenta());
    Ok(())
  } else {
    Err(format!("issuer named {} not found", issuer_name))
  }
}

fn add_user(global_state: &mut GlobalState,
            issuer_name: &str,
            user_name: &str)
            -> Result<(), String> {
  if let Some(_) = global_state.users.get(user_name) {
    Err(format!("user named {} already exists", user_name))
  } else {
    // println!("Looking up issuer: {}", issuer_name);
    if let Some(issuer) = global_state.issuers.get(issuer_name) {
      let (user_pk, user_sk) = ac_keygen_user::<_>(&mut global_state.prng, &issuer.public_key);
      let user = User { public_key: user_pk,
                        secret_key: user_sk };
      println!("New user {} with issuer {}", user_name, issuer_name);
      global_state.users.insert(user_name.to_string(), user);
      global_state.user_issuer
                  .insert(user_name.to_string(), issuer.clone());
      Ok(())
    } else {
      Err(format!("lookup of issuer {} failed", issuer_name))
    }
  }
}

fn show_user(global_state: &mut GlobalState, user_name: &str) -> Result<(), String> {
  if let Some(user) = global_state.users.get(user_name) {
    println!("{} is : {}",
             user_name.yellow(),
             serde_json::to_string_pretty(&user).unwrap().magenta());
    Ok(())
  } else {
    Err(format!("user named {} not found", user_name))
  }
}

fn issue_credential(global_state: &mut GlobalState,
                    user: &str,
                    attrs: &Vec<String>)
                    -> Result<(), String> {
  match (global_state.users.get(user), global_state.user_issuer.get(user)) {
    (Some(user_keys), Some(issuer_keys)) => {
      let sig = ac_sign(&mut global_state.prng,
                        &issuer_keys.secret_key,
                        &user_keys.public_key,
                        &attrs);

      // User generates committment to cred by calling ac_reveal with no attributes
      let empty_attrs: Vec<String> = Vec::new();
      let empty_bitmap: Vec<bool> = Vec::new();
      if let Ok(proof) = ac_reveal(&mut global_state.prng,
                                   &user_keys.secret_key,
                                   &issuer_keys.public_key,
                                   &sig,
                                   &empty_attrs,
                                   &empty_bitmap)
      {
        // There should be three outputs from ac_reveal: a signature, a proof,
        // and the randomness used to generate the signature
        // let sig = &proof.sig;
        // let pok = &proof.pok;
        // let rnd = &proof.rnd;
        let sig_string = serde_json::to_string(&proof.sig).unwrap();
        let user_addr = sha256(&user_keys.public_key);
        // Insert an entry AIR[user_pk] = cred, where
        global_state.smt.set(&user_addr.0, Some(sig_string));
        let credential = Credential { cred: proof,
                                      attrs: attrs.to_vec() };
        println!("Credential issued to {}", user);
        global_state.user_cred.insert(user.to_string(), credential);
        Ok(())
      } else {
        Err("Credential generation fails during credential issuing process".to_string())
      }
    }
    (None, None) => Err("Unable to find either issuer or user".to_string()),
    (None, _) => Err("Unable to find user".to_string()),
    (_, None) => Err("Unable to find issuer".to_string()),
  }
}

fn show_credentials(global_state: &mut GlobalState, user_name: &str) -> Result<(), String> {
  if let Some(credential) = global_state.user_cred.get(&user_name.to_string()) {
    println!("{} is : {}",
             user_name.yellow(),
             serde_json::to_string_pretty(&credential).unwrap().magenta());
    Ok(())
  } else {
    Err(format!("user {} credential not found", user_name))
  }
}

fn reveal(global_state: &mut GlobalState, user: &str, bitmap: &Vec<bool>) -> Result<(), String> {
  match (global_state.users.get(user),
         global_state.user_issuer.get(user),
         global_state.user_cred.get(user))
  {
    (Some(user_keys), Some(issuer_keys), Some(cred)) => {
      if let Ok(_proof) = ac_reveal_with_rand(&mut global_state.prng,
                                              &user_keys.secret_key,
                                              &issuer_keys.public_key,
                                              &cred.cred.sig,
                                              &cred.attrs,
                                              &bitmap,
                                              cred.cred.rnd.clone())
      {
        // TODO Using the hash of the user's public key as the proof
        // address precludes multiple proofs

        let key = sha256(&user_keys.public_key).0;
        let (value, proof) = global_state.smt.get_with_proof(&key);
        if global_state.smt.check_merkle_proof(&key, value, &proof) {
          println!("ac_reveal_with_rand and smt merkle proof succeed");
          Ok(())
        } else {
          println!("ac_reveal_with_rand succeeds but smt merkle proof fails");
          Err("smt merkle proof failed".to_string())
        }
      } else {
        Err("ac_reveal_with_rand failed".to_string())
      }
    }
    (None, _, _) => Err("Unable to find user".to_string()),
    (_, None, _) => Err("Unable to find issuer".to_string()),
    (_, _, None) => Err("Unable to find signature".to_string()),
  }
}

fn verify(global_state: &mut GlobalState, user: &str, bitmap: &Vec<bool>) -> Result<(), String> {
  match (global_state.users.get(user),
         global_state.user_issuer.get(user),
         global_state.user_cred.get(user))
  {
    (Some(user_keys), Some(issuer_keys), Some(cred)) => {
      println!("Command verify user {} with attrs = {:?}, bitmap {:?}",
               user, &cred.attrs, bitmap);
      if let Ok(proof) = ac_reveal_with_rand(&mut global_state.prng,
                                             &user_keys.secret_key,
                                             &issuer_keys.public_key,
                                             &cred.cred.sig,
                                             &cred.attrs,
                                             &bitmap,
                                             cred.cred.rnd.clone())
      {
        if let Err(e) = ac_verify(&issuer_keys.public_key,
                                  &cred.attrs,
                                  &bitmap,
                                  &proof.sig,
                                  &proof.pok)
        {
          Err(format!("ac_verify fails with {}", e))
        } else {
          // Check merkle proof here?
          let key = sha256(&user_keys.public_key).0;
          let (value, proof) = global_state.smt.get_with_proof(&key);
          if global_state.smt.check_merkle_proof(&key, value, &proof) {
            Ok(())
          } else {
            println!("ac_verify succeeds but smt merkle proof fails");
            Err("smt merkle proof failed".to_string())
          }
        }
      } else {
        Err("ac_reveal_with_rand fails".to_string())
      }
    }
    (None, _, _) => Err("Unable to find user".to_string()),
    (_, None, _) => Err("Unable to find issuer".to_string()),
    (_, _, None) => Err("Unable to find signature".to_string()),
  }
}

fn issuer_exists(global_state: &GlobalState, issuer: &str) -> Result<(), String> {
  if let Some(_) = global_state.issuers.get(issuer) {
    Ok(())
  } else {
    Err(format!("{} is not a valid issuer name", issuer))
  }
}

fn user_exists(global_state: &GlobalState, user: &str) -> Result<(), String> {
  if let Some(_) = global_state.users.get(user) {
    Ok(())
  } else {
    Err(format!("{} is not a valid user name", user))
  }
}

fn parse_args() -> ArgMatches<'static> {
  App::new("Test REPL").version("0.1.0")
                       .author("Brian Rogoff <brian@findora.org>")
                       .about("REPL with argument parsing")
                       .arg(Arg::with_name("registry").short("r")
                                                      .long("registry")
                                                      .takes_value(true)
                                                      .help("the registry dude"))
                       .arg(Arg::with_name("file").short("f")
                                                  .takes_value(true)
                                                  .help("Name of the file"))
                       .arg(Arg::with_name("v").short("v")
                                               .help("Sets the level of verbosity"))
                       .get_matches()
}

fn str_to_bool(s: &str) -> bool {
  let c = s.chars().next().unwrap();
  c == 'T' || c == 't'
}

fn exec_line(mut global_state: &mut GlobalState, line: &str) -> Result<(), String> {
  match line.trim().split(' ').collect::<Vec<&str>>().as_slice() {
    ["help"] => {
      println!("{}", HELP_STRING.green());
      Ok(())
    }
    ["help", cmd] => {
      if let Some(s) = COMMANDS.get(cmd) {
        println!("{}", s.green());
        Ok(())
      } else {
        Err(format!("{} is not a command", cmd.red()))
      }
    }
    ["test"] => test(&mut global_state),
    ["addissuer", issuer] => add_issuer(&mut global_state, &issuer),
    ["adduser", issuer, user] => {
      issuer_exists(&global_state, &issuer)?;
      add_user(&mut global_state, &issuer, &user)
    }
    ["getcreds", user, attrs @ ..] => {
      user_exists(&global_state, &user)?;
      let attrs_vec: Vec<String> = attrs.to_vec()
                                        .into_iter()
                                        .map(|s: &str| -> String { s.to_string() })
                                        .collect();
      issue_credential(&mut global_state, &user, &attrs_vec)
    }
    ["reveal", user, bits @ ..] => {
      user_exists(&global_state, &user)?;
      let bitmap: Vec<bool> = bits.to_vec().into_iter().map(str_to_bool).collect();
      reveal(&mut global_state, &user, &bitmap)
    }
    ["verify", user, bits @ ..] => {
      user_exists(&global_state, &user)?;
      let bitmap: Vec<bool> = bits.to_vec().into_iter().map(str_to_bool).collect();
      verify(&mut global_state, &user, &bitmap)
    }
    ["showissuer", issuer] => show_issuer(&mut global_state, &issuer),
    ["showuser", user] => show_user(&mut global_state, &user),
    ["showcreds", user] => show_credentials(&mut global_state, &user),
    _ => Err(format!("Invalid line: {}", line.red())),
  }
}

fn main() -> Result<(), rustyline::error::ReadlineError> {
  let args = parse_args();
  let _registry_path = Path::new(args.value_of("registry").unwrap_or(DEFAULT_REGISTRY_PATH));

  let mut global_state = GlobalState::new(&args);

  // println!("The registry path is {}", _registry_path);

  // `()` can be used when no completer is required
  let mut rl = Editor::<()>::new();
  if rl.load_history("history.txt").is_err() {
    println!("No previous history.");
  }

  loop {
    let readline = rl.readline(">>> ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(line.as_str());
        if let Err(e) = exec_line(&mut global_state, &line) {
          println!("Error: {}", e.red());
        } else {
          println!("Success: {}", line.cyan());
        }
      }
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
        break;
      }
      Err(ReadlineError::Eof) => {
        println!("CTRL-D");
        break;
      }
      Err(err) => {
        println!("Error: {}", err);
        break;
      }
    }
  }
  rl.save_history("history.txt")
}
