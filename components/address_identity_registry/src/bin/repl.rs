#![allow(stable_features)]
#![feature(slice_patterns)]
//#![deny(warnings)]
// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials
#[macro_use]
extern crate lazy_static;

use air::AIR;
use clap::{App, Arg, ArgMatches};
use colored::*;
use credentials::{
  credential_commit, credential_issuer_key_gen, credential_open_commitment, credential_sign,
  credential_user_key_gen, credential_verify, CredCommitment, CredCommitmentKey,
  CredIssuerPublicKey, CredIssuerSecretKey, CredPoK, CredSignature, CredUserPublicKey,
  CredUserSecretKey, Credential,
};
use cryptohash::sha256;
use log::{info, warn};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use serde::{Deserialize, Serialize};
use sha256::DIGESTBYTES;
use std::collections::HashMap;
use std::path::Path;

// Default file path of the anonymous credential registry
const DEFAULT_REGISTRY_PATH: &str = "acreg.json";
const HELP_HELP_STRING: &str = "Prints a help message";
const HELP_ADDISSUER_STRING: &str =
  "Creates an issuer which can be referred to by the name \"issuer_name\"";
const HELP_ADDUSER_STRING: &str = "Creates an user named \"user_name\" bound to \"issuer_name\"";
const HELP_ISSUECREDENTIAL_STRING: &str = "Creates a an issuer signed signature";
const HELP_USERCOMMIT_STRING: &str = "User commits to a signed credential set";
const HELP_VERIFY_STRING: &str = "Verifies the proof";
const HELP_SHOWISSUER_STRING: &str = "Print the internal representation of the issuer";
const HELP_SHOWUSER_STRING: &str = "Print the internal representation of the user";
const HELP_SHOWCREDS_STRING: &str = "Print the internal representation of the credentials";

const HELP_STRING: &str = r#"
  Commands:
    help:
      Prints this message
    add_issuer <issuer_name>:
      Creates an issuer which can be referred to by the name "issuer_name"
    add_user <issuer_name> <user_name>:
      Creates an user named "user_name" bound to "issuer_name"
    issue_credential <user_name> [<attr_name>]*:
      Creates a signature
    user_commit <user_name>:
      Creates a proof for a signature
    verify <user_name> [<bool>]*:
      Verifies the proof
    show_issuer <issuer_name>
      Print the internal representation of the issuer
    show_user <issuer_name>
      Print the internal representation of the user
    show_creds <issuer_name>
      Print the internal representation of the credentials

Example of use
  >>> add_issuer DOS
  >>> add_user DOS brian
  >>> issue_credential DOS brian dob:08221964 ss:666666666
  >>> user_commit brian
  >>> verify brian dob:08221964 ""
  etc...
"#;
lazy_static! {
  static ref COMMANDS: HashMap<&'static str, &'static str> = {
    let mut m = HashMap::new();
    m.insert("help", HELP_HELP_STRING);
    m.insert("add_issuer", HELP_ADDISSUER_STRING);
    m.insert("add_user", HELP_ADDUSER_STRING);
    m.insert("issue_credential", HELP_ISSUECREDENTIAL_STRING);
    m.insert("user_commit", HELP_USERCOMMIT_STRING);
    m.insert("verify", HELP_VERIFY_STRING);
    m.insert("show_issuer", HELP_SHOWISSUER_STRING);
    m.insert("show_user", HELP_SHOWUSER_STRING);
    m.insert("show_creds", HELP_SHOWCREDS_STRING);
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
  name: String,
  public_key: CredIssuerPublicKey,
  secret_key: CredIssuerSecretKey,
  users: HashMap<String, User>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct User {
  issuer_name: String,
  issuer_pk: CredIssuerPublicKey,
  public_key: CredUserPublicKey,
  secret_key: CredUserSecretKey,
}

#[derive(Debug)]
struct GlobalState {
  verbose: bool,
  prng: ChaChaRng,
  registry: Vec<String>, // Not used anymore, used to represent file storage
  smt: AIR,
  users: HashMap<String, User>,
  issuers: HashMap<String, Issuer>,
  user_sig: HashMap<String, CredSignature>, // Each user has at most a single credential issued to it
  user_attrs: HashMap<String, Vec<(String, String)>>, // Each user has at most a single credential issued to it
  user_commitment: HashMap<String, (CredCommitment, CredCommitmentKey)>, // Each user has at most a single credential issued to it
}

impl GlobalState {
  fn new(args: &ArgMatches /*<'static>*/) -> Self {
    GlobalState { verbose: args.is_present("v"),
                  prng: ChaChaRng::from_entropy(),
                  registry: Vec::<String>::new(),
                  smt: AIR::default(),
                  users: HashMap::new(),
                  issuers: HashMap::new(),
                  user_sig: HashMap::new(),
                  user_attrs: HashMap::new(),
                  user_commitment: HashMap::new() }
  }
}

// Generate a new issuer and append it to the registry.
fn add_issuer(mut global_state: &mut GlobalState, issuer_name: &str) -> Result<(), String> {
  // Generate a new issuer for anonymous credentials.
  fn new_issuer(global_state: &mut GlobalState, issuer_name: &str) -> Issuer {
    let attrs = [(String::from("ssn"), 4),
                 (String::from("dob"), 4),
                 (String::from("photo"), 8),
                 (String::from("dl"), 10)];
    let (issuer_pk, issuer_sk) = credential_issuer_key_gen(&mut global_state.prng, &attrs);
    Issuer { name: issuer_name.to_string(),
             public_key: issuer_pk,
             secret_key: issuer_sk,
             users: HashMap::new() }
  }
  if global_state.issuers.get(issuer_name).is_some() {
    Err(format!("issuer named {} already exists", issuer_name))
  } else {
    let issuer = new_issuer(&mut global_state, issuer_name);
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
  if global_state.users.get(user_name).is_some() {
    Err(format!("user named {} already exists", user_name))
  } else {
    // println!("Looking up issuer: {}", issuer_name);
    if let Some(issuer) = global_state.issuers.get(issuer_name) {
      let (user_pk, user_sk) = credential_user_key_gen(&mut global_state.prng, &issuer.public_key);
      let user = User { issuer_name: issuer_name.to_string(),
                        issuer_pk: issuer.public_key.clone(),
                        public_key: user_pk,
                        secret_key: user_sk };
      println!("New user {} with issuer {}", user_name, issuer_name);
      global_state.users.insert(user_name.to_string(), user);
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

// issue_credential is performed by the Issuer and has access to issuer secret key
fn issue_credential(global_state: &mut GlobalState,
                    issuer: &str,
                    user: &str,
                    attrs: &[(String, String)])
                    -> Result<(), String> {
  match (global_state.issuers.get(issuer), global_state.users.get(user)) {
    (Some(issuer_keys), Some(user_keys)) => {
      let attr_as_slice: Vec<(String, &[u8])> = attrs.iter()
                                                     .map(|(k, v)| (k.clone(), v.as_bytes()))
                                                     .collect();
      let sig = credential_sign(&mut global_state.prng,
                                &issuer_keys.secret_key,
                                &user_keys.public_key,
                                &attr_as_slice).unwrap();
      println!("Issuer {}: credential issued to {}", issuer, user);
      global_state.user_sig.insert(user.to_string(), sig);
      global_state.user_attrs
                  .insert(user.to_string(), attrs.to_vec());
      Ok(())
    }
    (None, None) => Err("Unable to find either issuer or user".to_string()),
    (None, _) => Err("Unable to find issuer".to_string()),
    (_, None) => Err("Unable to find user".to_string()),
  }
}

// user_commit is performed by the User and has access to user secret key
fn user_commit(global_state: &mut GlobalState, user: &str) -> Result<(), String> {
  match (global_state.users.get(user), global_state.user_sig.get(user)) {
    (Some(user_struct), Some(sig)) => {
      let a = global_state.user_attrs.get(user).unwrap();
      let map: Vec<(String, Vec<u8>)> = a.iter()
                                         .map(|(k, v)| (k.clone(), v.as_bytes().to_vec()))
                                         .collect();

      // println!("User {}: about to commit to attrs = {:?}", user, &attrs);
      let credential = Credential { signature: sig.clone(),
                                    attributes: map,
                                    issuer_pub_key: user_struct.issuer_pk.clone() };

      if let Ok((commitment, _proof, key)) = credential_commit(&mut global_state.prng,
                                                               &user_struct.secret_key,
                                                               &credential,
                                                               b"random message")
      {
        // There should be three outputs from ac_commit: a commitment, a proof, and a key
        // The User generates a commitment to the signed attribute values using **ac_commit** and stores both the commitment
        // and the *commitment key* used to generate the it, in a User wallet. The User generates a unique address (how?), and
        // asks the Ledger to store the commitment in the AIR at that address.
        let commitment_string = serde_json::to_string(&commitment).unwrap();
        let user_addr = serde_json::to_string(&user_struct.public_key).unwrap();
        // println!("User {}: commitment = {}", user, &commitment_string);
        // Insert an entry AIR[user_pk] = cred, where
        info!("user_commit: {:?}", &commitment);
        info!("user_commit: as string = {}", &commitment_string);
        global_state.smt.set(&user_addr, Some(commitment_string));
        global_state.user_commitment
                    .insert(user.to_string(), (commitment, key));
        Ok(())
      } else {
        Err("User unable to commit".to_string())
      }
    }
    (None, None) => Err("Unable to find either issuer or user".to_string()),
    (None, _) => Err("Unable to find user".to_string()),
    (_, None) => Err("Unable to find issuer".to_string()),
  }
}

fn show_creds(global_state: &mut GlobalState, user_name: &str) -> Result<(), String> {
  if let Some(sig) = global_state.user_sig.get(&user_name.to_string()) {
    println!("{} is : {}",
             user_name.yellow(),
             serde_json::to_string_pretty(&sig).unwrap().magenta());
    Ok(())
  } else {
    Err(format!("user {} credential not found", user_name))
  }
}

// user_selectively_reveal is run inside a User
fn user_selectively_reveal(global_state: &mut GlobalState,
                           user: &str,
                           reveal_fields: &[String])
                           -> Result<(String, CredPoK), String> {
  match (global_state.users.get(user),
         global_state.user_commitment.get(user),
         global_state.user_sig.get(user))
  {
    (Some(user_struct), Some((_commitment, key)), Some(sig)) => {
      // println!("user_selectively_reveal: commitment = {:?}", &commitment);
      let attrs = global_state.user_attrs.get(user).unwrap();
      // let attributes = attrs.to_vec(); //Vec<ACAttribute<String>> = attrs.to_vec().into_iter().map(string_to_attr).collect();
      let map: Vec<(String, Vec<u8>)> = attrs.iter()
                                             .map(|(k, v)| (k.clone(), v.as_bytes().to_vec()))
                                             .collect();
      let credential = Credential { signature: sig.clone(),
                                    attributes: map,
                                    issuer_pub_key: user_struct.issuer_pk.clone() };
      if let Ok(pok) = credential_open_commitment(&mut global_state.prng,
                                                  &user_struct.secret_key,
                                                  &credential,
                                                  &key,
                                                  reveal_fields)
      {
        let address = serde_json::to_string(&user_struct.public_key).unwrap();
        Ok((address, pok))
      } else {
        Err("user_selectively_reveal: open commitment fails".to_string())
      }
    }
    (None, None, None) => Err("Unable to find either user or commitment".to_string()),
    (None, _, _) => Err("Unable to find user".to_string()),
    (_, None, _) => Err("Unable to find commitment".to_string()),
    (_, _, None) => Err("Unable to find signature".to_string()),
  }
}
/* Never used
fn slice_to_attr<'a>(attr: (&String, &'a[u8])) -> Option<(String,&'a[u8])> {
  if attr.1 == b"" || attr.1 == b"\"\"" {
    None
  } else {
    Some((attr.0.clone(), attr.1))
  }
}
*/
// Verify is run by the verifier
fn verify(global_state: &mut GlobalState,
          user: &str,
          attrs: &[(String, String)])
          -> Result<(), String> {
  let mut reveal_fields = vec![];
  for (field, _attr) in attrs {
    reveal_fields.push(field.clone());
  }
  // println!("verify: bitmap = {:?}", &bitmap);
  let (address, pok) = user_selectively_reveal(global_state, user, &reveal_fields)?;
  let (commitment_opt, merkle_proof) = global_state.smt.get_with_proof(address.clone());
  if let Some(commitment_string) = commitment_opt {
    let commitment = serde_json::from_str(&commitment_string[..]).unwrap();
    info!("verify: as string = {}", &commitment_string);
    info!("verify: {:?}", &commitment);
    // println!("verify with attributes = {:?}", &attributes);
    let attrs_vec: Vec<(String, &[u8])> = attrs.iter()
                                               .map(|(k, v)| (k.clone(), v.as_bytes()))
                                               .collect();
    if global_state.smt
                   .check_merkle_proof(&address, commitment_opt, &merkle_proof)
    {
      let user_info = global_state.users.get(user).unwrap();
      if let Err(e) = credential_verify(&user_info.issuer_pk, &attrs_vec[..], &commitment, &pok) {
        Err(format!("smt merkle proof succeeds but ac_verify fails with {}", e))
      } else {
        println!("smt merkle proof and ac_verify succeed");
        Ok(())
      }
    } else {
      Err("smt merkle proof failed".to_string())
    }
  } else {
    Err(format!("Nothing stored in AIR at address {}", &address))
  }
}

fn issuer_exists(global_state: &GlobalState, issuer: &str) -> Result<(), String> {
  if global_state.issuers.get(issuer).is_some() {
    Ok(())
  } else {
    Err(format!("{} is not a valid issuer name", issuer))
  }
}

fn user_exists(global_state: &GlobalState, user: &str) -> Result<(), String> {
  if global_state.users.get(user).is_some() {
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

fn split2(s: &str) -> Option<(String, String)> {
  match s.split(':').collect::<Vec<&str>>().as_slice() {
    [s1, s2] => Some((String::from(*s1), String::from(*s2))),
    _ => None,
  }
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
    ["add_issuer", issuer] => add_issuer(&mut global_state, &issuer),
    ["add_user", issuer, user] => {
      issuer_exists(&global_state, &issuer)?;
      add_user(&mut global_state, &issuer, &user)
    }
    ["issue_credential", issuer, user, attrs @ ..] => {
      issuer_exists(&global_state, &issuer)?;
      user_exists(&global_state, &user)?;
      let attrs_vec: Vec<(String, String)> = attrs.to_vec()
                                                  .into_iter()
                                                  .map(|s| split2(s).unwrap())
                                                  .collect();
      issue_credential(&mut global_state, &issuer, &user, &attrs_vec)
    }
    ["user_commit", user] => {
      user_exists(&global_state, &user)?;
      user_commit(&mut global_state, user)
    }
    ["verify", user, attrs @ ..] => {
      user_exists(&global_state, &user)?;
      let attrs_vec: Vec<(String, String)> = attrs.to_vec()
                                                  .into_iter()
                                                  .map(|s| split2(s).unwrap())
                                                  .collect();
      // let bitmap: Vec<bool> = bits.to_vec().into_iter().map(str_to_bool).collect();
      verify(&mut global_state, &user, &attrs_vec)
    }
    ["show_issuer", issuer] => show_issuer(&mut global_state, &issuer),
    ["show_user", user] => show_user(&mut global_state, &user),
    ["show_creds", user] => show_creds(&mut global_state, &user),
    _ => Err(format!("Invalid line: {}", line.red())),
  }
}

fn main() -> Result<(), rustyline::error::ReadlineError> {
  env_logger::init();

  let args = parse_args();
  let _registry_path = Path::new(args.value_of("registry").unwrap_or(DEFAULT_REGISTRY_PATH));

  let mut global_state = GlobalState::new(&args);

  info!("The registry path is {}", _registry_path.display());

  // `()` can be used when no completer is required
  let mut rl = Editor::<()>::new();
  if rl.load_history("history.txt").is_err() {
    warn!("No previous history.");
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

#[cfg(test)]
mod tests {
  #![allow(dead_code)]
  use credentials::{
    credential_commit, credential_issuer_key_gen, credential_open_commitment, credential_sign,
    credential_user_key_gen, credential_verify, Credential,
  };
  use rand_chacha::ChaChaRng;
  use rand_core::SeedableRng;

  #[test]

  // test_ac_functions tests the expected order of operations of the AIR as described in the README
  fn test_ac_functions() {
    let mut prng = ChaChaRng::from_entropy();
    let dob = String::from("dob");
    let ss = String::from("ss");
    let dob = String::from("dob");
    let dl = String::from("dl");
    let attrs_temp = [(dob.clone(), 8), (ss.clone(), 9), (dl.clone(), 8)]; // 4
    let (issuer_pk, issuer_sk) = credential_issuer_key_gen::<ChaChaRng>(&mut prng, &attrs_temp);
    let (user_pk, user_sk) = credential_user_key_gen::<ChaChaRng>(&mut prng, &issuer_pk);
    let dob_attr = b"08221964";
    let ss_attr = b"666666666";
    let dl_attr = b"A1903479";
    let attributes = [(dob.clone(), &dob_attr[..]),
                      (ss.clone(), &ss_attr[..]),
                      (dl.clone(), &dl_attr[..])];
    println!("verify: attributes = {:?}", &attributes);
    let signature = credential_sign(&mut prng, &issuer_sk, &user_pk, &attributes).unwrap(); // Done by Issuer

    // Enter the User
    let mut attr_map = vec![];
    attr_map.push((dob.clone(), dob_attr.to_vec()));
    attr_map.push((ss.clone(), ss_attr.to_vec()));
    attr_map.push((dl.clone(), dl_attr.to_vec()));
    let credential = Credential { signature: signature.clone(),
                                  attributes: attr_map,
                                  issuer_pub_key: issuer_pk.clone() };
    let (commitment, _proof, key) =
      credential_commit(&mut prng, &user_sk, &credential, b"some addr").unwrap();
    println!("verify: commitment  = {:?}, key = {:?}", &commitment, &key);
    let commitment_string = serde_json::to_string(&commitment).unwrap();
    let commitment2 = serde_json::from_str(&commitment_string[..]).unwrap();
    println!("verify: commitment2 = {:?}, key = {:?}", &commitment, &key);

    if commitment == commitment2 {
      println!("commitments unchanged by serde");
    } else {
      println!("WTF commitments changed by serde");
    }
    // Inside user store commitment in AIR, commitment and key in user "wallet"

    // Next the Verifier wants to verify dob (the first attr) and asks the User to
    // reveal the first attribute and provide a proof

    // Enter the User with Verifier specified bitmap
    let bitmap = [String::from("dob")];
    let reveal_pok =
      credential_open_commitment(&mut prng, &user_sk, &credential, &key, &bitmap).unwrap();

    // Enter the Verifier with user generated POK (and address in the AIR tests)
    let attr_map = [(dob.clone(), &dob_attr[..])];
    // println!("verify: attr_map = {:?}", &attr_map);
    let result_verification_ok = credential_verify(&issuer_pk, &attr_map, &commitment, &reveal_pok);
    assert!(result_verification_ok.is_ok());
    let attr_map = [(ss.clone(), &ss_attr[..])];
    let result_verification_err =
      credential_verify(&issuer_pk, &attr_map, &commitment, &reveal_pok);
    assert!(result_verification_err.is_err());

    let attr_map = [(ss.clone(), &dob_attr[..])];
    let result_verification_err =
      credential_verify(&issuer_pk, &attr_map, &commitment, &reveal_pok);
    assert!(result_verification_err.is_err());
  }
}
