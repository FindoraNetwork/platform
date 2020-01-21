#![deny(warnings)]
// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials
// Anonymous Credentials with Selective Attribute Revelation

// I'm not certain the use case below is the one intended.
//
// 1. The (credential) issuer generates a key pair with knowledge of
//    the number of attributes. The number of attributes is either set by
//    the nature of the deal to be made (e.g. a standard form) or the
//    user's choice.
// 2. The user generates keys using the issuer's public key.
// 3. The user supplies attribute values and their newly generated public
//    key to the issuer. Or maybe the issuer already knows the attribute
//    values?
// 4. The issuer signs the attributes with the issuer's secret key
//    and the user's public key. This is the issuer's attestation that
//    the user has committed to the attribute values.
// 5. The user presents to the prover (second party) the issuer
//    attestation signature as proof the user has committed to certain
//    attributes.
// 6. Possibly immediately, possibly later, the user generates and presents a
//    "reveal signature" that makes it possible to prove the user committed
//    to the to-be revealed values without revealing the values.
// 8. The user reveals the attribute values to the prover.
// ?. Something happens to commit the prover to something?
// 9. Finally, the prover has sufficient information to prove the user
//    did in fact commit to the attributes as revealed.
//
// Trust Issues
//
// The issuer (specifically, the credential issuer) must be trusted
// regarding their standing and their discretion regarding keeping
// the attribute values confidential. How does the issuer know the
// user isn't lying about the attribute values? Maybe the issuer would
// only participate under certain conditions.
//
// The user must trust the issuer not to prematurely reveal the
// attributes or alter the attributes. Maybe the credential issuer has
// limited knowledge of the account requesting attestation or the
// pending transaction? If there were a small number of bidders
// perhaps the second party could infer something. Maybe the issuer
// wouldn't know with whom to conspire?
//
// How does the prover know these attributes are germane? Could the user
// game the system by getting the issuer to sign one set of attributes
// and later claim they were another set of attributes, i.e. changing
// the column headings?
//
// The only thing the prover knows is that the values were committed.
// The prover must also trust the issuer. Maybe reputation give the
// issuer incentive to be trustworthy. This is all that prevents the
// user from lying about the attribute values.

#[macro_use]
extern crate clap;

use env_logger::{Env, Target};
use log::{debug, error, info, trace, warn};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;

use hex;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rmp_serde;
use sha2::{Digest, Sha256};
use zei::api::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify,
  ACIssuerPublicKey, ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
//const AUTHOR: &str = "John D. Corbett <corbett@findora.org>";
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

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

fn init_logging() {
  // Log everything "trace" level or greater to stdout.
  // TODO document how to override this from an environment variable.
  env_logger::from_env(Env::default().default_filter_or("trace")).target(Target::Stdout)
                                                                 .init();
}

// Demonstrate logging format including timestamp and color-coded
// keywords
fn demo_logging() {
  error!("Sample error message");
  warn!("Sample warn message");
  info!("Sample info message");
  debug!("Sample debug message");
  trace!("Sample trace message");
}

fn parse_args() -> clap::ArgMatches<'static> {
  let path: std::path::PathBuf = std::env::current_exe().unwrap();
  let program_name: &str = path.file_name().unwrap().to_str().unwrap();
  clap_app!(tmp_name =>
    (name: program_name)
    (version: VERSION)
    (author: AUTHOR)
    (about: "Anonomyous credential registry command line interface")
    (@arg registry: -r --registry [FILE]
     "registry path (default: acreg.json)")
    (@arg debug: -d ... "Sets the level of debugging information")
    (@subcommand test => (about: "Automated self-test"))

    (@subcommand addissuer => (about: "Add a new anonymous credential issuer"))
    (@subcommand adduser =>
     (about: "Add a new anonymous credential user")
     (@arg issuer: +required "anonymous credential issuer"))
    (@subcommand sign =>
     (about: "Create an anonymous credential for user by issuer")
     (@arg issuer: +required "anonymous credential issuer")
     (@arg user: +required "user address"))
    (@subcommand reveal =>
     (about: "Generate a proof of an anonymous credential for user by issuer")
     (@arg issuer: +required "anonymous credential issuer")
     (@arg user: +required "user address"))
    (@subcommand verify =>
     (about: "Verify an anonymous credential for user by issuer")
     (@arg issuer: +required "anonymous credential issuer")
     (@arg user: +required "user address"))
  ).get_matches()
}

// Test anonymous credentials on fixed inputs. Similar to
// Zei's credentials_tests.
fn automated_test() -> bool {
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

  trace!("Issuer public key: {:?}", issuer_pk);
  trace!("Issuer secret key: {:?}", issuer_sk);

  let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer_pk);
  trace!("User public key: {:#?}", user_pk);
  info!("Address of user public key: {:?}", sha256(&user_pk));

  // The user secret key holds [u64; 6], but with more structure.
  trace!("User secret key: {:?}", user_sk);

  // Issuer vouches for the user's attributes given above.
  let sig = ac_sign(&mut prng, &issuer_sk, &user_pk, &attrs[..]);
  trace!("Credential signature: {:?}", sig);

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
  let verified = ac_verify(&issuer_pk, revealed_attrs.as_slice(), &bitmap, &reveal_sig).is_ok();
  if verified {
    info!("Verified revealed attributes match signed commitment.");
  } else {
    error!("Verification failed.");
  };
  verified
}

enum ShellExitStatus {
  Success = 0,
  Failure = 1,
}

fn subcommand_test(registry_path: &Path, args: &clap::ArgMatches) -> ShellExitStatus {
  trace!("subcommand test");
  if 0 < args.occurrences_of("debug") {
    demo_logging();
    trace!("registry: {}", registry_path.display());
  }
  if automated_test() {
    ShellExitStatus::Success
  } else {
    ShellExitStatus::Failure
  }
}

// Return the SHA256 hash of T as a hexadecimal string.
fn sha256<T>(key: &T) -> String
  where T: Serialize + TypeName
{
  trace!("ipk type: {}", key.type_string());
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

// Generate a new issuer and append it to the registry.
fn subcommand_add_issuer(registry_path: &Path) -> ShellExitStatus {
  trace!("subcommand addissuer");
  let issuer = new_issuer();
  let address = sha256(&issuer.public_key);
  trace!("Issuer address: {}", &address);

  match OpenOptions::new().append(true)
                          .create(true)
                          .open(&registry_path)
  {
    Err(io_error) => {
      error!("Couldn't create registry {}: {}",
             &registry_path.display(),
             io_error.to_string());
      ShellExitStatus::Failure
    }
    Ok(mut registry_file) => {
      let a = AddrIssuer { address,
                           value_type: 2,
                           value: issuer };
      let j = serde_json::to_string(&a).unwrap();
      trace!("json: {}", j);
      if let Err(e) = registry_file.write_fmt(format_args!("{}\n", j)) {
        error!("Error: {:?}", e);
        ShellExitStatus::Failure
      } else {
        ShellExitStatus::Success
      }
    }
  }
}

type Signature = ACSignature;

#[derive(Debug, Serialize, Deserialize)]
struct AddrValue<V> {
  address: String,
  value_type: i32,
  value: V,
}

// Generic key-value lookup and deserialization
fn lookup<T>(registry_path: &Path, address: &str) -> Option<T>
  where for<'d> T: Deserialize<'d>
{
  match File::open(registry_path) {
    Ok(mut registry_file) => {
      let mut contents = String::new();
      if registry_file.read_to_string(&mut contents).is_err() {
        return None;
      }
      let mut acjson = contents.lines();
      acjson.find_map(|x| match serde_json::from_str::<AddrValue<T>>(x) {
              Ok(item) => {
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
    Err(open_error) => {
      error!("{:?}", open_error);
      None
    }
  }
}

fn append_user(registry_path: &Path, user: AddrUser) -> bool {
  match OpenOptions::new().append(true)
                          .create(true)
                          .open(&registry_path)
  {
    Err(io_error) => {
      error!("Couldn't create registry {}: {}",
             &registry_path.display(),
             io_error.to_string());
      false
    }
    Ok(mut registry_file) => {
      let user_json = serde_json::to_string(&user).unwrap();
      if let Err(e) = registry_file.write_fmt(format_args!("{}\n", user_json)) {
        error!("Error: {:?}", e);
        false
      } else {
        true
      }
    }
  }
}

fn subcommand_add_user(registry_path: &Path, issuer: &str) -> ShellExitStatus {
  if let Some(issuer) = lookup::<Issuer>(registry_path, issuer) {
    let mut prng: ChaChaRng;
    // For a real application, the seed should be random.
    prng = ChaChaRng::from_seed([0u8; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_>(&mut prng, &issuer.public_key);
    let au = AddrUser { address: sha256(&user_pk),
                        value_type: 2,
                        value: User { public_key: user_pk,
                                      secret_key: user_sk } };
    info!("Added user: {}", au.address);
    if append_user(registry_path, au) {
      ShellExitStatus::Success
    } else {
      ShellExitStatus::Failure
    }
  } else {
    error!("Unable to locate issuer");
    ShellExitStatus::Failure
  }
}

#[derive(Debug, Serialize, Deserialize)]
struct AddrSig {
  address: String,
  value_type: u32,
  value: ACSignature,
}

fn subcommand_sign(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  match (lookup::<User>(registry_path, user), lookup::<Issuer>(registry_path, issuer)) {
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
      // TODO extract a generic function to append a record to the registry
      match OpenOptions::new().append(true).open(&registry_path) {
        Err(io_error) => {
          error!("Couldn't write to registry {}: {}",
                 &registry_path.display(),
                 io_error.to_string());
          ShellExitStatus::Failure
        }
        Ok(mut registry_file) => {
          let sig_json = serde_json::to_string(&addr_sig).unwrap();
          if let Err(io_error) = registry_file.write_fmt(format_args!("{}\n", sig_json)) {
            error!("Couldn't append to registry {}: {}",
                   registry_path.display(),
                   io_error.to_string());
            ShellExitStatus::Failure
          } else {
            info!("Signature");
            ShellExitStatus::Success
          }
        }
      }
    }
    (Some(_), _) => {
      error!("Unable to find user");
      ShellExitStatus::Failure
    }
    (_, Some(_)) => {
      error!("Unable to find issuer");
      ShellExitStatus::Failure
    }
    (_, _) => {
      error!("Unable to find either issuer or user");
      ShellExitStatus::Failure
    }
  }
}

type Proof = ACRevealSig;

#[derive(Debug, Serialize, Deserialize)]
struct AddrProof {
  address: String,
  value_type: u32,
  value: Proof,
}

fn subcommand_reveal(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  trace!("subcommand reveal user: {} issuer: {}", user, issuer);

  match (lookup::<User>(registry_path, user),
         lookup::<Issuer>(registry_path, issuer),
         lookup::<Signature>(registry_path, user))
  {
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
      // TODO extract a generic function to append a record to the registry
      match OpenOptions::new().append(true).open(&registry_path) {
        Err(io_error) => {
          error!("Couldn't write to registry {}: {}",
                 &registry_path.display(),
                 io_error.to_string());
          ShellExitStatus::Failure
        }
        Ok(mut registry_file) => {
          let sig_json = serde_json::to_string(&addr_proof).unwrap();
          if let Err(io_error) = registry_file.write_fmt(format_args!("{}\n", sig_json)) {
            error!("Couldn't append to registry {}: {}",
                   registry_path.display(),
                   io_error.to_string());
            ShellExitStatus::Failure
          } else {
            info!("Signature");
            ShellExitStatus::Success
          }
        }
      }
    }
    (some_user, some_issuer, some_sig) => {
      if some_user.is_none() {
        error!("Unable to find user.");
      };
      if some_issuer.is_none() {
        error!("Unable to find issuer.");
      };
      if some_sig.is_none() {
        error!("Unable to find signature.");
      };
      ShellExitStatus::Failure
    }
  }
}

fn subcommand_verify(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  trace!("subcommand verify user: {} issuer: {}", user, issuer);
  let verified;
  match (lookup::<Issuer>(registry_path, issuer), lookup::<Proof>(registry_path, user)) {
    (Some(issuer_keys), Some(proof)) => {
      let attrs = [0u64.to_le_bytes(),
                   1u64.to_le_bytes(),
                   2u64.to_le_bytes(),
                   3u64.to_le_bytes()];
      let bitmap = [false, false, false, false];
      verified = ac_verify(&issuer_keys.public_key, &attrs, &bitmap, &proof).is_ok();
    }
    (lookup_issuer, lookup_proof) => {
      if lookup_issuer.is_none() {
        error!("Unable to find issuer: {}", issuer);
      }
      if lookup_proof.is_none() {
        error!("Unable to find proof");
      }
      verified = false;
    }
  }
  if verified {
    info!("Verified revealed attributes match signed commitment.");
    ShellExitStatus::Success
  } else {
    error!("Verification failed.");
    ShellExitStatus::Failure
  }
}

fn main() {
  init_logging();
  let args = parse_args();
  let registry_path = Path::new(args.value_of("registry").unwrap_or(DEFAULT_REGISTRY_PATH));
  exit(match args.subcommand() {
         ("test", _) => subcommand_test(&registry_path, &args),

         ("addissuer", _) => subcommand_add_issuer(&registry_path),
         ("adduser", Some(matches)) => {
           let issuer = matches.value_of("issuer").unwrap();
           subcommand_add_user(&registry_path, &issuer)
         }
         ("sign", Some(matches)) => {
           let user = matches.value_of("user").unwrap();
           let issuer = matches.value_of("issuer").unwrap();
           subcommand_sign(&registry_path, &user, &issuer)
         }
         ("reveal", Some(matches)) => {
           let user = matches.value_of("user").unwrap();
           let issuer = matches.value_of("issuer").unwrap();
           subcommand_reveal(&registry_path, &user, &issuer)
         }
         ("verify", Some(matches)) => {
           let user = matches.value_of("user").unwrap();
           let issuer = matches.value_of("issuer").unwrap();
           subcommand_verify(&registry_path, &user, &issuer)
         }

         (subcommand, _) => {
           error!("Please specify a valid subcommand: {:?}. Use -h for help.",
                  subcommand);
           ShellExitStatus::Failure
         }
       } as i32)
}

/*
pub fn verify_credential(issuer_pub_key: &ACIssuerPublicKey<P::G1, P::G2>,
                         attrs: &[P::ScalarField],
                         bitmap: &[bool],
                         credential: &ACSignature<P::G1>)
                         -> Result<(), ZeiError> {
}
*/
