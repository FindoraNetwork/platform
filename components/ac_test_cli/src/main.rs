// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials
// Anonymous Credentials with Selective Attribute Revelation
//
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
// regarding their standing and their discression regarding keeping
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
use std::error::Error;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;

use hex;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use rmp_serde;
use sha2::{Digest, Sha256};
use zei::algebra::bls12_381::{BLSGt, BLSScalar, BLSG1, BLSG2};

use zei::algebra::groups::Scalar;
use zei::crypto::anon_creds::{
  ac_keygen_issuer, ac_keygen_user, ac_reveal, ac_sign, ac_verify, ACIssuerPublicKey,
  ACIssuerSecretKey, ACRevealSig, ACSignature, ACUserPublicKey, ACUserSecretKey,
};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = "John D. Corbett <corbett@findora.org>";

// Default file path of the anonymous credential registry
const DEFAULT_REGISTRY_PATH: &str = "acreg.json";

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

// TODO Change the commands to distinguish agents: issuer, user (prover), and verifier.
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
    (@subcommand create => (about: "Create a new anonymous credential"))

    (@subcommand addissuer => (about: "Add a new anonymous credential issuer"))
    (@subcommand adduser =>
     (about: "Add a new anonymous credential user")
     (@arg issuer: +required "anonymous credential issuer"))
    (@subcommand sign =>
     (about: "Create an anonymous credential for user by issuer")
     (@arg issuer: +required "anonymous credential issuer")
     (@arg user: +required "user address"))
    (@subcommand lookup =>
     (about: "Lookup anonymous credential")
     (@arg user: +required "anonymous credential address"))
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
  let attrs = [BLSScalar::from_u64(92574500),
               BLSScalar::from_u64(95050),
               BLSScalar::from_u64(720),
               BLSScalar::from_u64(20190820)];
  let att_count = bitmap.len();
  let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_, BLSScalar, BLSGt>(&mut prng, att_count);
  trace!("Issuer public key: {:?}", issuer_pk);
  trace!("Issuer secret key: {:?}", issuer_sk);

  let (user_pk, user_sk) = ac_keygen_user::<_, BLSScalar, BLSGt>(&mut prng, &issuer_pk);
  trace!("User public key: {:#?}", user_pk);
  info!("Address of user public key: {:?}", upk_sha256(&user_pk));

  // The user secret key holds [u64; 6], but with more structure.
  trace!("User secret key: {:?}", user_sk);

  // Issuer vouches for the user's attributes given above.
  let sig = ac_sign::<_, BLSScalar, BLSGt>(&mut prng, &issuer_sk, &user_pk, &attrs);
  trace!("Credential signature: {:?}", sig);

  // The user presents this to the second party in a transaction as proof
  // attributes have been committed without revealing the values.
  let reveal_sig = ac_reveal::<_, BLSScalar, BLSGt>(&mut prng, &user_sk, &issuer_pk, &sig, &attrs,
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
  let verified = ac_verify::<BLSScalar, BLSGt>(&issuer_pk,
                                               revealed_attrs.as_slice(),
                                               &bitmap,
                                               &reveal_sig).is_ok();
  if verified {
    info!("Verified revealed attributes match signed commitment.");
  } else {
    error!("Verification failed.");
  };
  verified
}

// TODO change this type to hold an actual signature, not a dummy string
// Record mapping an address to an anonymous credential signature
#[derive(Debug, Serialize, Deserialize)]
struct AddrCred<'a> {
  address: &'a str,
  sig_type: u32,
  signature: &'a str,
}

impl Default for AddrCred<'_> {
  fn default() -> Self {
    AddrCred { address: "",
               sig_type: 0,
               signature: "" }
  }
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

// TODO create command should create a random entry, not a dummy
fn subcommand_create(registry_path: &Path) -> ShellExitStatus {
  trace!("subcommand create");
  trace!("Creating and appending a record to {}",
         registry_path.display());
  match OpenOptions::new().append(true)
                          .create(true)
                          .open(&registry_path)
  {
    Err(io_error) => {
      error!("Couldn't create registry {}: {}",
             &registry_path.display(),
             io_error.description());
      ShellExitStatus::Failure
    }
    Ok(mut registry_file) => {
      // TODO The address and signature should come from Zei.
      let address = "0123";
      let signature = "0123456";
      let a = AddrCred { address: address,
                         sig_type: 2,
                         signature: signature };
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

fn subcommand_lookup(registry_path: &Path, address: &str) -> ShellExitStatus {
  trace!("subcommand lookup");
  // Maximum u64 is 20 digits: 18,446,744,073,709,551,616.
  // TODO if we do string comparison, trim leading zeros.
  //      if let Ok(address) = lookup.value_of("address").unwrap().parse::<u64>() {
  trace!("lookup: address={:?}", address);
  let mut contents = String::new();
  match File::open(&registry_path) {
    Ok(mut registry_file) => {
      &registry_file.read_to_string(&mut contents);
      trace!("Read: {}", &contents);
      let mut acjson = contents.lines();
      // TODO It's sketchy to bury the json parsing error.
      let target = acjson.rfind(|&x| {
                           let a: AddrCred = serde_json::from_str(&x).unwrap_or_default();
                           trace!("Comparing to: {}", a.address);
                           a.address == address
                         });
      info!("target: {}", target.unwrap_or("not found"));
      ShellExitStatus::Success
    }
    Err(wut) => {
      error!("{:?}", wut);
      ShellExitStatus::Failure
    }
  }
}

// Return the SHA256 hash of a user public key as a hexadecimal string.
fn upk_sha256(key: &ACUserPublicKey<BLSG1>) -> String {
  let mut bytes = vec![];
  key.serialize(&mut rmp_serde::Serializer::new(&mut bytes))
     .unwrap();
  // TODO Strange that the serialization is 67 bytes, but the
  // structure contains 6*3 u64 integers, which takes 144 bytes.
  let mut hasher = Sha256::new();
  // Salt the hash to avoid leaking information about other uses of
  // sha256 on the user's public key.
  // TODO Does this buy us anything?
  hasher.input("ACUserPublicKey<zei::algebra::bls12_381::BLSG1>");
  hasher.input(bytes.as_slice());
  hex::encode(hasher.result())
}

// Return the SHA256 hash of an issuer public key as a hexadecimal string.
fn ipk_sha256(key: &ACIssuerPublicKey<BLSG1, BLSG2>) -> String {
  let mut bytes = vec![];
  key.serialize(&mut rmp_serde::Serializer::new(&mut bytes))
     .unwrap();
  let mut hasher = Sha256::new();
  // Salt the hash to avoid leaking information about other uses of
  // sha256 on the user's public key.
  // TODO Does this buy us anything?
  hasher.input("ACIssuerPublicKey<zei::algebra::bls12_381::BLSG1, zei::algebra::bls12_381::BLSG2>");
  hasher.input(bytes.as_slice());
  hex::encode(hasher.result())
}

#[derive(Debug, Serialize, Deserialize)]
struct Issuer {
  public_key: ACIssuerPublicKey<BLSG1, BLSG2>,
  secret_key: ACIssuerSecretKey<BLSG1, BLSScalar>,
}

#[derive(Debug, Serialize, Deserialize)]
struct User {
  public_key: ACUserPublicKey<BLSG1>,
  secret_key: ACUserSecretKey<BLSScalar>,
}

// Generate a new issuer for anonymous credentials.
fn new_issuer() -> Issuer {
  let mut prng: ChaChaRng;
  // For a real application, the seed should be random.
  prng = ChaChaRng::from_seed([0u8; 32]);
  let att_count = 10;
  let (issuer_pk, issuer_sk) = ac_keygen_issuer::<_, BLSScalar, BLSGt>(&mut prng, att_count);
  Issuer { public_key: issuer_pk,
           secret_key: issuer_sk }
}

#[derive(Debug, Serialize, Deserialize)]
struct AddrIssuer {
  address: String,
  issuer_type: u32,
  issuer: Issuer,
}

#[derive(Debug, Serialize, Deserialize)]
struct AddrUser {
  address: String,
  user_type: u32,
  user: User,
}

// Generate a new issuer and append it to the registry.
fn subcommand_add_issuer(registry_path: &Path) -> ShellExitStatus {
  trace!("subcommand addissuer");
  let issuer = new_issuer();
  let address = ipk_sha256(&issuer.public_key);
  trace!("Issuer address: {}", &address);

  match OpenOptions::new().append(true)
                          .create(true)
                          .open(&registry_path)
  {
    Err(io_error) => {
      error!("Couldn't create registry {}: {}",
             &registry_path.display(),
             io_error.description());
      ShellExitStatus::Failure
    }
    Ok(mut registry_file) => {
      let a = AddrIssuer { address: address,
                           issuer_type: 2,
                           issuer: issuer };
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

// Find the first record in the registry that is an AddrIssuer with
// address matching the issuer argument.
fn lookup_issuer(registry_path: &Path, issuer: &str) -> Option<Issuer> {
  let mut contents = String::new();
  match File::open(&registry_path) {
    Ok(mut registry_file) => {
      &registry_file.read_to_string(&mut contents);
      let mut acjson = contents.lines();
      acjson.find_map(|x| match serde_json::from_str::<AddrIssuer>(x) {
              Ok(item) => {
                if item.address == issuer {
                  Some(item.issuer)
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

// Find the first record in the registry that is an AddrUser with
// address matching the user argument.
fn lookup_user(registry_path: &Path, user: &str) -> Option<User> {
  let mut contents = String::new();
  match File::open(&registry_path) {
    Ok(mut registry_file) => {
      &registry_file.read_to_string(&mut contents);
      let mut acjson = contents.lines();
      acjson.find_map(|x| match serde_json::from_str::<AddrUser>(x) {
              Ok(item) => {
                if item.address == user {
                  Some(item.user)
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

fn lookup_signature(registry_path: &Path, signature: &str) -> Option<ACSignature<BLSG1>> {
  let mut contents = String::new();
  match File::open(&registry_path) {
    Ok(mut registry_file) => {
      &registry_file.read_to_string(&mut contents);
      let mut acjson = contents.lines();
      acjson.find_map(|x| match serde_json::from_str::<AddrSig>(x) {
              Ok(item) => {
                if item.address == signature {
                  Some(item.signature)
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

// TODO I bet the return type is wrong.
fn lookup_proof(registry_path: &Path,
                signature: &str)
                -> Option<ACRevealSig<BLSG1, BLSG2, BLSScalar>> {
  let mut contents = String::new();
  match File::open(&registry_path) {
    Ok(mut registry_file) => {
      &registry_file.read_to_string(&mut contents);
      let mut acjson = contents.lines();
      acjson.find_map(|x| match serde_json::from_str::<AddrProof>(x) {
              Ok(item) => {
                if item.address == signature {
                  Some(item.proof)
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

// TODO How do we constrain T and TA to make this compile?
// fn lookup<T, TA>(registry_path: &Path, signature: &str) -> Option<T> {
//   let mut contents = String::new();
//   match File::open(&registry_path) {
//     Ok(mut registry_file) => {
//       &registry_file.read_to_string(&mut contents);
//       let mut acjson = contents.lines();
//       acjson.find_map(|x| match serde_json::from_str::<TA>(x) {
//               Ok(item) => {
//                 if item.address == signature {
//                   Some(item.signature)
//                 } else {
//                   None
//                 }
//               }
//               Err(_) => {
//                 // TODO Report errors other than "missing field" errors.
//                 None
//               }
//             })
//     }
//     Err(open_error) => {
//       error!("{:?}", open_error);
//       None
//     }
//   }
// }

fn append_user(registry_path: &Path, user: AddrUser) -> bool {
  match OpenOptions::new().append(true)
                          .create(true)
                          .open(&registry_path)
  {
    Err(io_error) => {
      error!("Couldn't create registry {}: {}",
             &registry_path.display(),
             io_error.description());
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
  if let Some(issuer) = lookup_issuer(registry_path, issuer) {
    let mut prng: ChaChaRng;
    // For a real application, the seed should be random.
    prng = ChaChaRng::from_seed([0u8; 32]);
    let (user_pk, user_sk) = ac_keygen_user::<_, BLSScalar, BLSGt>(&mut prng, &issuer.public_key);
    let au = AddrUser { address: upk_sha256(&user_pk),
                        user_type: 2,
                        user: User { public_key: user_pk,
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
  sig_type: u32,
  signature: ACSignature<BLSG1>,
}

fn subcommand_sign(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  match (lookup_user(registry_path, user), lookup_issuer(registry_path, issuer)) {
    (Some(user_keys), Some(issuer_keys)) => {
      let mut prng: ChaChaRng;
      // TODO Share one prng across all invocations.
      prng = ChaChaRng::from_seed([0u8; 32]);

      let attrs = [BLSScalar::from_u64(0),
                   BLSScalar::from_u64(1),
                   BLSScalar::from_u64(2),
                   BLSScalar::from_u64(3)];
      let sig = ac_sign::<_, BLSScalar, BLSGt>(&mut prng,
                                               &issuer_keys.secret_key,
                                               &user_keys.public_key,
                                               &attrs);
      // TODO Using the hash of the user's public key as the signature
      // address precludes multiple signatures
      let addr_sig = AddrSig { address: user.to_string(),
                               sig_type: 2,
                               signature: sig };
      // TODO extract a generic function to append a record to the registry
      match OpenOptions::new().append(true).open(&registry_path) {
        Err(io_error) => {
          error!("Couldn't write to registry {}: {}",
                 &registry_path.display(),
                 io_error.description());
          ShellExitStatus::Failure
        }
        Ok(mut registry_file) => {
          let sig_json = serde_json::to_string(&addr_sig).unwrap();
          if let Err(io_error) = registry_file.write_fmt(format_args!("{}\n", sig_json)) {
            error!("Couldn't append to registry {}: {}",
                   registry_path.display(),
                   io_error.description());
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

#[derive(Debug, Serialize, Deserialize)]
struct AddrProof {
  address: String,
  proof_type: u32,
  proof: ACRevealSig<BLSG1, BLSG2, BLSScalar>,
}

fn subcommand_reveal(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  trace!("subcommand reveal user: {} issuer: {}", user, issuer);

  match (lookup_user(registry_path, user),
         lookup_issuer(registry_path, issuer),
         lookup_signature(registry_path, user))
  {
    (Some(user_keys), Some(issuer_keys), Some(sig)) => {
      let mut prng: ChaChaRng;
      // TODO Share one prng across all invocations.
      prng = ChaChaRng::from_seed([0u8; 32]);

      let attrs = [BLSScalar::from_u64(0),
                   BLSScalar::from_u64(1),
                   BLSScalar::from_u64(2),
                   BLSScalar::from_u64(3)];
      let bitmap = [false, false, false, false];
      // TODO handle the error
      let proof = ac_reveal::<_, BLSScalar, BLSGt>(&mut prng,
                                                   &user_keys.secret_key,
                                                   &issuer_keys.public_key,
                                                   &sig,
                                                   &attrs,
                                                   &bitmap).unwrap();
      // TODO Using the hash of the user's public key as the proof
      // address precludes multiple proofs
      let addr_proof = AddrProof { address: user.to_string(),
                                   proof_type: 2,
                                   proof: proof };
      // TODO extract a generic function to append a record to the registry
      match OpenOptions::new().append(true).open(&registry_path) {
        Err(io_error) => {
          error!("Couldn't write to registry {}: {}",
                 &registry_path.display(),
                 io_error.description());
          ShellExitStatus::Failure
        }
        Ok(mut registry_file) => {
          let sig_json = serde_json::to_string(&addr_proof).unwrap();
          if let Err(io_error) = registry_file.write_fmt(format_args!("{}\n", sig_json)) {
            error!("Couldn't append to registry {}: {}",
                   registry_path.display(),
                   io_error.description());
            ShellExitStatus::Failure
          } else {
            info!("Signature");
            ShellExitStatus::Success
          }
        }
      }
    }
    // TODO this doesn't cover all the combinations and doesn't scale nicely
    (Some(_), _, _) => {
      error!("Unable to find user");
      ShellExitStatus::Failure
    }
    (_, Some(_), _) => {
      error!("Unable to find issuer");
      ShellExitStatus::Failure
    }
    (_, _, Some(_)) => {
      error!("Unable to find signature");
      ShellExitStatus::Failure
    }
    (_, _, _) => {
      error!("Unable to find more than one of user, issuer, and signature");
      ShellExitStatus::Failure
    }
  }
}

fn subcommand_verify(registry_path: &Path, user: &str, issuer: &str) -> ShellExitStatus {
  trace!("subcommand verify user: {} issuer: {}", user, issuer);
  let verified;
  match (lookup_issuer(registry_path, issuer), lookup_proof(registry_path, user)) {
    (Some(issuer_keys), Some(proof)) => {
      let attrs = [BLSScalar::from_u64(0),
                   BLSScalar::from_u64(1),
                   BLSScalar::from_u64(2),
                   BLSScalar::from_u64(3)];
      let bitmap = [false, false, false, false];
      verified =
        ac_verify::<BLSScalar, BLSGt>(&issuer_keys.public_key, &attrs, &bitmap, &proof).is_ok();
    }
    (lookup_issuer, lookup_proof) => {
      match lookup_issuer {
        None => error!("Unable to find issuer: {}", issuer),
        _ => (),
      }
      match lookup_proof {
        None => error!("Unable to find proof"),
        _ => (),
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
         ("lookup", Some(matches)) => {
           let user = matches.value_of("user").unwrap();
           subcommand_lookup(&registry_path, &user)
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

         ("create", _) => subcommand_create(&registry_path),
         (subcommand, _) => {
           error!("Please specify a valid subcommand: {:?}. Use -h for help.",
                  subcommand);
           ShellExitStatus::Failure
         }
       } as i32)
}
