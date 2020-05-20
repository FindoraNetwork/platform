#![deny(warnings)]
use fs2::FileExt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::Result;
use utils::er;

/// Define a type for a key for the hashmap.  A version of a
/// transaction has a name and a sequence number.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
struct TransactionVersion {
  name: String,
  sequence: u32,
}

/// Define the states allowed for a specific transaction version
///
/// Permission Type
///    Undefined    not present
///    Current      valid and in use
///    Deprecated   valid but being retired
///    Disallowed   formerly valid but not legal, accepted, etc
///    Retired      formerly valid but outdated
///
/// Disallowed transaction types might be negotiated with other
/// machines in the network at some point in time.  Deprecated
/// transactions might result in a warning returned to the source.
///
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum TransactionPermissions {
  Undefined,
  Current,
  Deprecated,
  Disallowed,
  Retired,
}

/// Define a transaction dictionary type.  It maps a transaction
/// version to a permission.
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct TransactionDictionary {
  map: HashMap<TransactionVersion, TransactionPermissions>,
}

type Superset = HashMap<TransactionVersion, TransactionPermissions>;

impl TransactionDictionary {
  /// Create a new, empty directory.
  pub fn new() -> TransactionDictionary {
    Default::default()
  }

  /// Create a directory and initialize it from a file saved
  /// from a previous run.
  pub fn open(data: &mut File) -> Result<TransactionDictionary> {
    let result = match bincode::deserialize_from(data) {
      Ok(result) => result,
      Err(e) => {
        return er(format!("{}", e));
      }
    };

    Ok(result)
  }

  /// Write a dictionary's contents to a file.
  pub fn write(&self, data: &mut File) -> Result<()> {
    // The serialize interface consumes our file descriptor.
    let copy = data.duplicate();

    if let Err(e) = bincode::serialize_into(data, &self) {
      return er(format!("{}", e));
    }

    if let Ok(file) = copy {
      file.sync_all()?;
    }

    Ok(())
  }

  /// Declare an entry in the dictionary.
  pub fn declare(&mut self,
                 transaction: &str,
                 sequence: u32,
                 permission: TransactionPermissions)
                 -> Result<()> {
    self.map
        .insert(TransactionVersion { name: transaction.to_string(),
                                     sequence },
                permission);
    Ok(())
  }

  /// Check for permission to include a transaction in a block.
  pub fn allow(&self, transaction: &str, sequence: u32) -> bool {
    match self.map
              .get(&TransactionVersion { name: transaction.to_string(),
                                         sequence })
    {
      Some(TransactionPermissions::Current) => true,
      Some(TransactionPermissions::Deprecated) => true,
      _ => false,
    }
  }

  /// Return the precise permission associated with a transaction type.  This
  /// can be used during any required negotiations with members of the cluster.
  pub fn query(&self, transaction: &str, sequence: u32) -> TransactionPermissions {
    let lookup = self.map
                     .get(&TransactionVersion { name: transaction.to_string(),
                                                sequence });

    match lookup {
      Some(&x) => x,
      None => TransactionPermissions::Undefined,
    }
  }

  /// Combine a set of dictionaries to form a list of acceptable transaction
  /// versions.
  fn superset(list: &[TransactionDictionary]) -> Superset {
    let mut superset: Superset = HashMap::new();

    for dictionary in list {
      for key in dictionary.map.keys() {
        superset.insert(key.clone(), TransactionPermissions::Undefined);
      }
    }

    superset
  }

  /// Eventually, compute the set of acceptable transaction versions.
  pub fn reconcile(list: &[TransactionDictionary]) -> TransactionDictionary {
    // Clippy doesn't allow cloning of ** types, but its suggested workaround
    // doesn't compile.  When I fix it to compile, another clippy warning
    // is triggered.
    #[allow(clippy::clone_double_ref)]
    let superset = TransactionDictionary::superset(list.clone());
    let mut result = TransactionDictionary { map: HashMap::new() };

    for key in superset.keys() {
      let mut state = TransactionPermissions::Current;

      for dictionary in list {
        let permission = dictionary.query(&key.name, key.sequence);

        state = match permission {
          TransactionPermissions::Current => state,
          TransactionPermissions::Deprecated => match state {
            TransactionPermissions::Current => permission,
            TransactionPermissions::Deprecated => state,
            TransactionPermissions::Disallowed => state,
            TransactionPermissions::Retired => state,
            TransactionPermissions::Undefined => state,
          },
          TransactionPermissions::Disallowed => TransactionPermissions::Disallowed,
          TransactionPermissions::Retired => match state {
            TransactionPermissions::Current => permission,
            TransactionPermissions::Deprecated => permission,
            TransactionPermissions::Disallowed => state,
            TransactionPermissions::Retired => state,
            TransactionPermissions::Undefined => state,
          },
          TransactionPermissions::Undefined => match state {
            TransactionPermissions::Current => permission,
            TransactionPermissions::Deprecated => permission,
            TransactionPermissions::Disallowed => state,
            TransactionPermissions::Retired => permission,
            TransactionPermissions::Undefined => state,
          },
        };

        result.map.insert(key.clone(), state);
      }
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::fs::remove_file;
  use std::fs::OpenOptions;

  #[test]
  fn test_basic_permissions() {
    // Create a dictionary.
    let mut dictionary = TransactionDictionary::new();

    // Declare some entries in the dictionary.
    dictionary.declare("current", 1, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("transit", 2, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("deprecated", 0, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("disallowed", 1, TransactionPermissions::Disallowed)
              .unwrap();
    dictionary.declare("retired", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("undefined", 3, TransactionPermissions::Undefined)
              .unwrap();

    // Check that our entries are working.
    assert!(dictionary.allow("current", 1));
    assert!(dictionary.allow("transit", 2));
    assert!(dictionary.allow("deprecated", 0));
    assert!(!dictionary.allow("disallowed", 1));
    assert!(!dictionary.allow("retired", 2));
    assert!(!dictionary.allow("undefined", 3));
    assert!(!dictionary.allow("xxxxyyyy", 0));
    assert!(dictionary.map.len() == 6);

    let path = "test-dictionary";

    let _ = remove_file(&path);
    let mut file = OpenOptions::new().create(true)
                                     .read(true)
                                     .write(true)
                                     .open(&path)
                                     .unwrap();

    // Write the dictionary to a file and try reopening it.
    if let Err(e) = dictionary.write(&mut file) {
      panic!("Close returned an error:  {}", e);
    }

    let file = OpenOptions::new().create(true)
                                 .read(true)
                                 .write(true)
                                 .open(&path)
                                 .unwrap();

    let dictionary: TransactionDictionary = bincode::deserialize_from(&file).unwrap();

    // Check that the contents match.
    assert!(dictionary.allow("current", 1));
    assert!(dictionary.allow("transit", 2));
    assert!(dictionary.allow("deprecated", 0));
    assert!(!dictionary.allow("disallowed", 1));
    assert!(!dictionary.allow("retired", 2));
    assert!(!dictionary.allow("undefined", 3));
    assert!(!dictionary.allow("xxxxyyyy", 0));
    assert!(dictionary.map.len() == 6);

    // Check the entries in the dictionary.
    assert!(dictionary.query("current", 1) == TransactionPermissions::Current);
    assert!(dictionary.query("transit", 2) == TransactionPermissions::Current);
    assert!(dictionary.query("deprecated", 0) == TransactionPermissions::Deprecated);
    assert!(dictionary.query("disallowed", 1) == TransactionPermissions::Disallowed);
    assert!(dictionary.query("retired", 2) == TransactionPermissions::Retired);
    assert!(dictionary.query("undefined", 3) == TransactionPermissions::Undefined);

    // Try another write.
    let mut file = OpenOptions::new().create(true)
                                     .read(true)
                                     .write(true)
                                     .open(&path)
                                     .unwrap();
    dictionary.write(&mut file).unwrap();
    let _ = remove_file(&path);

    let mut vector = vec![dictionary];
    let summary = TransactionDictionary::reconcile(&vector);

    println!("summary = {:?}", summary);
    assert!(summary.map.len() == 6);

    assert!(summary.query("retired", 2) == TransactionPermissions::Retired);
    assert!(summary.query("undefined", 3) == TransactionPermissions::Undefined);
    assert!(summary.query("deprecated", 0) == TransactionPermissions::Deprecated);
    assert!(summary.query("current", 1) == TransactionPermissions::Current);
    assert!(summary.query("transit", 2) == TransactionPermissions::Current);
    assert!(summary.query("disallowed", 1) == TransactionPermissions::Disallowed);

    // Declare another dictionary.
    let dictionary = new_dictionary();

    vector.push(dictionary);
    let summary = TransactionDictionary::reconcile(&vector);

    println!("summary = {:?}", summary);

    assert!(summary.query("retired", 2) == TransactionPermissions::Retired);
    assert!(summary.query("undefined", 3) == TransactionPermissions::Undefined);
    assert!(summary.query("deprecated", 0) == TransactionPermissions::Deprecated);
    assert!(summary.query("current", 1) == TransactionPermissions::Current);
    assert!(summary.query("transit", 2) == TransactionPermissions::Retired);
    assert!(summary.query("disallowed", 1) == TransactionPermissions::Disallowed);
    assert!(summary.query("unmapped", 3) == TransactionPermissions::Undefined);
    assert!(summary.query("different", 2) == TransactionPermissions::Undefined);

    // Check the number of entries in the dictionary.
    assert!(summary.map.len() == 16);

    let dictionary = new_dictionary();

    assert!(dictionary.query("different", 3) == TransactionPermissions::Current);

    let mut vector = vec![dictionary];
    let summary = TransactionDictionary::reconcile(&vector);

    println!("summary = {:?}", summary);

    assert!(summary.query("different", 3) == TransactionPermissions::Current);
    assert!(summary.query("different", 4) == TransactionPermissions::Undefined);
    assert!(summary.query("different", 5) == TransactionPermissions::Current);
    assert!(summary.query("different", 6) == TransactionPermissions::Current);
    assert!(summary.query("different", 7) == TransactionPermissions::Current);
    assert!(summary.query("different", 8) == TransactionPermissions::Deprecated);
    assert!(summary.query("different", 9) == TransactionPermissions::Retired);

    assert!(summary.map.len() == 14);

    let dictionary = new_dictionary();
    vector.push(dictionary);

    let mut dictionary = new_dictionary();

    dictionary.declare("different", 3, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("different", 4, TransactionPermissions::Disallowed)
              .unwrap();
    dictionary.declare("different", 5, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("different", 6, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("different", 7, TransactionPermissions::Undefined)
              .unwrap();
    dictionary.declare("different", 8, TransactionPermissions::Undefined)
              .unwrap();
    dictionary.declare("different", 9, TransactionPermissions::Undefined)
              .unwrap();

    let length = dictionary.map.len();

    vector.push(dictionary);

    let summary = TransactionDictionary::reconcile(&vector);

    assert!(summary.query("different", 3) == TransactionPermissions::Deprecated);
    assert!(summary.query("different", 4) == TransactionPermissions::Disallowed);
    assert!(summary.query("different", 5) == TransactionPermissions::Retired);
    assert!(summary.query("different", 6) == TransactionPermissions::Deprecated);
    assert!(summary.query("different", 7) == TransactionPermissions::Undefined);
    assert!(summary.query("different", 8) == TransactionPermissions::Undefined);
    assert!(summary.query("different", 9) == TransactionPermissions::Undefined);
    assert!(summary.map.len() == length);
  }

  fn new_dictionary() -> TransactionDictionary {
    let mut dictionary = TransactionDictionary { map: HashMap::new() };

    dictionary.declare("current", 1, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("retired", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("transit", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("deprecated", 0, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("disallowed", 2, TransactionPermissions::Disallowed)
              .unwrap();
    dictionary.declare("different", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("unmapped", 3, TransactionPermissions::Undefined)
              .unwrap();

    dictionary.declare("different", 3, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("different", 4, TransactionPermissions::Undefined)
              .unwrap();
    dictionary.declare("different", 5, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("different", 6, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("different", 7, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("different", 8, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("different", 9, TransactionPermissions::Retired)
              .unwrap();
    dictionary
  }
}
