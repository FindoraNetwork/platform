use fs2::FileExt;
use std::collections::HashMap;
use std::fs::File;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Result;

/// Define a type for a key for the hashmap.  A version of a
/// transaction has a name and a sequence number.
#[derive(Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
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

type Summary = HashMap<String, HashMap<u32, HashMap<TransactionPermissions, u32>>>;

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
        return er!("{}", e);
      }
    };

    Ok(result)
  }

  /// Write a dictionary's contents to a file.
  pub fn write(&self, data: &mut File) -> Result<()> {
    // The serialize interface consumes our file descriptor.
    let copy = data.duplicate();

    if let Err(e) = bincode::serialize_into(data, &self) {
      return er!("{}", e);
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
  pub fn summarize(list: &[TransactionDictionary]) -> Summary {
    let mut counts: Summary = HashMap::new();

    for dictionary in list {
      for key in dictionary.map.keys() {
        let counts_entry = match counts.get_mut(&key.name) {
          Some(e) => e,
          None => {
            let e = HashMap::new();
            counts.insert(key.name.clone(), e);
            counts.get_mut(&key.name).unwrap()
          }
        };

        let sequence_entry = match counts_entry.get_mut(&key.sequence) {
          Some(e) => e,
          None => {
            let e = HashMap::new();
            counts_entry.insert(key.sequence, e);
            counts_entry.get_mut(&key.sequence).unwrap()
          }
        };

        let permission = dictionary.map[key];

        let permission_entry = match sequence_entry.get_mut(&permission) {
          Some(e) => e,
          None => {
            sequence_entry.insert(permission, 0);
            sequence_entry.get_mut(&permission).unwrap()
          }
        };

        *permission_entry += 1;
      }
    }

    counts
  }

  /// Eventually, compute the set of acceptable transaction versions.
  pub fn reconcile(input: &Summary) -> TransactionDictionary {
    let mut result = TransactionDictionary { map: HashMap::new() };

    for name in input.keys() {
      for version in input[name].keys() {
        let mut state = TransactionPermissions::Current;

        for permission in input[name][version].keys() {
          state = match permission {
            TransactionPermissions::Current => state,
            TransactionPermissions::Deprecated => match state {
              TransactionPermissions::Current => *permission,
              TransactionPermissions::Deprecated => state,
              TransactionPermissions::Disallowed => state,
              TransactionPermissions::Retired => state,
              TransactionPermissions::Undefined => state,
            },
            TransactionPermissions::Disallowed => TransactionPermissions::Disallowed,
            TransactionPermissions::Retired => match state {
              TransactionPermissions::Current => *permission,
              TransactionPermissions::Deprecated => *permission,
              TransactionPermissions::Disallowed => state,
              TransactionPermissions::Retired => state,
              TransactionPermissions::Undefined => state,
            },
            TransactionPermissions::Undefined => match state {
              TransactionPermissions::Current => *permission,
              TransactionPermissions::Deprecated => *permission,
              TransactionPermissions::Disallowed => state,
              TransactionPermissions::Retired => *permission,
              TransactionPermissions::Undefined => state,
            },
          };
        }

        let key = TransactionVersion { name: name.to_string(),
                                       sequence: *version };

        result.map.insert(key, state);
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
    dictionary.declare("current", 2, TransactionPermissions::Retired)
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
    assert!(!dictionary.allow("current", 2));
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
    assert!(!dictionary.allow("current", 2));
    assert!(dictionary.allow("deprecated", 0));
    assert!(!dictionary.allow("disallowed", 1));
    assert!(!dictionary.allow("retired", 2));
    assert!(!dictionary.allow("undefined", 3));
    assert!(!dictionary.allow("xxxxyyyy", 0));
    assert!(dictionary.map.len() == 6);

    // Declare some entries in the dictionary.
    assert!(dictionary.query("current", 1) == TransactionPermissions::Current);
    assert!(dictionary.query("current", 2) == TransactionPermissions::Retired);
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
    let summary = TransactionDictionary::summarize(&vector);

    println!("summary = {:?}", summary);
    assert!(summary.len() == 5);
    assert!(summary["current"][&1][&TransactionPermissions::Current] == 1);
    assert!(summary["current"][&2][&TransactionPermissions::Retired] == 1);
    assert!(summary["deprecated"][&0][&TransactionPermissions::Deprecated] == 1);
    assert!(summary["disallowed"][&1][&TransactionPermissions::Disallowed] == 1);
    assert!(summary["retired"][&2][&TransactionPermissions::Retired] == 1);
    assert!(summary["undefined"][&3][&TransactionPermissions::Undefined] == 1);

    // Declare another dictionary.
    let mut dictionary = TransactionDictionary { map: HashMap::new() };

    dictionary.declare("current", 1, TransactionPermissions::Current)
              .unwrap();
    dictionary.declare("current", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("deprecated", 1, TransactionPermissions::Deprecated)
              .unwrap();
    dictionary.declare("disallowed", 2, TransactionPermissions::Disallowed)
              .unwrap();
    dictionary.declare("different", 2, TransactionPermissions::Retired)
              .unwrap();
    dictionary.declare("unmapped", 3, TransactionPermissions::Undefined)
              .unwrap();

    vector.push(dictionary);
    let summary = TransactionDictionary::summarize(&vector);

    println!("summary = {:?}", summary);

    // Check the number of entries in the first level hash map.
    assert!(summary.len() == 7);

    // Now check the counts for all the second-level hash maps.
    assert!(summary["current"].len() == 2);
    assert!(summary["deprecated"].len() == 2);
    assert!(summary["disallowed"].len() == 2);
    assert!(summary["retired"].len() == 1);
    assert!(summary["undefined"].len() == 1);
    assert!(summary["different"].len() == 1);
    assert!(summary["unmapped"].len() == 1);

    // Check the actual count values.
    assert!(summary["current"][&1][&TransactionPermissions::Current] == 2);
    assert!(summary["current"][&2][&TransactionPermissions::Retired] == 2);
    assert!(summary["deprecated"][&0][&TransactionPermissions::Deprecated] == 1);
    assert!(summary["disallowed"][&1][&TransactionPermissions::Disallowed] == 1);
    assert!(summary["deprecated"][&1][&TransactionPermissions::Deprecated] == 1);
    assert!(summary["disallowed"][&2][&TransactionPermissions::Disallowed] == 1);
    assert!(summary["retired"][&2][&TransactionPermissions::Retired] == 1);
    assert!(summary["undefined"][&3][&TransactionPermissions::Undefined] == 1);
    assert!(summary["different"][&2][&TransactionPermissions::Retired] == 1);
    assert!(summary["unmapped"][&3][&TransactionPermissions::Undefined] == 1);

    let reconciled = TransactionDictionary::reconcile(&summary);

    println!("reconciled = {:?}", reconciled);

    assert!(reconciled.map.len() == 10);

    assert!(reconciled.query("unmapped", 3) == TransactionPermissions::Undefined);
    assert!(reconciled.query("undefined", 3) == TransactionPermissions::Undefined);
    assert!(reconciled.query("deprecated", 0) == TransactionPermissions::Deprecated);
    assert!(reconciled.query("deprecated", 1) == TransactionPermissions::Deprecated);
    assert!(reconciled.query("current", 2) == TransactionPermissions::Retired);
    assert!(reconciled.query("different", 2) == TransactionPermissions::Retired);
    assert!(reconciled.query("retired", 2) == TransactionPermissions::Retired);
    assert!(reconciled.query("disallowed", 1) == TransactionPermissions::Disallowed);
    assert!(reconciled.query("disallowed", 2) == TransactionPermissions::Disallowed);
    assert!(reconciled.query("current", 1) == TransactionPermissions::Current);
  }
}
