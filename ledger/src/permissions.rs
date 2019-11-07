use fs2::FileExt;
use std::collections::HashMap;
use std::fs::File;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Result;

/// Define a type for a key for the hashmap.  A version of a
/// transaction has a name and a sequence number.
#[derive(Deserialize, Eq, Hash, PartialEq, Serialize)]
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
#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
pub enum TransactionPermissions {
  Undefined,
  Current,
  Deprecated,
  Disallowed,
  Retired,
}

/// Define a transaction dictionary type.  It maps a transaction
/// version to a permission.
#[derive(Deserialize, Serialize, Default)]
pub struct TransactionDictionary {
  map: HashMap<TransactionVersion, TransactionPermissions>,
}

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
  }
}
