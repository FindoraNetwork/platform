use std::fmt;
use zei::errors::ZeiError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PlatformError {
  DeserializationError,
  SerializationError,
  InputsError,
  // Option(String) so I (joe) can be lazy about error descriptions but also catch the laziness
  // later by removing Option
  InvariantError(Option<String>),
  LedgerAppError(Option<String>),
  ZeiError(ZeiError),
  IoError(String),
}

impl std::error::Error for PlatformError {}

impl fmt::Display for PlatformError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      PlatformError::DeserializationError => f.write_str("Could not deserialize object"),
      PlatformError::SerializationError => f.write_str("Could not serialize object"),
      PlatformError::InputsError => f.write_str("Invalid parameters"),
      PlatformError::InvariantError(msg) => {
        f.write_str(format!("Invariant violated: {}",
                            msg.as_ref().unwrap_or(&"UNKNOWN".to_string())).as_str())
      }
      PlatformError::LedgerAppError(msg) => {
        f.write_str(format!("Ledger Application Error: {}",
                            msg.as_ref().unwrap_or(&"UNKNOWN".to_string())).as_str())
      }
      PlatformError::ZeiError(ze) => ze.fmt(f),
      PlatformError::IoError(ioe) => f.write_str(&ioe),
    }
  }
}

impl From<serde_json::Error> for PlatformError {
  fn from(_error: serde_json::Error) -> Self {
    PlatformError::DeserializationError
  }
}

impl From<ZeiError> for PlatformError {
  fn from(error: ZeiError) -> Self {
    PlatformError::ZeiError(error)
  }
}

impl From<std::io::Error> for PlatformError {
  fn from(error: std::io::Error) -> Self {
    PlatformError::IoError(error.to_string())
  }
}
