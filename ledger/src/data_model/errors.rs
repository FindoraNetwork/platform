use std::{error, fmt};
use zei::errors::ZeiError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PlatformError {
  DeserializationError,
  SerializationError,
  InputsError,
  PolicyFailureError,
  // Option(String) so I (joe) can be lazy about error descriptions but also catch the laziness
  // later by removing Option
  InvariantError(Option<String>),
  ZeiError(ZeiError),
  IoError(String),
}

impl fmt::Display for PlatformError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      PlatformError::DeserializationError => f.write_str("Could not deserialize object"),
      PlatformError::SerializationError => f.write_str("Could not serialize object"),
      PlatformError::InputsError => f.write_str("Invalid parameters"),
      PlatformError::PolicyFailureError => f.write_str("Failed policy check"),
      PlatformError::InvariantError(msg) => {
        f.write_str(format!("Invariant violated: {}",
                            msg.as_ref().unwrap_or(&"UNKNOWN".to_string())).as_str())
      }
      PlatformError::ZeiError(ze) => ze.fmt(f),
      PlatformError::IoError(ioe) => f.write_str(&ioe),
    }
  }
}

impl error::Error for PlatformError {
  fn description(&self) -> &str {
    match self {
      PlatformError::DeserializationError => "Could not deserialize object",
      PlatformError::SerializationError => "Could not serialize object",
      PlatformError::InputsError => "Parameters were not consistent",
      PlatformError::PolicyFailureError => "Policy check failed",
      PlatformError::InvariantError(_msg) => "Invariant error",
      PlatformError::ZeiError(ze) => ze.description(),
      PlatformError::IoError(ioe) => &ioe,
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
