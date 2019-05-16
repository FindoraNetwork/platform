use std::{error, fmt};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PlatformError {
  DeserializationError,
  SerializationError,
  InputsError,
  ZeiError,
}

impl fmt::Display for PlatformError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(match self {
                  PlatformError::DeserializationError => "Could not deserialize object",
                  PlatformError::SerializationError => "Could not serialize object",
                  PlatformError::InputsError => "Invalid parameters",
                  PlatformError::ZeiError => "Unaccessible error from Zei",
                })
  }
}

impl error::Error for PlatformError {
  fn description(&self) -> &str {
    match self {
      PlatformError::DeserializationError => "Could not deserialize object",
      PlatformError::SerializationError => "Could not serialize object",
      PlatformError::InputsError => "Parameters were not consistent",
      PlatformError::ZeiError => "Unaccessible error from Zei",
    }
  }
}

impl From<serde_json::Error> for PlatformError {
  fn from(_error: serde_json::Error) -> Self {
    PlatformError::DeserializationError
  }
}

// impl From<zei::errors::ZeiError> for PlatformError {
//     fn from(error: zei::errors::ZeiError) -> Self {
//         PlatformError::ZeiError(error)
//     }
// }
