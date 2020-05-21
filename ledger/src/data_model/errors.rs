use std::fmt;
use zei::errors::ZeiError;

#[macro_export]
macro_rules! error_location {
  () => {
    format!("{}:{}:{}", std::file!(), std::line!(), std::column!())
  };
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PlatformError {
  DeserializationError(String),
  SerializationError(String),
  InputsError(String),
  PolicyFailureError(String),
  CheckedReplayError(String),
  // Option(String) so I (joe) can be lazy about error descriptions but also catch the laziness
  // later by removing Option
  InvariantError(String),
  SubmissionServerError(String),
  QueryServerError(String),
  ZeiError(String, ZeiError),
  IoError(String),
}

impl std::error::Error for PlatformError {}

impl fmt::Display for PlatformError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      PlatformError::DeserializationError(s) => {
        f.write_str(&format!("Could not deserialize object: {}", s))
      }
      PlatformError::SerializationError(s) => {
        f.write_str(&format!("Could not serialize object: {}", s))
      }
      PlatformError::CheckedReplayError(msg) => {
        f.write_str(&format!("Inconsistency found while replaying: {}", msg))
      }
      PlatformError::InputsError(location) => f.write_str(&format!("Error at: {}", location)),
      PlatformError::PolicyFailureError(x) => f.write_str(&format!("Failed policy check: {}", x)),
      PlatformError::InvariantError(msg) => f.write_str(&format!("Invariant violated: {}", msg)),
      PlatformError::SubmissionServerError(msg) => {
        f.write_str(&format!("Ledger Application Error: {}", msg))
      }
      PlatformError::QueryServerError(msg) => f.write_str(&format!("Query Server Error: {}", msg)),
      PlatformError::ZeiError(msg, ze) => f.write_str(&format!("Zei error ({}): ", msg))
                                           .and_then(|_| ze.fmt(f)),
      PlatformError::IoError(ioe) => f.write_str(&ioe),
    }
  }
}

#[macro_export]
macro_rules! zei_fail {
  ($e:expr) => {
    PlatformError::ZeiError(error_location!(), $e)
  };
  ($e: expr, $s:expr) => {
    PlatformError::ZeiError(format!("[{}] {}", &error_location!(), &$s), $e)
  };
}

#[macro_export]
macro_rules! inp_fail {
  () => {
    PlatformError::InputsError(error_location!())
  };
  ($s:expr) => {
    PlatformError::InputsError(format!("[{}] {}", &error_location!(), &$s))
  };
}

#[macro_export]
macro_rules! ser_fail {
  () => {
    PlatformError::SerializationError(error_location!())
  };
  ($s:expr) => {
    PlatformError::SerializationError(format!("[{}] {}", &error_location!(), &$s))
  };
}

#[macro_export]
macro_rules! des_fail {
  () => {
    PlatformError::DeserializationError(error_location!())
  };
  ($s:expr) => {
    PlatformError::DeserializationError(format!("[{}] {}", &error_location!(), &$s))
  };
}

#[macro_export]
macro_rules! inv_fail {
  () => {
    PlatformError::InvariantError(error_location!())
  };
  ($s:expr) => {
    PlatformError::InvariantError(format!("[{}] {}", &error_location!(), &$s))
  };
}

impl From<serde_json::Error> for PlatformError {
  fn from(error: serde_json::Error) -> Self {
    PlatformError::DeserializationError(format!("{:?}", &error))
  }
}

// impl From<ZeiError> for PlatformError {
//   fn from(error: ZeiError) -> Self {
//     PlatformError::ZeiError("Unknown location".to_string(), error)
//   }
// }

impl From<std::io::Error> for PlatformError {
  fn from(error: std::io::Error) -> Self {
    PlatformError::IoError(error.to_string())
  }
}
