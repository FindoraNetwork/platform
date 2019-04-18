use std::{fmt, error};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LedgerError {
    DeserializationError,
    SerializationError,
}

impl fmt::Display for LedgerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            LedgerError::DeserializationError => "Could not deserialize object",
            LedgerError::SerializationError => "Could not serialize object",
        })
    }
}

impl error::Error for LedgerError {
    fn description(&self) -> &str {
        match self {
            LedgerError::DeserializationError => "Could not deserialize object",
            LedgerError::SerializationError => "Could not serialize object",
        }
    }
}

impl From<serde_json::Error> for LedgerError {
    fn from(_error: serde_json::Error) -> Self {
        LedgerError::DeserializationError
    }
}
