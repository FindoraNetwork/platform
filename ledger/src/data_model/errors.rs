use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PlatformError {
    DeserializationError(Option<String>),
    SerializationError(Option<String>),
    InputsError(Option<String>),
    PolicyFailureError(Option<String>),
    CheckedReplayError(Option<String>),
    InvariantError(Option<String>),
    SubmissionServerError(Option<String>),
    QueryServerError(Option<String>),
    ZeiError(Option<String>),
    IoError(Option<String>),
    Unknown,
}

impl std::error::Error for PlatformError {}

impl fmt::Display for PlatformError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PlatformError::DeserializationError(s) => f.write_str(&format!(
                "Could not deserialize object: {}",
                s.as_deref().unwrap_or_default()
            )),
            PlatformError::SerializationError(s) => f.write_str(&format!(
                "Could not serialize object: {}",
                s.as_deref().unwrap_or_default()
            )),
            PlatformError::CheckedReplayError(msg) => f.write_str(&format!(
                "Inconsistency found while replaying: {}",
                msg.as_deref().unwrap_or_default()
            )),
            PlatformError::InputsError(location) => f.write_str(&format!(
                "Error at: {}",
                location.as_deref().unwrap_or_default()
            )),
            PlatformError::PolicyFailureError(x) => f.write_str(&format!(
                "Failed policy check: {}",
                x.as_deref().unwrap_or_default()
            )),
            PlatformError::InvariantError(msg) => f.write_str(&format!(
                "Invariant violated: {}",
                msg.as_deref().unwrap_or_default()
            )),
            PlatformError::SubmissionServerError(msg) => f.write_str(&format!(
                "Ledger Application Error: {}",
                msg.as_deref().unwrap_or_default()
            )),
            PlatformError::QueryServerError(msg) => f.write_str(&format!(
                "Query Server Error: {}",
                msg.as_deref().unwrap_or_default()
            )),
            PlatformError::ZeiError(msg) => f.write_str(&format!(
                "{} ({}): ",
                stringify!(PlatformError::ZeiError),
                msg.as_deref().unwrap_or_default()
            )),
            PlatformError::IoError(ioe) => {
                f.write_str(&ioe.as_deref().unwrap_or_default())
            }
            PlatformError::Unknown => f.write_str(stringify!(PlatformError::Unknown)),
        }
    }
}

#[macro_export]
macro_rules! zei_fail {
    () => {
        $crate::data_model::errors::PlatformError::ZeiError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::ZeiError(Some(format!("{}", &$s)))
    };
}

#[macro_export]
macro_rules! inp_fail {
    () => {
        $crate::data_model::errors::PlatformError::InputsError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::InputsError(Some(format!("{}", &$s)))
    };
}

#[macro_export]
macro_rules! sub_fail {
    () => {
        $crate::data_model::errors::PlatformError::SubmissionServerError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::SubmissionServerError(Some(format!(
            "{}",
            &$s
        )))
    };
}

#[macro_export]
macro_rules! ser_fail {
    () => {
        $crate::data_model::errors::PlatformError::SerializationError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::SerializationError(Some(format!(
            "{}",
            &$s
        )))
    };
}

#[macro_export]
macro_rules! des_fail {
    () => {
        $crate::data_model::errors::PlatformError::DeserializationError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::DeserializationError(Some(format!(
            "{}",
            &$s
        )))
    };
}

#[macro_export]
macro_rules! inv_fail {
    () => {
        $crate::data_model::errors::PlatformError::InvariantError(None)
    };
    ($s:expr) => {
        $crate::data_model::errors::PlatformError::InvariantError(Some(format!(
            "{}",
            &$s
        )))
    };
}

impl From<serde_json::Error> for PlatformError {
    fn from(error: serde_json::Error) -> Self {
        PlatformError::DeserializationError(Some(format!("{:?}", &error)))
    }
}

impl From<std::io::Error> for PlatformError {
    fn from(error: std::io::Error) -> Self {
        PlatformError::IoError(Some(error.to_string()))
    }
}
