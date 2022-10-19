#![deny(warnings)]
#![allow(missing_docs)]

#[cfg(test)]
mod tests;

pub use abci::{Event as AbciEvent, Pair as AbciPair};
pub use fp_event_derive::Event;
pub use serde_json::to_vec;

pub trait Event {
    /// Generates `Event` where value types are all casted to strings.
    #[allow(clippy::wrong_self_convention)]
    fn emit_event(field_type: String, structure: Self) -> AbciEvent;

    /// Generates `Event` where value types are serializable.
    #[allow(clippy::wrong_self_convention)]
    fn emit_serde_event(field_type: String, structure: Self) -> AbciEvent;
}
