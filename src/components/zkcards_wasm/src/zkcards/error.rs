use barnett::error::CardProtocolError;
use proof_essentials::error::CryptoError;
use std::fmt::{Display, Formatter};

pub type Result<U> = std::result::Result<U, GameErrors>;

#[derive(Debug, PartialEq)]
pub enum GameErrors {
    InvalidParameters,
    CardNotFound,
    InvalidCard,
    NotReady,
    AllShuffled,
    ProtocolError(CardProtocolError),
    CryptoError(CryptoError),
    NotEnoughRevealedTokens(u32),
}

impl Display for GameErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidParameters => write!(f, "Invalid config parameters"),
            Self::CardNotFound => write!(f, "No such card in hand"),
            Self::InvalidCard => write!(f, "Invalid card"),
            Self::NotReady => write!(f, "Game not Ready"),
            Self::AllShuffled => write!(f, "All players have been shuffled"),
            Self::ProtocolError(e) => write!(f, "Protocol Error: {}", e.to_string()),
            Self::CryptoError(e) => write!(f, "Crypto Error: {}", e.to_string()),
            Self::NotEnoughRevealedTokens(_) => write!(f, "No enough revealed tokens"),
        }
    }
}

impl From<CardProtocolError> for GameErrors {
    fn from(value: CardProtocolError) -> Self {
        Self::ProtocolError(value)
    }
}

impl From<CryptoError> for GameErrors {
    fn from(value: CryptoError) -> Self {
        Self::CryptoError(value)
    }
}
