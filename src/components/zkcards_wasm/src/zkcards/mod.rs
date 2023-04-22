mod error;
mod player;
mod user_card;

pub use player::*;
pub use user_card::*;

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use barnett::discrete_log_cards;
use proof_essentials::{
    homomorphic_encryption::el_gamal::ElGamal,
    vector_commitment::pedersen::PedersenCommitment,
    zkp::{
        arguments::shuffle,
        proofs::{chaum_pedersen_dl_equality, schnorr_identification},
    },
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// Choose elliptic curve setting
// And instantiate concrete type for our card protocol
type Curve = starknet_curve::Projective;
type Scalar = starknet_curve::Fr;

type CardProtocol<'a> = discrete_log_cards::DLCards<'a, Curve>;
type Enc = ElGamal<Curve>;
type Comm = PedersenCommitment<Curve>;

#[wasm_bindgen]
#[derive(Deserialize, Serialize)]
pub struct CardParameters(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    discrete_log_cards::Parameters<Curve>,
);
impl CardParameters {
    pub fn card_nums(&self) -> (usize, usize) {
        self.0.card_nums()
    }
}
impl From<discrete_log_cards::Parameters<Curve>> for CardParameters {
    fn from(value: discrete_log_cards::Parameters<Curve>) -> Self {
        Self(value)
    }
}
impl From<CardParameters> for discrete_log_cards::Parameters<Curve> {
    fn from(value: CardParameters) -> Self {
        value.0
    }
}
impl<'a> From<&'a CardParameters> for &'a discrete_log_cards::Parameters<Curve> {
    fn from(value: &'a CardParameters) -> Self {
        &value.0
    }
}

type PlayerSecretKey = discrete_log_cards::PlayerSecretKey<Curve>;

#[wasm_bindgen]
#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct PlayerPublicKey(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    pub(crate)  discrete_log_cards::PublicKey<Curve>,
);
#[wasm_bindgen]
#[derive(Clone, Serialize, Deserialize)]
pub struct AggregatePublicKey(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    pub(crate)  discrete_log_cards::PublicKey<Curve>,
);

#[wasm_bindgen]
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug, Serialize, Deserialize)]
pub struct Card(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    discrete_log_cards::Card<Curve>,
);
impl From<discrete_log_cards::Card<Curve>> for Card {
    fn from(value: discrete_log_cards::Card<Curve>) -> Self {
        Self(value)
    }
}
impl From<Card> for discrete_log_cards::Card<Curve> {
    fn from(value: Card) -> Self {
        value.0
    }
}

#[wasm_bindgen]
pub struct MaskedCards {
    inner: Vec<MaskedCard>,
}

#[wasm_bindgen]
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug, Serialize, Deserialize)]
pub struct MaskedCard(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    discrete_log_cards::MaskedCard<Curve>,
);
impl From<discrete_log_cards::MaskedCard<Curve>> for MaskedCard {
    fn from(value: discrete_log_cards::MaskedCard<Curve>) -> Self {
        Self(value)
    }
}
impl From<MaskedCard> for discrete_log_cards::MaskedCard<Curve> {
    fn from(value: MaskedCard) -> Self {
        value.0
    }
}
impl<'a> From<&'a MaskedCard> for &'a discrete_log_cards::MaskedCard<Curve> {
    fn from(value: &'a MaskedCard) -> Self {
        &value.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug, Serialize, Deserialize)]
pub struct RevealToken(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    discrete_log_cards::RevealToken<Curve>,
);
impl From<discrete_log_cards::RevealToken<Curve>> for RevealToken {
    fn from(value: discrete_log_cards::RevealToken<Curve>) -> Self {
        Self(value)
    }
}
impl From<RevealToken> for discrete_log_cards::RevealToken<Curve> {
    fn from(value: RevealToken) -> Self {
        value.0
    }
}

#[wasm_bindgen]
#[derive(Copy, Clone, Deserialize, Serialize)]
pub struct ProofKeyOwnership(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    schnorr_identification::proof::Proof<Curve>,
);
impl From<schnorr_identification::proof::Proof<Curve>> for ProofKeyOwnership {
    fn from(value: schnorr_identification::proof::Proof<Curve>) -> Self {
        Self(value)
    }
}
impl From<ProofKeyOwnership> for schnorr_identification::proof::Proof<Curve> {
    fn from(value: ProofKeyOwnership) -> Self {
        value.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
pub struct ProofReveal(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    chaum_pedersen_dl_equality::proof::Proof<Curve>,
);
impl From<chaum_pedersen_dl_equality::proof::Proof<Curve>> for ProofReveal {
    fn from(value: chaum_pedersen_dl_equality::proof::Proof<Curve>) -> Self {
        Self(value)
    }
}
impl From<ProofReveal> for chaum_pedersen_dl_equality::proof::Proof<Curve> {
    fn from(value: ProofReveal) -> Self {
        value.0
    }
}

#[wasm_bindgen]
#[derive(Deserialize, Serialize)]
pub struct ProofShuffle(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    shuffle::proof::Proof<Scalar, Enc, Comm>,
);
impl From<shuffle::proof::Proof<Scalar, Enc, Comm>> for ProofShuffle {
    fn from(value: shuffle::proof::Proof<Scalar, Enc, Comm>) -> Self {
        Self(value)
    }
}
impl From<ProofShuffle> for shuffle::proof::Proof<Scalar, Enc, Comm> {
    fn from(value: ProofShuffle) -> Self {
        value.0
    }
}
impl<'a> From<&'a ProofShuffle> for &'a shuffle::proof::Proof<Scalar, Enc, Comm> {
    fn from(value: &'a ProofShuffle) -> Self {
        &value.0
    }
}

//pub struct ProofMasking(chaum_pedersen_dl_equality::proof::Proof<Curve>);
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
pub struct ProofRemasking(
    #[serde(serialize_with = "ark_se", deserialize_with = "ark_de")]
    chaum_pedersen_dl_equality::proof::Proof<Curve>,
);
impl From<chaum_pedersen_dl_equality::proof::Proof<Curve>> for ProofRemasking {
    fn from(value: chaum_pedersen_dl_equality::proof::Proof<Curve>) -> Self {
        Self(value)
    }
}
impl From<ProofRemasking> for chaum_pedersen_dl_equality::proof::Proof<Curve> {
    fn from(value: ProofRemasking) -> Self {
        value.0
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct RevealedToken {
    pub(crate) token: RevealToken,
    pub(crate) proof: ProofReveal,
    pub(crate) player: PlayerPublicKey,
}

fn ark_se<S, A: CanonicalSerialize>(a: &A, s: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut bytes = vec![];
    a.serialize(&mut bytes).map_err(serde::ser::Error::custom)?;
    s.serialize_bytes(&bytes)
}

fn ark_de<'de, D, A: CanonicalDeserialize>(data: D) -> Result<A, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let s: Vec<u8> = serde::de::Deserialize::deserialize(data)?;
    let a = A::deserialize(s.as_slice());
    a.map_err(serde::de::Error::custom)
}
