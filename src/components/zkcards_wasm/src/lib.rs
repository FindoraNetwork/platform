#![allow(non_snake_case)]
#![allow(dead_code)]

use ark_std::{UniformRand, Zero};
use barnett_smart_card_protocol::Reveal;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsValue;
use web_sys::console;

use rand::thread_rng;

use ark_bn254::{fr::FrConfig, g1::Config, Fr, G1Affine, G1Projective};
use ark_ec::models::short_weierstrass::{Affine, Projective};
use ark_ff::fields::models::fp::{Fp, MontBackend};
use ark_std::One;
use barnett_smart_card_protocol::{
    discrete_log_cards::{
        DLCards, MaskedCard as InMaskedCard, Parameters, RevealToken as InRevealToken,
    },
    BarnettSmartProtocol,
};
use proof_essentials::zkp::proofs::schnorr_identification::proof::Proof as InKeyownershipProof;
use proof_essentials::{
    homomorphic_encryption::el_gamal::{ElGamal, Plaintext},
    utils::{permutation::Permutation as CPermutation, rand::sample_vector},
    vector_commitment::pedersen::PedersenCommitment,
    zkp::{
        arguments::shuffle::proof::Proof as InShuffleProof,
        proofs::chaum_pedersen_dl_equality::proof::Proof as InRevealProof,
    },
};

type CConfig = Config;
type CProjective<T> = Projective<T>;
type CCurve = G1Projective;
type CCardProtocol<'a> = DLCards<'a, CCurve>;
type CParameters = Parameters<CProjective<CConfig>>;
type CPublicKey = Affine<CConfig>;
type CSecretKey = Fp<MontBackend<FrConfig, 4>, 4>;
type CMaskedCard = InMaskedCard<CCurve>;
type CRevealToken = InRevealToken<CCurve>;
type CRevealProof = InRevealProof<CCurve>;
type CAggregatePublicKey = G1Affine;
type CScalar = Fr;
type CShuffleProof = InShuffleProof<Fr, ElGamal<CCurve>, PedersenCommitment<CCurve>>;
type CCard = Plaintext<CProjective<CConfig>>;
type CKeyownershipProof = InKeyownershipProof<CCurve>;

#[derive(PartialEq, Clone, Copy, Eq)]
pub enum Suite {
    Greet,
    Poison,
}

#[derive(PartialEq, Clone, Eq, Copy)]
pub struct MurderCard {
    suite: Suite,
}

impl MurderCard {
    pub fn new(suite: Suite) -> Self {
        Self { suite }
    }
}

impl std::fmt::Debug for MurderCard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let suite = match self.suite {
            Suite::Greet => "G",
            Suite::Poison => "P",
        };

        write!(f, "{}", suite)
    }
}

// Generate a public parameter to setup game instance
// @param {number] m- deck size in 1st dimension
// @param {number] n - deck size in 2nd dimension
// @returns [CardParameters]
#[wasm_bindgen]
pub fn setup(m: usize, n: usize) -> Result<CardParameters, JsValue> {
    let rng = &mut thread_rng();
    CCardProtocol::setup(rng, m, n)
        .map(|v| {
            console::log_1(&"setup success".to_string().into());
            CardParameters { v }
        })
        .map_err(|e| {
            console::log_1(&format!("setup error: {:?}", e).into());
            JsValue::from_str(&e.to_string())
        })
}

// Generate a game key pair
// @param {string} name - Player name
// @returns [PublicKey, SecretKey]
#[wasm_bindgen]
pub fn keygen(pp: &CardParameters, name: String) -> Result<GameKeyAndProof, JsValue> {
    let rng = &mut thread_rng();
    let v = match CCardProtocol::player_keygen(rng, &pp.v) {
        Ok(v) => {
            console::log_1(&"keygen success".to_string().into());
            v
        }
        Err(e) => {
            console::log_1(&format!("keygen error: {:?}", e).into());
            return Err(JsValue::from_str(&e.to_string()));
        }
    };

    let pubKey = PublicKey { v: v.0 };
    let secKey = SecretKey { v: v.1 };

    CCardProtocol::prove_key_ownership(rng, &pp.v, &v.0, &v.1, &name.as_bytes().to_owned())
        .map(|v| {
            console::log_1(&"keygen success".to_string().into());
            GameKeyAndProof {
                pubKey: pubKey,
                secKey: secKey,
                keyownershipProof: KeyownershipProof { v },
            }
        })
        .map_err(|e| {
            console::log_1(&format!("keygen error: {:?}", e).into());
            JsValue::from_str(&e.to_string())
        })
}

// Mask encoded cards as masked cards
// Qparam {CardParameters] parameters- The public parameters of the game
// @param {AggregatePublicKey} sharedKey- The public aggregate key
// @param {Card} encoded - An initial encoded card
// @returns [MaskedCard}
#[wasm_bindgen]
pub fn mask(
    parameters: CardParameters,
    sharedKey: AggregatePublicKey,
    encoded: Card,
) -> Result<MaskedCard, JsValue> {
    let rng = &mut thread_rng();
    CCardProtocol::mask(
        rng,
        &parameters.v,
        &sharedKey.v,
        &encoded.v,
        &CScalar::one(),
    )
    .map(|v| {
        console::log_1(&"mask success".to_string().into());
        MaskedCard { v: v.0 }
    })
    .map_err(|e| {
        console::log_1(&format!("mask error: {:?}", e).into());
        JsValue::from_str(&e.to_string())
    })
}

// Shuffle and remask the deck of cards with a random permutation
// Qparam {CardParameters] parameters- The public parameters of the game
// Qparam {AggregatePublicKey} sharedKey- The public aggregate key
// @param {MaskedCard[]} deck - The deck of masked cards
// Qparam {Permutation} permutation - The deck of masked cards
// @returns [MaskedCard[], ProofShuffle]
#[wasm_bindgen]
pub fn shuffleAndRemask(
    parameters: &CardParameters,
    sharedKey: &AggregatePublicKey,
    deck: &VMaskedCard,
    permutation: &Permutation,
) -> Result<MaskedCardAndShuffleProof, JsValue> {
    let rng = &mut thread_rng();
    let deck: Vec<CMaskedCard> = deck.v.iter().map(|v| v.v).collect();
    let masking_factors: Vec<CScalar> = sample_vector(rng, deck.len());

    CCardProtocol::shuffle_and_remask(
        rng,
        &parameters.v,
        &sharedKey.v,
        &deck,
        &masking_factors,
        &permutation.v,
    )
    .map(|v| {
        console::log_1(&"shuffleAndRemask success".to_string().into());
        let deck: Vec<MaskedCard> = v.0.iter().map(|v| MaskedCard { v: v.clone() }).collect();
        let vdeck = VMaskedCard { v: deck };
        let shuffleProof = ShuffleProof { v: v.1 };
        MaskedCardAndShuffleProof {
            vmaskedCard: vdeck,
            shuffleProof,
        }
    })
    .map_err(|e| {
        console::log_1(&format!("shuffleAndRemask error: {:?}", e).into());
        JsValue::from_str(&e.to_string())
    })
}

// Compute reveal token for a given masked card
// @param [CardParameters] parameters- The public parameters of the game
// Qparam {SecretKey] parameters- A game secret key
// @param {PublicKey} secretKey- A game public key
// @param {MaskedCard} masked- A masked card
// @returns [RevealToken, ProofReveal]
#[wasm_bindgen]
pub fn computeRevealToken(
    parameters: &CardParameters,
    secretKey: &SecretKey,
    publicKey: &PublicKey,
    masked: &MaskedCard,
) -> Result<RevealTokenAndProof, JsValue> {
    let rng = &mut thread_rng();
    CCardProtocol::compute_reveal_token(rng, &parameters.v, &secretKey.v, &publicKey.v, &masked.v)
        .map(|v| {
            console::log_1(&"computeRevealToken success".to_string().into());
            RevealTokenAndProof {
                revealToken: RevealToken { v: v.0 },
                revealProof: RevealProof { v: v.1 },
            }
        })
        .map_err(|e| {
            console::log_1(&format!("computeRevealToken error: {:?}", e).into());
            JsValue::from_str(&e.to_string())
        })
}

// Reveal a masked card
// @param [RevealToken[]]- Reveal tokens for a masked card
// Qparam [MaskedCard}- A masked card
// @returns [MaskedCard]
#[wasm_bindgen]
pub fn reveal(revealTokens: VRevealToken, masked: &MaskedCard) -> Result<Card, JsValue> {
    let zero = CRevealToken::zero();
    let mut aggregate_token = zero;
    for revealToken in revealTokens.v {
        aggregate_token = aggregate_token + revealToken.v;
    }

    aggregate_token
        .reveal(&masked.v)
        .map(|v| Card { v })
        .map_err(|e| {
            console::log_1(&format!("reveal error: {:?}", e).into());
            JsValue::from_str(&e.to_string())
        })
}

// Generate `num' of encoded cards
// @param {number] num - deck size
// @returns Card[]
pub fn encodeCards(num: usize) -> VCard {
    let rng = &mut thread_rng();

    // let mut map: HashMap<CCard, MurderCard> = HashMap::new();
    let plaintexts = (0..num).map(|_| CCard::rand(rng)).collect::<Vec<_>>();

    // // 4 guests
    // for i in 0..40 {
    //     let greet_card = MurderCard::new(Suite::Greet);
    //     map.insert(plaintexts[i], greet_card);
    // }

    // // 2 killers
    // for i in 0..2 {
    //     for j in 0..5 {
    //         // 5 poison cards
    //         let poison_card = MurderCard::new(Suite::Poison);
    //         map.insert(plaintexts[40 + i * 10 + j], poison_card);
    //     }
    //     for j in 5..10 {
    //         // 5 greet cards
    //         let greet_card = MurderCard::new(Suite::Greet);
    //         map.insert(plaintexts[40 + i * 10 + j], greet_card);
    //     }
    // }

    let vcard: Vec<Card> = plaintexts.iter().map(|v| Card { v: v.clone() }).collect();

    VCard { v: vcard }

    // (plaintexts, map)
}

// Public parameters of the game
#[wasm_bindgen]
pub struct CardParameters {
    v: CParameters,
}

// A game public key
#[wasm_bindgen]
#[derive(Clone)]
pub struct PublicKey {
    v: CPublicKey,
}

// A game secret key
#[wasm_bindgen]
#[derive(Clone)]
pub struct SecretKey {
    v: CSecretKey,
}

// A game public key and secret key
#[wasm_bindgen]
pub struct GameKeyAndProof {
    pubKey: PublicKey,
    secKey: SecretKey,
    keyownershipProof: KeyownershipProof,
}
#[wasm_bindgen]
impl GameKeyAndProof {
    #[wasm_bindgen(js_name = getPubKey)]
    pub fn getPubKey(&self) -> PublicKey {
        return self.pubKey.clone();
    }
    #[wasm_bindgen(js_name = getSecKey)]
    pub fn getSecKey(&self) -> SecretKey {
        return self.secKey.clone();
    }
    #[wasm_bindgen(js_name = getProof)]
    pub fn getProof(&self) -> KeyownershipProof {
        return self.keyownershipProof.clone();
    }
}

// An aggregate public key
#[wasm_bindgen]
pub struct AggregatePublicKey {
    v: CAggregatePublicKey,
}

// An initial encoded card
#[wasm_bindgen]
#[derive(Clone)]
pub struct Card {
    v: CCard,
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct VCard {
    v: Vec<Card>,
}

// // A masked card
#[wasm_bindgen]
#[derive(Clone)]
pub struct MaskedCard {
    v: CMaskedCard,
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct VMaskedCard {
    v: Vec<MaskedCard>,
}

#[wasm_bindgen]
pub struct MaskedCardAndShuffleProof {
    vmaskedCard: VMaskedCard,
    shuffleProof: ShuffleProof,
}

#[wasm_bindgen]
impl MaskedCardAndShuffleProof {
    #[wasm_bindgen(js_name = getMaskedCard)]
    pub fn getMaskedCard(&self) -> VMaskedCard {
        self.vmaskedCard.clone()
    }
    #[wasm_bindgen(js_name = getShuffleProof)]
    pub fn getShuffleProof(self) -> ShuffleProof {
        self.shuffleProof
    }
}

// A reveal token for a masked card
#[wasm_bindgen]
#[derive(Clone)]
pub struct RevealToken {
    v: CRevealToken,
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct VRevealToken {
    v: Vec<RevealToken>,
}

// Zero-knowledge proof of card reveal token
#[wasm_bindgen]
#[derive(Clone)]
pub struct RevealProof {
    v: CRevealProof,
}

#[wasm_bindgen]
pub struct RevealTokenAndProof {
    revealToken: RevealToken,
    revealProof: RevealProof,
}

#[wasm_bindgen]
impl RevealTokenAndProof {
    #[wasm_bindgen(js_name = getRevealToken)]
    pub fn getRevealToken(self) -> RevealToken {
        self.revealToken
    }
    #[wasm_bindgen(js_name = getRevealProof)]
    pub fn getRevealProof(self) -> RevealProof {
        self.revealProof
    }
}

// // A permutation for deck shuffling
#[wasm_bindgen]
pub struct Permutation {
    v: CPermutation,
}

// // Zero-knowledge proof of deck shuffling
#[wasm_bindgen]
pub struct ShuffleProof {
    v: CShuffleProof,
}

// Zero-knowledge proof of game key ownership
#[wasm_bindgen]
#[derive(Clone)]
pub struct KeyownershipProof {
    v: CKeyownershipProof,
}
