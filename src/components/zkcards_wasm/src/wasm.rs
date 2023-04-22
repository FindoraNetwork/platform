//!
//! Interface for issuing transactions that can be compiled to Wasm.
//!
//! Allows web clients to issue transactions from a browser contexts.
//!
//! For now, forwards transactions to a ledger hosted locally.
//!
//! To compile wasm package, run wasm-pack build in the wasm directory.
//!

#![allow(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

//todo: remove `unwrap`
//todo: more comments

mod zkcards;

use crate::zkcards::{
    AggregatePublicKey, Card, CardParameters, MaskedCard, Player, ProofShuffle,
    RevealedToken, Surrogate,
};
use rand::thread_rng;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
/// create a new player with `name` and `card parameters` received from contract
pub fn new_player(pp: CardParameters, name: Vec<u8>) -> Player {
    let rng = &mut thread_rng();
    Player::new(rng, &pp, &name).unwrap()
}

#[wasm_bindgen]
/// generate a `surrogate` with `ProofKeyOwnerShip` as this player's behave
pub fn new_surrogate(player: &Player, pp: &CardParameters) -> Surrogate {
    player.new_surrogate(pp)
}

#[wasm_bindgen]
/// verify a player
pub fn verify_proof_pk(player: Surrogate, pp: &CardParameters) -> bool {
    player.verify(&pp)
}

#[wasm_bindgen]
/// Perform a shuffle operation
pub fn shuffle(
    player: &Player,
    pp: &CardParameters,
    deck: &Vec<MaskedCard>,
    joint_pk: &AggregatePublicKey,
    nums_of_cards: usize,
) -> (Vec<MaskedCard>, ProofShuffle) {
    player.shuffle(pp, deck, joint_pk, nums_of_cards).unwrap()
}

#[wasm_bindgen]
/// verify shuffled deck from another player
pub fn verify_shuffle(
    parameters: &CardParameters,
    joint_pk: &AggregatePublicKey,
    original_deck: &Vec<MaskedCard>,
    shuffled_deck: &Vec<MaskedCard>,
    proof_shuffle: &ProofShuffle,
) -> bool {
    Player::verify_shuffle(
        parameters,
        joint_pk,
        original_deck,
        shuffled_deck,
        proof_shuffle,
    )
    .is_ok()
}

#[wasm_bindgen]
/// reveal a card
pub fn compute_reveal_token(
    player: &Player,
    card: &MaskedCard,
    pp: &CardParameters,
) -> RevealedToken {
    let rng = &mut thread_rng();
    player.compute_reveal_token(rng, pp, card).unwrap()
}

#[wasm_bindgen]
/// open a card
pub fn open_card(
    parameters: &CardParameters,
    reveal_tokens: &Vec<RevealedToken>,
    card_mappings: &HashMap<Card, Vec<u8>>,
    card: &MaskedCard,
    cards: &Vec<MaskedCard>,
) -> Vec<u8> {
    Player::peek_at_card(parameters, reveal_tokens, card_mappings, card, cards).unwrap()
}
