//!
//! Interface for issuing transactions that can be compiled to Wasm.
//!
//! To compile wasm package, run wasm-pack build in the wasm directory.
//!

#![allow(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

//todo: reduce se/de operations
//todo: remove `unwrap`
//todo: more comments

mod zkcards;

use crate::zkcards::{
    AggregatePublicKey, Card, CardParameters, MaskedCard, Player, ProofShuffle,
    RevealToken, RevealedToken, ShuffleResult, Surrogate,
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
/// generate a `surrogate` with `ProofKeyOwnerShip` on the player's behave
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
    deck: JsValue,
    joint_pk: &AggregatePublicKey,
    nums_of_cards: usize,
) -> JsValue {
    let deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(deck).unwrap();
    let (tokens, proof) = player.shuffle(pp, &deck, joint_pk, nums_of_cards).unwrap();
    let res = ShuffleResult { tokens, proof };

    serde_wasm_bindgen::to_value(&res).unwrap()
}

#[wasm_bindgen]
/// verify shuffled deck from another player
pub fn verify_shuffle(
    parameters: &CardParameters,
    joint_pk: &AggregatePublicKey,
    original_deck: JsValue,
    shuffled_deck: JsValue,
    proof_shuffle: &ProofShuffle,
) -> bool {
    let original_deck: Vec<MaskedCard> =
        serde_wasm_bindgen::from_value(original_deck).unwrap();
    let shuffled_deck: Vec<MaskedCard> =
        serde_wasm_bindgen::from_value(shuffled_deck).unwrap();

    Player::verify_shuffle(
        parameters,
        joint_pk,
        &original_deck,
        &shuffled_deck,
        proof_shuffle,
    )
    .is_ok()
}

#[wasm_bindgen]
/// Register initial card mappings received from game conract or server
pub fn add_card_mappings(player: &mut Player, mappings: JsValue) {
    let mappings: HashMap<Card, Vec<u8>> =
        serde_wasm_bindgen::from_value(mappings).unwrap();

    player.add_card_mappings(mappings);
}

#[wasm_bindgen]
/// Register final deck received from game conract or server
pub fn add_final_deck(player: &mut Player, deck: JsValue) {
    let final_deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(deck).unwrap();

    player.add_deck(final_deck);
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
    player: &Player,
    parameters: &CardParameters,
    reveal_tokens: JsValue,
    card: &MaskedCard,
) -> JsValue {
    let reveal_tokens: Vec<RevealedToken> =
        serde_wasm_bindgen::from_value(reveal_tokens).unwrap();

    let play_card = player
        .peek_at_card(parameters, &reveal_tokens, card)
        .unwrap();
    serde_wasm_bindgen::to_value(&play_card).unwrap()
}
