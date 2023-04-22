//!
//! Interface for issuing transactions that can be compiled to Wasm.
//!
//! To compile wasm package, run wasm-pack build in the wasm directory.
//!

#![allow(warnings)]
#![deny(missing_docs)]
#![allow(clippy::needless_borrow)]

//todo: more comments

mod zkcards;

use crate::zkcards::{
    AggregatePublicKey, Card, CardParameters, MaskedCard, Player, ProofShuffle,
    RevealToken, RevealedToken, ShuffleResult, Surrogate,
};
use rand::thread_rng;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[inline(always)]
fn error_to_jsvalue<T: std::fmt::Display>(e: T) -> JsValue {
    JsValue::from_str(&e.to_string())
}

#[wasm_bindgen]
/// create a new player with `name` and `card parameters` received from contract
pub fn new_player(pp: &CardParameters, name: &[u8]) -> Result<Player, JsValue> {
    let rng = &mut thread_rng();
    Player::new(rng, pp, name).map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// generate a `surrogate` with `ProofKeyOwnerShip` on the player's behave
pub fn new_surrogate(
    player: &Player,
    pp: &CardParameters,
) -> Result<Surrogate, JsValue> {
    player.new_surrogate(pp).map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// verify a player
pub fn verify_proof_pk(player: Surrogate, pp: &CardParameters) -> Result<bool, JsValue> {
    player.verify(&pp).map_err(error_to_jsvalue)?;
    Ok(true)
}

#[wasm_bindgen]
/// Perform a shuffle operation
pub fn shuffle(
    player: &Player,
    pp: &CardParameters,
    deck: JsValue,
    joint_pk: &AggregatePublicKey,
    nums_of_cards: usize,
) -> Result<JsValue, JsValue> {
    let deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(deck)?;
    let (tokens, proof) = player
        .shuffle(pp, &deck, joint_pk, nums_of_cards)
        .map_err(error_to_jsvalue)?;
    let res = ShuffleResult { tokens, proof };

    serde_wasm_bindgen::to_value(&res).map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// verify shuffled deck from another player
pub fn verify_shuffle(
    parameters: &CardParameters,
    joint_pk: &AggregatePublicKey,
    original_deck: JsValue,
    shuffled_deck: JsValue,
    proof_shuffle: &ProofShuffle,
) -> Result<bool, JsValue> {
    let original_deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(original_deck)?;
    let shuffled_deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(shuffled_deck)?;

    Player::verify_shuffle(
        parameters,
        joint_pk,
        &original_deck,
        &shuffled_deck,
        proof_shuffle,
    )
    .map_err(error_to_jsvalue)?;

    Ok(true)
}

#[wasm_bindgen]
/// Register initial card mappings received from game conract or server
pub fn add_card_mappings(player: &mut Player, mappings: JsValue) -> Result<(), JsValue> {
    let mappings: HashMap<Card, Vec<u8>> = serde_wasm_bindgen::from_value(mappings)?;
    player.add_card_mappings(mappings);
    Ok(())
}

#[wasm_bindgen]
/// Register final deck received from game conract or server
pub fn add_final_deck(player: &mut Player, deck: JsValue) -> Result<(), JsValue> {
    let final_deck: Vec<MaskedCard> = serde_wasm_bindgen::from_value(deck)?;
    player.add_deck(final_deck);
    Ok(())
}

#[wasm_bindgen]
/// reveal a card
pub fn compute_reveal_token(
    player: &Player,
    card: &MaskedCard,
    pp: &CardParameters,
) -> Result<RevealedToken, JsValue> {
    let rng = &mut thread_rng();
    player
        .compute_reveal_token(rng, pp, card)
        .map_err(error_to_jsvalue)
}

#[wasm_bindgen]
/// open a card
pub fn open_card(
    player: &Player,
    parameters: &CardParameters,
    reveal_tokens: JsValue,
    card: &MaskedCard,
) -> Result<JsValue, JsValue> {
    let reveal_tokens: Vec<RevealedToken> =
        serde_wasm_bindgen::from_value(reveal_tokens)?;

    let play_card = player
        .peek_at_card(parameters, &reveal_tokens, card)
        .map_err(error_to_jsvalue)?;

    serde_wasm_bindgen::to_value(&play_card).map_err(error_to_jsvalue)
}
