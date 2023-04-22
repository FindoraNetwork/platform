use super::{
    ark_de, ark_se,
    error::{GameErrors, Result},
    user_card::ClassicPlayingCard,
    AggregatePublicKey, Card, CardParameters, CardProtocol, MaskedCard, PlayerPublicKey,
    PlayerSecretKey, ProofKeyOwnership, ProofReveal, ProofShuffle, RevealToken,
    RevealedToken, Scalar,
};
use ark_std::rand::Rng;
use barnett::{BarnettSmartProtocol, Mask};
use proof_essentials::utils::{permutation::Permutation, rand::sample_vector};
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct Player {
    name: Vec<u8>,
    sk: PlayerSecretKey,
    pk: PlayerPublicKey,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Surrogate {
    pub(crate) name: Vec<u8>,
    pub(crate) pk: PlayerPublicKey,
    pub(crate) proof_key: ProofKeyOwnership,
}

impl Surrogate {
    pub fn verify(&self, pp: &CardParameters) -> bool {
        CardProtocol::verify_key_ownership(
            pp.into(),
            &self.pk.0,
            &self.name,
            &self.proof_key.into(),
        )
        .is_ok()
    }
}

impl Player {
    pub fn new<R: Rng>(
        rng: &mut R,
        pp: &CardParameters,
        name: &Vec<u8>,
    ) -> Result<Self> {
        let (pk, sk) = CardProtocol::player_keygen(rng, pp.into())?;
        Ok(Self {
            name: name.clone(),
            sk,
            pk: PlayerPublicKey(pk),
        })
    }

    pub fn new_surrogate(&self, pp: &CardParameters) -> Surrogate {
        let rng = &mut thread_rng();
        let proof_key = CardProtocol::prove_key_ownership(
            rng,
            pp.into(),
            &self.pk.0,
            &self.sk,
            &self.name,
        )
        .unwrap();

        Surrogate {
            name: self.name.clone(),
            pk: self.pk,
            proof_key: proof_key.into(),
        }
    }

    pub fn shuffle(
        &self,
        parameters: &CardParameters,
        deck: &Vec<MaskedCard>,
        joint_pk: &AggregatePublicKey,
        nums_of_cards: usize,
    ) -> Result<(Vec<MaskedCard>, ProofShuffle)> {
        let mut rng = thread_rng();
        let permutation = Permutation::new(&mut rng, nums_of_cards);
        let masking_factors: Vec<Scalar> = sample_vector(&mut rng, nums_of_cards);

        let deck = deck.iter().map(|c| c.clone().into()).collect::<Vec<_>>();
        let (shuffled_deck, shuffle_proof) = CardProtocol::shuffle_and_remask(
            &mut rng,
            parameters.into(),
            &joint_pk.0,
            &deck,
            &masking_factors,
            &permutation,
        )?;

        let shuffled_deck = shuffled_deck
            .into_iter()
            .map(|c| c.into())
            .collect::<Vec<_>>();

        Ok((shuffled_deck, shuffle_proof.into()))
    }

    pub fn verify_shuffle(
        parameters: &CardParameters,
        joint_pk: &AggregatePublicKey,
        original_deck: &Vec<MaskedCard>,
        shuffled_deck: &Vec<MaskedCard>,
        proof_shuffle: &ProofShuffle,
    ) -> Result<()> {
        let original_deck = original_deck
            .iter()
            .map(|e| e.clone().into())
            .collect::<Vec<_>>();
        let shuffled_deck = shuffled_deck
            .iter()
            .map(|e| e.clone().into())
            .collect::<Vec<_>>();
        CardProtocol::verify_shuffle(
            parameters.into(),
            &joint_pk.0,
            &original_deck,
            &shuffled_deck,
            proof_shuffle.into(),
        )
        .map_err(|e| GameErrors::CryptoError(e))
    }

    pub fn peek_at_card(
        parameters: &CardParameters,
        reveal_tokens: &Vec<RevealedToken>,
        card_mappings: &HashMap<Card, Vec<u8>>,
        card: &MaskedCard,
        cards: &Vec<MaskedCard>,
    ) -> Result<Vec<u8>> {
        let _ = cards
            .iter()
            .position(|&x| x == *card)
            .ok_or(GameErrors::CardNotFound)?;

        let raw_reveal_tokens = reveal_tokens
            .iter()
            .map(|t| (t.token.into(), t.proof.into(), t.player.0))
            .collect::<Vec<_>>();

        let unmasked_card =
            CardProtocol::unmask(parameters.into(), &raw_reveal_tokens, card.into())?;
        let opened_card = card_mappings
            .get(&unmasked_card.into())
            .ok_or(GameErrors::InvalidCard)?;

        Ok(opened_card.to_owned())
    }

    pub fn compute_reveal_token<R: Rng>(
        &self,
        rng: &mut R,
        pp: &CardParameters,
        card: &MaskedCard,
    ) -> Result<RevealedToken> {
        let (reveal_token, reveal_proof) = CardProtocol::compute_reveal_token(
            rng,
            pp.into(),
            &self.sk,
            &self.pk.0,
            card.into(),
        )?;

        Ok(RevealedToken {
            token: reveal_token.into(),
            proof: reveal_proof.into(),
            player: self.pk,
        })
    }
}
