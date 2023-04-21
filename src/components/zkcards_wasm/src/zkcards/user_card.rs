use super::Card;
use ark_ff::UniformRand;
use ark_std::rand::Rng;
use barnett::discrete_log_cards;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(PartialEq, Clone, Copy, Eq, Serialize, Deserialize)]
pub enum Suite {
    Club,
    Diamond,
    Heart,
    Spade,
}

impl Suite {
    pub(crate) const VALUES: [Self; 4] =
        [Self::Club, Self::Diamond, Self::Heart, Self::Spade];
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Eq, Serialize, Deserialize)]
pub enum Value {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl Value {
    pub(crate) const VALUES: [Self; 13] = [
        Self::Two,
        Self::Three,
        Self::Four,
        Self::Five,
        Self::Six,
        Self::Seven,
        Self::Eight,
        Self::Nine,
        Self::Ten,
        Self::Jack,
        Self::Queen,
        Self::King,
        Self::Ace,
    ];
}

#[derive(PartialEq, Clone, Eq, Copy, Serialize, Deserialize)]
pub struct ClassicPlayingCard {
    value: Value,
    suite: Suite,
}

impl ClassicPlayingCard {
    pub fn new(value: Value, suite: Suite) -> Self {
        Self { value, suite }
    }
}

impl std::fmt::Debug for ClassicPlayingCard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let suite = match self.suite {
            Suite::Club => "♣",
            Suite::Diamond => "♦",
            Suite::Heart => "♥",
            Suite::Spade => "♠",
        };

        let val = match self.value {
            Value::Two => "2",
            Value::Three => "3",
            Value::Four => "4",
            Value::Five => "5",
            Value::Six => "6",
            Value::Seven => "7",
            Value::Eight => "8",
            Value::Nine => "9",
            Value::Ten => "10",
            Value::Jack => "J",
            Value::Queen => "Q",
            Value::King => "K",
            Value::Ace => "A",
        };

        write!(f, "{}{}", val, suite)
    }
}

pub(crate) fn encode_cards<R: Rng>(
    rng: &mut R,
    num_of_cards: usize,
) -> HashMap<Card, Vec<u8>> {
    let mut map = HashMap::new();
    let plaintexts = (0..num_of_cards)
        .map(|_| discrete_log_cards::Card::rand(rng).into())
        .collect::<Vec<_>>();

    let mut i = 0;
    for value in Value::VALUES.iter().copied() {
        for suite in Suite::VALUES.iter().copied() {
            let current_card = ClassicPlayingCard::new(value, suite);
            let raw_card = serde_json::to_vec(&current_card).unwrap();
            map.insert(plaintexts[i], raw_card);
            i += 1;
        }
    }

    map
}
