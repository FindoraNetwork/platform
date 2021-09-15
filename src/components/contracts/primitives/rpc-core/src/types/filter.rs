// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
// This file is part of Frontier.
//
// Copyright (c) 2015-2020 Parity Technologies (UK) Ltd.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.
use core::convert::AsRef;
use ethereum_types::{Bloom, BloomInput, H160, H256, U256};
use serde::de::{DeserializeOwned, Error};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{from_value, Value};
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use crate::types::{BlockNumber, Log};

/// Variadic value
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum VariadicValue<T>
where
    T: DeserializeOwned,
{
    /// Single
    Single(T),
    /// List
    Multiple(Vec<T>),
    /// None
    Null,
}

impl<'a, T> Deserialize<'a> for VariadicValue<T>
where
    T: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<VariadicValue<T>, D::Error>
    where
        D: Deserializer<'a>,
    {
        let v: Value = Deserialize::deserialize(deserializer)?;

        if v.is_null() {
            return Ok(VariadicValue::Null);
        }

        from_value(v.clone())
            .map(VariadicValue::Single)
            .or_else(|_| from_value(v).map(VariadicValue::Multiple))
            .map_err(|err| {
                D::Error::custom(format!("Invalid variadic value type: {}", err))
            })
    }
}

/// Filter Address
pub type FilterAddress = VariadicValue<H160>;
/// Topic, supports `A` | `null` | `[A,B,C]` | `[A,[B,C]]` | [null,[B,C]] | [null,[null,C]]
pub type Topic = VariadicValue<Option<VariadicValue<Option<H256>>>>;
/// FlatTopic, simplifies the matching logic.
pub type FlatTopic = VariadicValue<Option<H256>>;

pub type BloomFilter<'a> = Vec<Option<Bloom>>;

/// Filter
#[derive(Debug, PartialEq, Clone, Deserialize, Eq, Hash)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "camelCase")]
pub struct Filter {
    /// From Block
    pub from_block: Option<BlockNumber>,
    /// To Block
    pub to_block: Option<BlockNumber>,
    /// Block hash
    pub block_hash: Option<H256>,
    /// Address
    pub address: Option<FilterAddress>,
    /// Topics
    pub topics: Option<Topic>,
}

/// Helper for Filter matching.
/// Supports conditional indexed parameters and wildcards.
#[derive(Debug)]
pub struct FilteredParams {
    pub filter: Option<Filter>,
    pub flat_topics: Vec<FlatTopic>,
}

impl Default for FilteredParams {
    fn default() -> Self {
        FilteredParams {
            filter: None,
            flat_topics: Vec::new(),
        }
    }
}

impl FilteredParams {
    pub fn new(f: Option<Filter>) -> Self {
        if let Some(f) = f {
            return FilteredParams {
                filter: Some(f.clone()),
                flat_topics: {
                    if let Some(t) = f.topics {
                        Self::flatten(&t)
                    } else {
                        Vec::new()
                    }
                },
            };
        }
        Self::default()
    }

    pub fn bloom_filter<'a>(
        address: &'a Option<FilterAddress>,
        topics: &'a Option<Vec<FlatTopic>>,
    ) -> BloomFilter<'a> {
        let mut blooms = BloomFilter::new();
        // Address
        if let Some(address) = address {
            match address {
                VariadicValue::Single(address) => {
                    let bloom: Bloom = BloomInput::Raw(address.as_ref()).into();
                    blooms.push(Some(bloom))
                }
                VariadicValue::Multiple(addresses) => {
                    for address in addresses.iter() {
                        let bloom: Bloom = BloomInput::Raw(address.as_ref()).into();
                        blooms.push(Some(bloom))
                    }
                }
                _ => blooms.push(None),
            }
        }
        // Topics
        if let Some(topics) = topics {
            for flat in topics {
                match flat {
                    VariadicValue::Single(topic) => {
                        if let Some(topic) = topic {
                            let bloom: Bloom = BloomInput::Raw(topic.as_ref()).into();
                            blooms.push(Some(bloom));
                        } else {
                            blooms.push(None);
                        }
                    }
                    _ => blooms.push(None),
                }
            }
        }
        blooms
    }

    /// Checks the possible existance of an address or topic(s) in a Bloom.
    /// Wildcards (VariadicValue::Null) are matched as positive.
    pub fn in_bloom(bloom: Bloom, bloom_input: &[Option<Bloom>]) -> bool {
        if bloom_input.is_empty() {
            return true;
        } else {
            for inner in bloom_input {
                // Wildcard (None) or matching topic.
                if match inner {
                    Some(input) => bloom.contains_bloom(input),
                    None => true,
                } {
                    return true;
                }
            }
        }
        false
    }

    /// Cartesian product for VariadicValue conditional indexed parameters.
    /// Executed once on struct instance.
    /// i.e. `[A,[B,C]]` to `[[A,B],[A,C]]`.
    fn flatten(topic: &Topic) -> Vec<FlatTopic> {
        fn cartesian(lists: &[Vec<Option<H256>>]) -> Vec<Vec<Option<H256>>> {
            let mut res = vec![];
            let mut list_iter = lists.iter();
            if let Some(first_list) = list_iter.next() {
                for &i in first_list {
                    res.push(vec![i]);
                }
            }
            for l in list_iter {
                let mut tmp = vec![];
                for r in res {
                    for &el in l {
                        let mut tmp_el = r.clone();
                        tmp_el.push(el);
                        tmp.push(tmp_el);
                    }
                }
                res = tmp;
            }
            res
        }
        let mut out: Vec<FlatTopic> = Vec::new();
        match topic {
            VariadicValue::Multiple(multi) => {
                let mut vvs: Vec<Vec<Option<H256>>> = Vec::new();
                for v in multi {
                    vvs.push({
                        if let Some(v) = v {
                            match v {
                                VariadicValue::Single(s) => {
                                    vec![*s]
                                }
                                VariadicValue::Multiple(s) => s.clone(),
                                VariadicValue::Null => {
                                    vec![None]
                                }
                            }
                        } else {
                            vec![None]
                        }
                    });
                }
                for permut in cartesian(&vvs) {
                    out.push(FlatTopic::Multiple(permut));
                }
            }
            VariadicValue::Single(single) => {
                if let Some(single) = single {
                    out.push(single.clone());
                }
            }
            VariadicValue::Null => {
                out.push(FlatTopic::Null);
            }
        }
        out
    }

    /// Replace None values - aka wildcards - for the log input value in that position.
    pub fn replace(&self, log: &Log, topic: FlatTopic) -> Option<Vec<H256>> {
        let mut out: Vec<H256> = Vec::new();
        match topic {
            VariadicValue::Single(Some(value)) => {
                out.push(value);
            }
            VariadicValue::Multiple(value) => {
                for (k, v) in value.into_iter().enumerate() {
                    if let Some(v) = v {
                        out.push(v);
                    } else {
                        out.push(log.topics[k]);
                    }
                }
            }
            _ => {}
        };
        if out.is_empty() {
            return None;
        }
        Some(out)
    }

    pub fn filter_block_range(&self, block_number: u64) -> bool {
        let mut out = true;
        let filter = self.filter.clone().unwrap();
        if let Some(from) = filter.from_block {
            if let BlockNumber::Num(_) = from {
                if from.to_min_block_num().unwrap_or(0_u64) > block_number {
                    out = false;
                }
            }
        }
        if let Some(to) = filter.to_block {
            match to {
                BlockNumber::Num(_) => {
                    if to.to_min_block_num().unwrap_or(0_u64) < block_number {
                        out = false;
                    }
                }
                BlockNumber::Earliest => {
                    out = false;
                }
                _ => {}
            }
        }
        out
    }

    pub fn filter_block_hash(&self, block_hash: H256) -> bool {
        if let Some(h) = self.filter.clone().unwrap().block_hash {
            if h != block_hash {
                return false;
            }
        }
        true
    }

    pub fn filter_address(&self, log: &Log) -> bool {
        if let Some(input_address) = &self.filter.clone().unwrap().address {
            match input_address {
                VariadicValue::Single(x) => {
                    if log.address != *x {
                        return false;
                    }
                }
                VariadicValue::Multiple(x) => {
                    if !x.contains(&log.address) {
                        return false;
                    }
                }
                _ => {
                    return true;
                }
            }
        }
        true
    }

    pub fn filter_topics(&self, log: &Log) -> bool {
        let mut out: bool = true;
        for topic in self.flat_topics.clone() {
            match topic {
                VariadicValue::Single(single) => {
                    if let Some(single) = single {
                        if !log.topics.starts_with(&[single]) {
                            out = false;
                        }
                    }
                }
                VariadicValue::Multiple(multi) => {
                    // Shrink the topics until the last item is Some.
                    let mut new_multi = multi;
                    while new_multi
                        .iter()
                        .last()
                        .unwrap_or(&Some(H256::default()))
                        .is_none()
                    {
                        new_multi.pop();
                    }
                    // We can discard right away any logs with lesser topics than the filter.
                    if new_multi.len() > log.topics.len() {
                        out = false;
                        break;
                    }
                    let replaced: Option<Vec<H256>> =
                        self.replace(log, VariadicValue::Multiple(new_multi));
                    if let Some(replaced) = replaced {
                        out = false;
                        if log.topics.starts_with(&replaced[..]) {
                            out = true;
                            break;
                        }
                    }
                }
                _ => {
                    out = true;
                }
            }
        }
        out
    }
}

/// Results of the filter_changes RPC.
#[derive(Debug, PartialEq)]
pub enum FilterChanges {
    /// New logs.
    Logs(Vec<Log>),
    /// New hashes (block or transactions)
    Hashes(Vec<H256>),
    /// Empty result,
    Empty,
}

impl Serialize for FilterChanges {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            FilterChanges::Logs(ref logs) => logs.serialize(s),
            FilterChanges::Hashes(ref hashes) => hashes.serialize(s),
            FilterChanges::Empty => (&[] as &[Value]).serialize(s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FilterType {
    Block,
    PendingTransaction,
    Log(Filter),
}

#[derive(Debug, Clone)]
pub struct FilterPoolItem {
    pub last_poll: BlockNumber,
    pub filter_type: FilterType,
    pub at_block: u64,
}

/// On-memory stored filters created through the `eth_newFilter` RPC.
pub type FilterPool = Arc<Mutex<BTreeMap<U256, FilterPoolItem>>>;
