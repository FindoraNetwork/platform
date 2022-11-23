//! transfer staking data to evm-staking

use super::{Delegation, DelegationState, StakerMemo, Staking, BLOCK_HEIGHT_MAX};
use ruc::{d, Result, RucResult};
use zei::xfr::sig::XfrPublicKey;

use std::collections::HashMap;

impl Staking {
    ///make date for transfer.
    pub fn make_transfer_data(
        &mut self,
    ) -> Result<(
        Vec<(Vec<u8>, Vec<u8>, XfrPublicKey, u64, StakerMemo, [u64; 2])>,
        HashMap<XfrPublicKey, Delegation>,
        HashMap<XfrPublicKey, Delegation>,
    )> {
        let mut delegation_bond = HashMap::new();
        let mut delegation_unbond = HashMap::new();

        let keys: Vec<_> = self
            .delegation_info
            .global_delegation_records_map
            .keys()
            .map(|k| *k)
            .collect();

        for pk in keys.into_iter() {
            let d = self
                .delegation_info
                .global_delegation_records_map
                .remove(&pk)
                .unwrap();
            if d.state == DelegationState::Bond {
                if d.end_height == BLOCK_HEIGHT_MAX {
                    delegation_bond.insert(pk, d);
                } else {
                    delegation_unbond.insert(pk, d);
                }
            } else {
                self.delegation_info
                    .global_delegation_records_map
                    .insert(pk, d);
            }
        }

        let height_last = *self.validator_info.keys().rev().next().c(d!())?;
        println!("Data at height: {}", height_last);

        let validator_data = self.validator_info.remove(&height_last).c(d!())?;

        let mut stake_date = vec![];
        for (pk, v) in validator_data.body.into_iter() {
            stake_date.push((
                v.td_addr,
                v.td_pubkey,
                pk,
                v.td_power,
                v.memo,
                v.commission_rate,
            ));
        }

        Ok((stake_date, delegation_bond, delegation_unbond))
    }
}

/// remove delgations that delegate to 0, flatten nested map to list.
/// return (delegator public key, validator public key, amount).
pub fn flatten_filter_delegations(
    delegations: HashMap<XfrPublicKey, Delegation>,
) -> Vec<(XfrPublicKey, XfrPublicKey, u64)> {
    let mut results = vec![];
    for (pk, d) in delegations.into_iter() {
        for (v, am) in d.delegations {
            if am != 0 {
                results.push((pk, v, am));
            }
        }
    }
    return results;
}
