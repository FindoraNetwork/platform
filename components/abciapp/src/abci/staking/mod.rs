//!
//! # Staking
//!
//! Business logic based on [**Ledger Staking**](ledger::staking).
//!

use crate::abci::server::callback::TENDERMINT_BLOCK_HEIGHT;
use abci::{Evidence, Header, LastCommitInfo, PubKey, ValidatorUpdate};
use lazy_static::lazy_static;
use ledger::{
    data_model::{Operation, Transaction, ASSET_TYPE_FRA},
    staking::{
        ops::{
            governance::{governance_penalty_tendermint_auto, ByzantineKind},
            mint_fra::{MintEntry, MintFraOps, MintKind},
        },
        td_addr_to_string, Staking, VALIDATOR_UPDATE_BLOCK_ITV,
    },
    store::{LedgerAccess, LedgerUpdate},
};
use rand_core::{CryptoRng, RngCore};
use ruc::*;
use serde::Serialize;
use std::{
    collections::{HashMap, HashSet},
    sync::atomic::Ordering,
};

mod whoami;

// The top 50~ candidate validators
// will become official validators.
const VALIDATOR_LIMIT: usize = 58;

lazy_static! {
    /// Tendermint node address, sha256(pubkey)[:20]
    pub static ref TD_NODE_SELF_ADDR: Vec<u8> = pnk!(whoami::get_self_addr());
}

/// Get the effective validators at current block height.
///
/// > #### Tendermint Rules
/// >
/// > Validator updates returned by block H impact blocks H+1, H+2, and H+3,
/// > but only effects changes on the validator set of H+2:
/// > - H+1: NextValidatorsHash
/// > - H+2: ValidatorsHash (and thus the validator set)
/// > - H+3: LastCommitInfo (ie. the last validator set)
/// > - Consensus params returned for block H apply for block H+1
/// >
/// > The pub_key currently supports only one type:
/// > - type = "ed25519"
/// >
/// > The power is the new voting power for the validator, with the following rules:
/// > - power must be non-negative
/// >   - if power is 0, the validator must already exist, and will be removed from the validator set
/// >   - if power is non-0:
/// >     - if the validator does not already exist, it will be added to the validator set with the given power
/// >     - if the validator does already exist, its power will be adjusted to the given power
/// > - the total power of the new validator set must not exceed MaxTotalVotingPower
pub fn get_validators(
    staking: &Staking,
    last_commit_info: Option<&LastCommitInfo>,
) -> Result<Option<Vec<ValidatorUpdate>>> {
    // Update the validator list every 4 blocks to ensure that
    // the validator list obtained from `LastCommitInfo` is exactly
    // the same as the current block.
    // So we can use it to filter out non-existing entries.
    if 0 != TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed) % VALIDATOR_UPDATE_BLOCK_ITV
    {
        return Ok(None);
    }

    // Get existing entries in the last block.
    let last_entries = if let Some(lci) = last_commit_info {
        lci.votes
            .as_slice()
            .iter()
            .flat_map(|v| v.validator.as_ref().map(|v| (&v.address, v.power)))
            .collect::<HashMap<_, _>>()
    } else {
        map! {}
    };

    // The logic of the context guarantees:
    // - current entries == last entries
    let cur_entries = last_entries;

    let mut vs = staking
        .validator_get_current()
        .c(d!())?
        .body
        .values()
        .filter(|v| {
            if let Some(power) = cur_entries.get(&v.td_addr) {
                // - new power > 0: change existing entries
                // - new power = 0: remove existing entries
                // - the power returned by `LastCommitInfo` is impossible
                // to be zero in the context of tendermint
                *power as u64 != v.td_power
            } else {
                // add new validator
                //
                // try to remove non-existing entries is not allowed
                0 < v.td_power
            }
        })
        // this conversion is safe in the context of tendermint
        .map(|v| (&v.td_pubkey, v.td_power as i64))
        .collect::<Vec<_>>();

    if vs.is_empty() {
        return Ok(None);
    }

    // reverse sort
    vs.sort_by(|a, b| b.1.cmp(&a.1));

    // set the power of every extra validators to zero,
    // then tendermint can remove them from consensus logic.
    vs.iter_mut().skip(VALIDATOR_LIMIT).for_each(|(k, power)| {
        alt!(cur_entries.contains_key(k), *power = 0, *power = -1);
    });

    Ok(Some(
        vs.iter()
            .filter(|(_, power)| -1 < *power)
            .map(|(pubkey, power)| {
                let mut vu = ValidatorUpdate::new();
                let mut pk = PubKey::new();
                pk.set_field_type("ed25519".to_owned());
                pk.set_data(pubkey.to_vec());
                vu.set_power(*power);
                vu.set_pub_key(pk);
                vu
            })
            .collect(),
    ))
}

// Call this function in `EndBlock`,
// - pay delegation rewards
// - pay proposer rewards(traditional block rewards)
// - do governance operations
pub fn system_ops<RNG: RngCore + CryptoRng>(
    la: &mut (impl LedgerAccess + LedgerUpdate<RNG>),
    header: &Header,
    last_commit_info: Option<&LastCommitInfo>,
    evs: &[Evidence],
) {
    // trigger system staking process
    la.get_staking_mut().delegation_process();
    la.get_staking_mut().validator_apply_current();

    ruc::info_omit!(set_rewards(
        la,
        &header.proposer_address,
        last_commit_info.map(|lci| get_last_vote_percent(lci))
    ));

    // tendermint primary governances
    evs.iter()
        .filter(|ev| ev.validator.is_some())
        .for_each(|ev| {
            let v = ev.validator.as_ref().unwrap();
            let bz = ByzantineInfo {
                addr: &td_addr_to_string(&v.address),
                kind: ev.field_type.as_str(),
            };

            ruc::info_omit!(system_governance(la.get_staking_mut(), &bz));
        });

    // application custom governances
    if let Some(lci) = last_commit_info {
        let online_list = lci
            .votes
            .iter()
            .filter(|v| v.signed_last_block)
            .flat_map(|info| info.validator.as_ref().map(|v| &v.address))
            .collect::<HashSet<_>>();

        let staking = la.get_staking_mut();

        // mark if a validator is online at last block
        if let Ok(vd) = ruc::info!(staking.validator_get_current_mut()) {
            vd.body.values_mut().for_each(|v| {
                if online_list.contains(&v.td_addr) {
                    v.signed_last_block = true;
                    v.signed_cnt += 1;
                } else {
                    v.signed_last_block = false;
                }
            });
        }

        if online_list.len() != lci.votes.len() {
            if let Ok(pl) = ruc::info!(gen_offline_punish_list(staking, &online_list)) {
                pl.into_iter().for_each(|v| {
                    let bz = ByzantineInfo {
                        addr: &td_addr_to_string(&v),
                        kind: "OFF_LINE",
                    };
                    ruc::info_omit!(system_governance(la.get_staking_mut(), &bz));
                });
            }
        }
    }
}

// Get the actual voted power of last block.
fn get_last_vote_percent(last_commit_info: &LastCommitInfo) -> [u64; 2] {
    last_commit_info
        .votes
        .iter()
        .flat_map(|info| {
            info.validator
                .as_ref()
                .map(|v| [alt!(info.signed_last_block, v.power, 0), v.power])
        })
        .fold([0, 0], |mut acc, i| {
            // this `AddAsign` is safe in the context of tendermint
            acc[0] += i[0] as u64;
            acc[1] += i[1] as u64;
            acc
        })
}

// Set delegation rewards and proposer rewards
fn set_rewards<RNG: RngCore + CryptoRng>(
    la: &mut (impl LedgerUpdate<RNG> + LedgerAccess),
    proposer: &[u8],
    last_vote_percent: Option<[u64; 2]>,
) -> Result<()> {
    Staking::set_last_block_rewards(la, &td_addr_to_string(proposer), last_vote_percent)
        .c(d!())
}

#[derive(Serialize)]
struct ByzantineInfo<'a> {
    addr: &'a str,
    // - "UNKNOWN"
    // - "DUPLICATE_VOTE"
    // - "LIGHT_CLIENT_ATTACK"
    kind: &'a str,
}

// Auto governance.
fn system_governance(staking: &mut Staking, bz: &ByzantineInfo) -> Result<()> {
    ruc::pd!(serde_json::to_string(&bz).unwrap());
    let kind = match bz.kind {
        "DUPLICATE_VOTE" => ByzantineKind::DuplicateVote,
        "LIGHT_CLIENT_ATTACK" => ByzantineKind::LightClientAttack,
        "OFF_LINE" => ByzantineKind::OffLine,
        "UNKNOWN" => ByzantineKind::Unknown,
        _ => return Err(eg!()),
    };
    governance_penalty_tendermint_auto(staking, bz.addr, &kind).c(d!())
}

/// Pay for freed 'Delegations' and 'FraDistributions'.
pub fn system_mint_pay<RNG: RngCore + CryptoRng>(
    la: &(impl LedgerAccess + LedgerUpdate<RNG>),
) -> Option<Transaction> {
    let staking = la.get_staking();
    let mut limit = staking.coinbase_balance() as i128;

    // at most `NUM_TO_PAY` items to pay per block
    const NUM_TO_PAY: usize = 2048;

    let mint_entries = staking
        .delegation_get_global_principal_with_receiver()
        .into_iter()
        .map(|(k, (n, receiver_pk))| {
            MintEntry::new(MintKind::UnStake, k, receiver_pk, n, ASSET_TYPE_FRA)
        })
        .chain(
            staking
                .delegation_get_global_rewards()
                .into_iter()
                .chain(
                    staking
                        .fra_distribution_get_plan()
                        .iter()
                        .map(|(k, n)| (*k, *n)),
                )
                .take_while(|(_, n)| {
                    limit -= *n as i128;
                    limit >= 0
                })
                .map(|(k, n)| {
                    MintEntry::new(MintKind::Claim, k, None, n, ASSET_TYPE_FRA)
                }),
        )
        .take(NUM_TO_PAY)
        .collect::<Vec<_>>();

    if mint_entries.is_empty() {
        None
    } else {
        let mint_ops =
            Operation::MintFra(MintFraOps::new(staking.cur_height(), mint_entries));
        Some(Transaction::from_operation_coinbase_mint(
            mint_ops,
            la.get_state_commitment().1,
        ))
    }
}

fn gen_offline_punish_list(
    staking: &Staking,
    online_list: &HashSet<&Vec<u8>>,
) -> Result<Vec<Vec<u8>>> {
    let last_height = TENDERMINT_BLOCK_HEIGHT
        .load(Ordering::Relaxed)
        .saturating_sub(1);
    let validators = staking
        .validator_get_effective_at_height(last_height as u64)
        .c(d!())?;

    let mut vs = validators
        .body
        .values()
        .map(|v| (&v.td_addr, v.td_power))
        .collect::<Vec<_>>();
    vs.sort_by(|a, b| b.1.cmp(&a.1));
    vs.iter_mut().skip(VALIDATOR_LIMIT).for_each(|(_, power)| {
        *power = 0;
    });

    Ok(vs
        .into_iter()
        .filter(|v| 0 < v.1 && !online_list.contains(&v.0))
        .map(|(id, _)| id.clone())
        .collect())
}

#[cfg(test)]
#[cfg(feature = "abci_mock")]
pub mod abci_mock_test;

#[cfg(test)]
mod test {
    use ledger::{
        data_model::{
            Transaction, TransferType, TxnEffect, TxoRef, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY, TX_FEE_MIN,
        },
        staking::{Staking, FF_PK_LIST, FRA_TOTAL_AMOUNT},
        store::{fra_gen_initial_tx, LedgerAccess, LedgerState, LedgerUpdate},
    };
    use rand::random;
    use rand_chacha::ChaChaRng;
    use rand_core::SeedableRng;
    use ruc::*;
    use txn_builder::{
        BuildsTransactions, TransactionBuilder, TransferOperationBuilder,
    };
    use zei::xfr::{
        asset_record::{open_blind_asset_record, AssetRecordType},
        sig::{XfrKeyPair, XfrPublicKey},
        structs::{AssetRecordTemplate, XfrAmount},
    };

    #[test]
    fn staking_block_rewards_rate() {
        pnk!(check_block_rewards_rate());
    }

    // 1. create a ledger instance
    // 2. define and issue FRAs
    // 3. transfer to some addresses of the 9 reserved accounts looply
    // 4. check if the block rewards rate is correct per loop
    fn check_block_rewards_rate() -> Result<()> {
        let mut ledger = LedgerState::test_ledger();
        let root_kp = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

        let tx = fra_gen_initial_tx(&root_kp);

        let effect = TxnEffect::compute_effect(tx).c(d!())?;
        let mut block = ledger.start_block().c(d!())?;
        ledger
            .apply_transaction(&mut block, effect, false)
            .c(d!())?;
        ledger.finish_block(block).c(d!())?;

        // set a fake delegation amount
        *ledger.get_staking_mut().get_global_delegation_amount_mut() =
            FRA_TOTAL_AMOUNT / 200;

        let mut seq_id = 1;
        let mut prev_rate = [2, 1];
        for i in 1..200 {
            let tx = gen_transfer_tx(
                &ledger,
                &root_kp,
                &FF_PK_LIST[random::<usize>() % FF_PK_LIST.len()],
                FRA_TOTAL_AMOUNT / 200,
                seq_id,
            )
            .c(d!())?;

            let effect = TxnEffect::compute_effect(tx).c(d!())?;
            let mut block = ledger.start_block().c(d!())?;
            ledger
                .apply_transaction(&mut block, effect, false)
                .c(d!())?;
            ledger.finish_block(block).c(d!())?;

            {
                let rate = Staking::get_block_rewards_rate(&ledger);
                let rate = [rate[0] as u128, rate[1] as u128];
                // max value: 105%
                assert!(rate[0] * 100 <= rate[1] * 105);
                // min value: 2%
                assert!(rate[0] * 100 >= rate[1] * 2);

                if 1 == i {
                    assert_eq!(rate[0] * 100, rate[1] * 105);
                } else if 499 == i {
                    assert_eq!(rate[0] * 100, rate[1] * 2);
                }

                // more locked percent, less return rate
                if rate[0] * 100 != rate[1] * 105 && rate[0] * 100 != rate[1] * 2 {
                    assert!(rate[0] * prev_rate[1] < rate[1] * prev_rate[0]);
                }

                prev_rate = rate;
            }

            seq_id += 1;
        }

        Ok(())
    }

    fn gen_transfer_tx(
        la: &LedgerState,
        owner_kp: &XfrKeyPair,
        target_pk: &XfrPublicKey,
        am: u64,
        seq_id: u64,
    ) -> Result<Transaction> {
        let mut tx_builder = TransactionBuilder::from_seq_id(seq_id);

        let target_list = vec![(target_pk, am), (&*BLACK_HOLE_PUBKEY, TX_FEE_MIN)];

        let mut trans_builder = TransferOperationBuilder::new();

        let mut am = target_list.iter().map(|(_, am)| *am).sum();
        let mut i_am;
        let utxos = la.get_owned_utxos(owner_kp.get_pk_ref()).into_iter();

        for (sid, (utxo, owner_memo)) in utxos {
            if let XfrAmount::NonConfidential(n) = utxo.0.record.amount {
                alt!(n < am, i_am = n, i_am = am);
                am = am.saturating_sub(n);
            } else {
                continue;
            }

            open_blind_asset_record(&utxo.0.record, &owner_memo, owner_kp)
                .c(d!())
                .and_then(|ob| {
                    trans_builder
                        .add_input(TxoRef::Absolute(sid), ob, None, None, i_am)
                        .c(d!())
                })?;

            alt!(0 == am, break);
        }

        if 0 != am {
            return Err(eg!("insufficient balance"));
        }

        let outputs = target_list.into_iter().map(|(pk, n)| {
            AssetRecordTemplate::with_no_asset_tracing(
                n,
                ASSET_TYPE_FRA,
                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                *pk,
            )
        });

        for output in outputs {
            trans_builder
                .add_output(&output, None, None, None)
                .c(d!())?;
        }

        let op = trans_builder
            .balance()
            .c(d!())?
            .create(TransferType::Standard)
            .c(d!())?
            .sign(owner_kp)
            .c(d!())?
            .transaction()
            .c(d!())?;

        tx_builder.add_operation(op);
        Ok(tx_builder.take_transaction())
    }
}
