//!
//! # Staking
//!
//! Business logic based on [**Ledger Staking**](ledger::staking).
//!

use ledger::data_model::{
    AssetType, AssetTypeCode, IssuerPublicKey, BLACK_HOLE_PUBKEY_STAKING,
};

mod whoami;

#[cfg(test)]
mod test;

use {
    crate::abci::server::callback::TENDERMINT_BLOCK_HEIGHT,
    abci::{Evidence, Header, LastCommitInfo, PubKey, ValidatorUpdate},
    baseapp::BaseApp as AccountBaseApp,
    lazy_static::lazy_static,
    ledger::{
        data_model::{Operation, Transaction, ASSET_TYPE_FRA},
        staking::{
            ops::{
                governance::{governance_penalty_tendermint_auto, ByzantineKind},
                mint_fra::{MintEntry, MintFraOps, MintKind},
            },
            td_addr_to_string, Staking, VALIDATOR_UPDATE_BLOCK_ITV,
        },
        store::LedgerState,
    },
    ruc::*,
    serde::Serialize,
    std::{
        collections::{BTreeMap, BTreeSet},
        ops::{Deref, DerefMut},
        sync::atomic::Ordering,
    },
};

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
            .collect::<BTreeMap<_, _>>()
    } else {
        BTreeMap::new()
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

/// Call this function in `EndBlock`,
/// - pay delegation rewards
/// - pay proposer rewards(traditional block rewards)
/// - do governance operations
pub fn system_ops(
    la: &mut LedgerState,
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
        last_commit_info.map(get_last_vote_percent)
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

            ruc::info_omit!(system_governance(la.get_staking_mut().deref_mut(), &bz));
        });

    // application custom governances
    if let Some(lci) = last_commit_info {
        let online_list = lci
            .votes
            .iter()
            .filter(|v| v.signed_last_block)
            .flat_map(|info| info.validator.as_ref().map(|v| &v.address))
            .collect::<BTreeSet<_>>();

        // mark if a validator is online at last block
        if let Ok(vd) = ruc::info!(la.get_staking_mut().validator_get_current_mut()) {
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
            if let Ok(pl) = ruc::info!(gen_offline_punish_list(
                la.get_staking().deref(),
                &online_list
            )) {
                pl.into_iter().for_each(|v| {
                    let bz = ByzantineInfo {
                        addr: &td_addr_to_string(&v),
                        kind: "OFF_LINE",
                    };
                    ruc::info_omit!(system_governance(
                        la.get_staking_mut().deref_mut(),
                        &bz
                    ));
                });
            }
        }
    }
}

/// Get the actual voted power of last block.
fn get_last_vote_percent(last_commit_info: &LastCommitInfo) -> [u64; 2] {
    // Returns Voted in last block and Total Voting power (including signed_last_block = false)
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

/// Set delegation rewards and proposer rewards
fn set_rewards(
    la: &mut LedgerState,
    proposer: &[u8],
    last_vote_percent: Option<[u64; 2]>,
) -> Result<()> {
    la.staking_set_last_block_rewards(&td_addr_to_string(proposer), last_vote_percent)
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

/// Auto governance.
fn system_governance(staking: &mut Staking, bz: &ByzantineInfo) -> Result<()> {
    // ruc::pd!(serde_json::to_string(&bz).unwrap());
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
pub fn system_prism_mint_pay(
    la: &mut LedgerState,
    account_base_app: &mut AccountBaseApp,
) -> Option<Transaction> {
    let mut mints = Vec::new();

    if let Some(account_mint) = account_base_app.consume_mint() {
        for mint in account_mint {
            if mint.asset != ASSET_TYPE_FRA {
                let atc = AssetTypeCode { val: mint.asset };
                let at = if let Some(mut at) = la.get_asset_type(&atc) {
                    at.properties.issuer = IssuerPublicKey {
                        key: *BLACK_HOLE_PUBKEY_STAKING,
                    };
                    if mint.max_supply != 0 {
                        at.properties.asset_rules.max_units = Some(mint.max_supply);
                    }
                    at
                } else {
                    let mut at = AssetType::default();
                    at.properties.issuer = IssuerPublicKey {
                        key: *BLACK_HOLE_PUBKEY_STAKING,
                    };

                    if mint.max_supply != 0 {
                        at.properties.asset_rules.max_units = Some(mint.max_supply);
                    }

                    at.properties.code = AssetTypeCode { val: mint.asset };

                    at
                };

                la.insert_asset_type(atc, at);
            }

            let mint_entry = MintEntry::new(
                MintKind::Other,
                mint.target,
                None,
                mint.amount,
                mint.asset,
            );

            mints.push(mint_entry);
        }
    }

    if mints.is_empty() {
        None
    } else {
        let mint_ops =
            Operation::MintFra(MintFraOps::new(la.get_staking().cur_height(), mints));
        Some(Transaction::from_operation_coinbase_mint(
            mint_ops,
            la.get_state_commitment().1,
        ))
    }
}

/// Pay for freed 'Delegations' and 'FraDistributions'.
pub fn system_mint_pay(la: &LedgerState) -> Option<Transaction> {
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

/// filtering online lists from staking's validators
fn gen_offline_punish_list(
    staking: &Staking,
    online_list: &BTreeSet<&Vec<u8>>,
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
