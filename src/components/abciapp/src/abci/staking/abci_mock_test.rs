//!
//! # Integration Testing
//!

#![allow(warnings)]
#![allow(missing_docs)]

use crate::abci::server::{
    callback::TENDERMINT_BLOCK_HEIGHT,
    tx_sender::{forward_txn_with_mode, CHAN},
    ABCISubmissionServer,
};
use abci::*;
use cryptohash::sha256::{self, Digest};
use finutils::{
    api::DelegationInfo,
    txn_builder::{TransactionBuilder, TransferOperationBuilder},
};
use globutils::{fresh_tmp_dir, wallet};
use lazy_static::lazy_static;
use ledger::{
    data_model::{
        Operation, Transaction, TransferType, TxoRef, TxoSID, Utxo, ASSET_TYPE_FRA,
        BLACK_HOLE_PUBKEY, BLACK_HOLE_PUBKEY_STAKING, TX_FEE_MIN,
    },
    staking::{
        calculate_delegation_rewards,
        ops::{
            governance::ByzantineKind,
            mint_fra::{MintEntry, MintFraOps, MintKind},
        },
        td_addr_to_bytes, td_addr_to_string, td_pubkey_to_td_addr, Delegation,
        DelegationState, PartialUnDelegation, TendermintAddr,
        Validator as StakingValidator, ValidatorKind, BLOCK_HEIGHT_MAX, FRA,
        FRA_TOTAL_AMOUNT, UNBOND_BLOCK_CNT,
    },
    store::utils::fra_gen_initial_tx,
};
use parking_lot::{Mutex, RwLock};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ruc::*;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Deref;
use std::path::Path;
use std::{
    collections::BTreeMap,
    convert::TryFrom,
    env, mem,
    sync::{
        atomic::{AtomicI64, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc,
    },
    thread,
    time::Duration,
};
use zei::xfr::{
    asset_record::{open_blind_asset_record, AssetRecordType},
    sig::{XfrKeyPair, XfrPublicKey},
    structs::{AssetRecordTemplate, OwnerMemo, XfrAmount},
};

lazy_static! {
    static ref INITIAL_KEYPAIR_LIST: Vec<XfrKeyPair> = pnk!(gen_initial_keypair_list());
    static ref ABCI_MOCKER: Arc<RwLock<AbciMocker>> =
        Arc::new(RwLock::new(AbciMocker::new()));
    static ref TD_MOCKER: Arc<RwLock<TendermintMocker>> =
        Arc::new(RwLock::new(TendermintMocker::new()));
    static ref FAILED_TXS: Arc<RwLock<BTreeMap<Digest, Transaction>>> =
        Arc::new(RwLock::new(map! {B}));
    static ref SUCCESS_TXS: Arc<RwLock<BTreeMap<Digest, Transaction>>> =
        Arc::new(RwLock::new(map! {B}));
    static ref ROOT_KEYPAIR: XfrKeyPair = gen_keypair();
}

const INITIAL_MNEMONIC: [&str; 20] = [
    "portion dwarf silk physical critic jacket express action okay reject power draw neither addict tumble panda filter crawl path obscure merry proof end liberty",
    "flat fiction patrol wheel lion pulp option cupboard super drum birth lava wedding quote noise room warm life path minor find mobile rice promote",
    "nest genuine open merit metal night song photo child congress kiss assume perfect rice radio option afford cream library valley cancel curtain keen pumpkin",
    "such similar city scene bamboo warfare inner novel soccer drift promote runway cruise list rule payment filter tomato scene verb dance portion save eye",
    "uphold cushion dutch nice album truth target name antique pond number milk wire industry current urge able memory arrange device welcome true alter clean",
    "east vicious lazy evolve tray minor hold despair tent orbit leisure invite squirrel puzzle arrest network hip club slight true leave tooth layer waste",
    "bring sister wise grant desert marriage enemy farm pledge cream amused claim bag refuse firm toddler empty bind derive prepare fabric best win lumber",
    "inflict artwork plate salad fitness ancient dress feed limb rescue another knock employ mirror garbage smooth walnut lottery busy street arrest zero fit gossip",
    "salute apology unhappy thought person assume rough only present web merry lazy remain giant pledge day noodle oppose connect skill strategy talk burst melt",
    "patient much snake tiny luxury surge health steak obey escape fee recall barrel era scan stem wire usual educate rookie blur fame pencil limit",
    "tonight tribe pair spare among trim cream diamond angry measure skin pencil dutch legal razor video dry decline stairs uncover kangaroo model kid sauce",
    "conduct fat execute jar deny wasp slam any know junk bronze damage trust relief mother apple chair pig embody adjust loud toast garbage april",
    "blanket index shell cave discover drink hint desk famous chef yard output ridge face lucky end unlock control aspect snow crane odor behave also",
    "unhappy truck item argue domain peace honey acoustic solar cry return butter live recipe rigid vivid skate detect student magnet holiday pond cabbage kiss",
    "still fluid antenna mother nominee napkin lottery crisp debris kit suit game bitter gesture return foam casino sample frog depart worry limit cram suit",
    "submit alpha dirt pulse acid leaf royal reward thunder purpose post frozen coral cross hidden bubble harvest rather flat cancel glory ugly egg differ",
    "poet october tank record scan grit ticket weather exotic total tennis better mountain melt fire then traffic assume suffer boring office produce journey useless",
    "fence census sunny manage nominee owner vital code tortoise foot choose cross wide capital lawsuit smooth ecology pause deposit worry mass limit crystal when",
    "matrix uncle bachelor aunt lazy museum cancel feel also bundle gospel analyst index cereal move tower lion buyer long connect circle balance accuse valid",
    "clerk purpose acid rail invite stone raccoon pottery blame harbor dawn wrap cluster relief account law angle warm bullet great auction naive moral cloth",
];

const ITV: u64 = 10;
const INITIAL_POWER: u64 = 1_0000 * FRA;

struct AbciMocker(ABCISubmissionServer);

impl AbciMocker {
    fn new() -> AbciMocker {
        AbciMocker(pnk!(ABCISubmissionServer::new(None, String::new())))
    }

    fn new_with_path(path: Option<&str>) -> AbciMocker {
        if let Some(s) = path {
            let base_dir = Path::new(s);
            AbciMocker(pnk!(ABCISubmissionServer::new(
                Some(base_dir),
                String::new()
            )))
        } else {
            AbciMocker::new()
        }
    }

    fn produce_block(&mut self) {
        // do not generate empty blocks,
        // in order to reduce error messages
        let txs = CHAN.1.lock().try_iter().collect::<Vec<_>>();
        alt!(txs.is_empty(), return);

        let h = 1 + TENDERMINT_BLOCK_HEIGHT.fetch_add(1, Ordering::Relaxed);
        let proposer = pnk!(td_addr_to_bytes(
            &TD_MOCKER.read().validators.keys().next().unwrap()
        ));

        self.0.begin_block(&gen_req_begin_block(h, proposer));

        let mut failed_txs = FAILED_TXS.write();
        let mut successful_txs = SUCCESS_TXS.write();

        for tx in txs.into_iter() {
            let key = gen_tx_hash(&tx);
            if 0 == self.0.deliver_tx(&gen_req_deliver_tx(tx.clone())).code {
                assert!(successful_txs.insert(key, tx).is_none());
            } else {
                assert!(failed_txs.insert(key, tx).is_none());
            }
        }
        drop(failed_txs);
        drop(successful_txs);

        let resp = self.0.end_block(&gen_req_end_block(h.saturating_sub(20)));
        if 0 < resp.validator_updates.len() {
            TD_MOCKER.write().validators = resp
                .validator_updates
                .into_vec()
                .into_iter()
                .filter(|v| 0 < v.power)
                .filter_map(|v| {
                    v.pub_key
                        .as_ref()
                        .map(|pk| (td_pubkey_to_td_addr(pk.get_data()), v.power as u64))
                })
                .collect();
        }

        self.0.commit(&gen_req_commit());
    }

    fn get_owned_utxos(
        &self,
        addr: &XfrPublicKey,
    ) -> BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)> {
        pnk!(self
            .0
            .la
            .read()
            .get_committed_state()
            .read()
            .get_owned_utxos(addr))
    }

    fn get_owned_balance(&self, addr: &XfrPublicKey) -> u64 {
        self.get_owned_utxos(addr)
            .values()
            .map(|(utxo, _)| {
                if let XfrAmount::NonConfidential(am) = utxo.0.record.amount {
                    am
                } else {
                    0
                }
            })
            .sum()
    }

    fn get_owned_reward(&self, addr: &XfrPublicKey) -> u64 {
        self.0
            .la
            .read()
            .get_committed_state()
            .read()
            .get_staking()
            .delegation_get_rewards(addr)
            .unwrap()
    }

    fn get_owned_delegation_principal(&self, addr: &XfrPublicKey) -> u64 {
        self.0
            .la
            .read()
            .get_committed_state()
            .read()
            .get_staking()
            .delegation_get_principal(addr)
            .unwrap_or_else(|b| {
                d!(b);
                0_u64
            })
    }

    fn get_current_rate(&self) -> [u128; 2] {
        let sub = self.0.la.read();
        let la = sub.get_committed_state().read();
        la.staking_get_block_rewards_rate()
    }

    fn get_current_rate_str(&self) -> String {
        let v = self.get_current_rate();
        f64::try_from(i32::try_from(v[0]).unwrap())
            .c(d!("overflow"))
            .map(|molecular| {
                f64::try_from(i32::try_from(v[1]).unwrap())
                    .c(d!("overflow"))
                    .map(|denominator| format!("{:.2}%", (molecular / denominator)))
                    .unwrap()
            })
            .c(d!())
            .unwrap()
    }

    fn calculate_delegation_rewards_for_one_block(&self, addr: &XfrPublicKey) -> u64 {
        let principal = self.get_owned_delegation_principal(addr) as u128;

        let rate = self.get_current_rate();

        principal
            .checked_mul(rate[0])
            .and_then(|i| i.checked_div(rate[1]))
            .c(d!("overflow"))
            .and_then(|n| u64::try_from(n).c(d!()))
            .unwrap()
    }

    fn get_delegation_by_addr(&self, addr: &XfrPublicKey) -> Option<Delegation> {
        let op = self
            .0
            .la
            .read()
            .get_committed_state()
            .read()
            .get_staking()
            .delegation_get(addr)
            .cloned();
        op
    }

    fn get_validators_by_delegation_addr(
        &self,
        addr: &XfrPublicKey,
    ) -> Option<DelegationInfo> {
        self.get_delegation_by_addr(addr).map(|d| {
            let sub = self.0.la.read();

            let la = sub.get_committed_state().read();

            let staking = la.get_staking();

            let bond_entries: Vec<(String, u64)> = d
                .entries
                .iter()
                .filter_map(|(v_pk, am)| {
                    staking
                        .validator_app_pk_to_td_addr(v_pk)
                        .ok()
                        .map(|addr| (addr, *am))
                })
                .collect();
            let mut bond_amount = d.amount();
            let mut unbond_amount = 0;

            match d.state {
                DelegationState::Paid => {
                    bond_amount = 0;
                }
                DelegationState::Free => {
                    mem::swap(&mut bond_amount, &mut unbond_amount);
                }
                DelegationState::Bond => {
                    if staking.cur_height()
                        > d.end_height().saturating_sub(UNBOND_BLOCK_CNT)
                    {
                        mem::swap(&mut bond_amount, &mut unbond_amount);
                    }
                }
            }

            let mut di = DelegationInfo::new(
                bond_amount,
                bond_entries,
                unbond_amount,
                d.rwd_amount,
                self.get_current_rate(),
                staking.delegation_info_global_amount(),
                staking.validator_global_power(),
            );
            di.start_height = d.start_height();
            di.current_height = staking.cur_height();
            di.end_height = d.end_height;
            di.delegation_rwd_cnt = d.delegation_rwd_cnt;
            di.proposer_rwd_cnt = d.proposer_rwd_cnt;

            di
        })
    }

    fn get_validator_power(&self, addr: &XfrPublicKey) -> Result<u64> {
        self.0
            .la
            .read()
            .get_committed_state()
            .read()
            .get_staking()
            .validator_get_power(addr)
            .c(d!("addr not exist"))
    }

    fn get_delegation_power(
        &self,
        addr: &XfrPublicKey,
    ) -> Option<HashMap<XfrPublicKey, u64>> {
        self.get_delegation_by_addr(addr).map(|d| {
            d.entries
                .iter()
                .filter_map(|(v_pk, am)| {
                    let power = pnk!(self.get_validator_power(v_pk).c(d!()));
                    Some((v_pk.clone(), power))
                })
                .collect::<HashMap<XfrPublicKey, u64>>()
        })
    }
}

pub struct TendermintMocker {
    validators: BTreeMap<String, u64>,
}

impl TendermintMocker {
    fn new() -> TendermintMocker {
        thread::spawn(move || loop {
            thread::sleep(Duration::from_millis(ITV));
            ABCI_MOCKER.write().produce_block();
        });

        TendermintMocker {
            validators: map! {B td_addr_to_string(&[0; 20]) => 1 },
        }
    }

    fn clean(&mut self) {
        CHAN.1.lock().try_iter().for_each(|_| {});
        self.validators = map! {B td_addr_to_string(&[0; 20]) => 1 };
    }
}

fn gen_initial_keypair_list() -> Result<Vec<XfrKeyPair>> {
    INITIAL_MNEMONIC
        .iter()
        .map(|m| wallet::restore_keypair_from_mnemonic_default(m).c(d!()))
        .collect::<Result<Vec<_>>>()
}

fn gen_req_begin_block(h: i64, proposer: Vec<u8>) -> RequestBeginBlock {
    let mut header = Header::new();
    header.set_height(h);
    header.set_proposer_address(proposer);

    let mut res = RequestBeginBlock::new();
    res.set_header(header);

    res
}

fn gen_req_deliver_tx(tx: Transaction) -> RequestDeliverTx {
    let mut res = RequestDeliverTx::new();
    res.set_tx(pnk!(serde_json::to_vec(&tx)));
    res
}

fn gen_req_end_block(h: i64) -> RequestEndBlock {
    let mut req = RequestEndBlock::new();
    req.height = h;
    req
}

fn gen_req_commit() -> RequestCommit {
    RequestCommit::new()
}

fn gen_tx_hash(tx: &Transaction) -> Digest {
    sha256::hash(&pnk!(bincode::serialize(tx)))
}

fn gen_keypair() -> XfrKeyPair {
    XfrKeyPair::generate(&mut ChaChaRng::from_entropy())
}

fn get_owned_utxos(pk: &XfrPublicKey) -> BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)> {
    ABCI_MOCKER.read().get_owned_utxos(pk)
}

pub fn gen_transfer_op(
    owner_kp: &XfrKeyPair,
    mut target_list: Vec<(&XfrPublicKey, u64)>,
) -> Result<Operation> {
    // auto fee
    target_list.push((&*BLACK_HOLE_PUBKEY, TX_FEE_MIN));

    let mut trans_builder = TransferOperationBuilder::new();

    let mut am = target_list.iter().map(|(_, am)| *am).sum();
    let mut i_am;
    let utxos = get_owned_utxos(owner_kp.get_pk_ref()).into_iter();

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
        return Err(eg!());
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

    trans_builder
        .balance()
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(owner_kp)
        .c(d!())?
        .transaction()
        .c(d!())
}

fn new_tx_builder() -> TransactionBuilder {
    let h = TENDERMINT_BLOCK_HEIGHT.load(Ordering::Relaxed) as u64;
    TransactionBuilder::from_seq_id(get_seq_id())
}

fn get_seq_id() -> u64 {
    ABCI_MOCKER
        .read()
        .0
        .la
        .read()
        .get_committed_state()
        .read()
        .get_state_commitment()
        .1
}

fn gen_fee_op(owner_kp: &XfrKeyPair) -> Result<Operation> {
    gen_transfer_op(owner_kp, vec![]).c(d!())
}

fn gen_new_validators(n: u8) -> (Vec<StakingValidator>, Vec<XfrKeyPair>) {
    let kps = (0..n).map(|_| gen_keypair()).collect::<Vec<_>>();

    // make sure the sequence is same as them in BTreeMap
    let td_pubkey_bytes = (0..n)
        .map(|i| vec![i; 32])
        .map(|k| (td_pubkey_to_td_addr(&k), k))
        .collect::<BTreeMap<_, _>>();

    let v_set = td_pubkey_bytes
        .into_iter()
        .map(|(_, td_pk)| td_pk)
        .zip(kps.iter())
        .map(|(td_pk, kp)| {
            pnk!(StakingValidator::new(
                td_pk,
                INITIAL_POWER,
                kp.get_pk(),
                [50, 100],
                Default::default(),
                ValidatorKind::Staker
            ))
        })
        .collect::<Vec<_>>();

    (v_set, kps)
}

fn governance(
    owner_kp: &XfrKeyPair,
    cosig_kps: &[&XfrKeyPair],
    byzantine_id: XfrPublicKey,
    kind: ByzantineKind,
) -> Result<Digest> {
    governance_x(owner_kp, cosig_kps, byzantine_id, kind, None)
}

fn governance_x(
    owner_kp: &XfrKeyPair,
    cosig_kps: &[&XfrKeyPair],
    byzantine_id: XfrPublicKey,
    kind: ByzantineKind,
    custom_amount: Option<[u64; 2]>,
) -> Result<Digest> {
    let mut builder = new_tx_builder();

    builder
        .add_operation_governance(cosig_kps, byzantine_id, kind, custom_amount)
        .c(d!())
        .and_then(|b| {
            gen_fee_op(owner_kp)
                .c(d!())
                .map(move |op| b.add_operation(op))
        })?;

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn update_validator(
    owner_kp: &XfrKeyPair,
    cosig_kps: &[&XfrKeyPair],
    h: u64,
    v_set: Vec<StakingValidator>,
) -> Result<Digest> {
    let mut builder = new_tx_builder();

    builder
        .add_operation_update_validator(cosig_kps, h, v_set)
        .c(d!())
        .and_then(|b| {
            gen_fee_op(owner_kp)
                .c(d!())
                .map(move |op| b.add_operation(op))
        })?;

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn distribute_fra(
    owner_kp: &XfrKeyPair,
    cosig_kps: &[&XfrKeyPair],
    alloc_table: BTreeMap<XfrPublicKey, u64>,
) -> Result<Digest> {
    let mut builder = new_tx_builder();

    builder
        .add_operation_fra_distribution(cosig_kps, alloc_table)
        .c(d!())
        .and_then(|b| {
            gen_fee_op(owner_kp)
                .c(d!())
                .map(move |op| b.add_operation(op))
        })?;

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn delegate(
    owner_kp: &XfrKeyPair,
    validator: TendermintAddr,
    amount: u64,
) -> Result<Digest> {
    delegate_x(owner_kp, validator, amount, false).c(d!())
}

fn delegate_x(
    owner_kp: &XfrKeyPair,
    validator: TendermintAddr,
    mut amount: u64,
    is_evil: bool,
) -> Result<Digest> {
    let mut builder = new_tx_builder();
    builder.add_operation_delegation(owner_kp, validator);

    alt!(is_evil, amount = 1);

    builder.add_operation(
        gen_transfer_op(owner_kp, vec![(&BLACK_HOLE_PUBKEY_STAKING, amount)]).c(d!())?,
    );

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn undelegate(owner_kp: &XfrKeyPair) -> Result<Digest> {
    undelegate_x(owner_kp, None).c(d!())
}

fn undelegate_x(
    owner_kp: &XfrKeyPair,
    pu: Option<PartialUnDelegation>,
) -> Result<Digest> {
    let mut builder = new_tx_builder();
    builder.add_operation_undelegation(owner_kp, pu);

    gen_fee_op(owner_kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn claim(owner_kp: &XfrKeyPair, am: u64) -> Result<Digest> {
    let mut builder = new_tx_builder();
    builder.add_operation_claim(owner_kp, Some(am));

    gen_fee_op(owner_kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let tx = builder.take_transaction();
    let h = gen_tx_hash(&tx);
    send_tx(tx).c(d!()).map(|_| h)
}

fn gen_final_tx(ops: Vec<Operation>) -> Result<Transaction> {
    let mut builder = new_tx_builder();

    ops.into_iter().for_each(|op| {
        builder.add_operation(op);
    });

    Ok(builder.take_transaction())
}

fn send_tx(tx: Transaction) -> Result<()> {
    forward_txn_with_mode("", tx, true).c(d!())
}

fn transfer(owner_kp: &XfrKeyPair, target_pk: &XfrPublicKey, am: u64) -> Result<Digest> {
    transfer_batch(owner_kp, vec![(target_pk, am)]).c(d!())
}

fn transfer_batch(
    owner_kp: &XfrKeyPair,
    targets: Vec<(&XfrPublicKey, u64)>,
) -> Result<Digest> {
    gen_transfer_op(owner_kp, targets)
        .c(d!())
        .and_then(|op| gen_final_tx(vec![op]).c(d!()))
        .and_then(|tx| {
            let h = gen_tx_hash(&tx);
            send_tx(tx).c(d!()).map(|_| h)
        })
}

fn wait_one_block() {
    wait_n_block(1);
}

fn wait_n_block(n: u8) {
    (0..n).for_each(|_| {
        sleep_ms!(2 * ITV);
    });
}

fn is_successful(tx_hash: &Digest) -> bool {
    SUCCESS_TXS.read().contains_key(tx_hash) && !FAILED_TXS.read().contains_key(tx_hash)
}

fn is_failed(tx_hash: &Digest) -> bool {
    !SUCCESS_TXS.read().contains_key(tx_hash) && FAILED_TXS.read().contains_key(tx_hash)
}

fn env_refresh(validator_num: u8) {
    // make sure the sequence is same as them in BTreeMap
    let td_pubkey_bytes = (0..validator_num)
        .map(|i| vec![i; 32])
        .map(|k| (td_pubkey_to_td_addr(&k), k))
        .collect::<BTreeMap<_, _>>()
        .into_iter()
        .next()
        .unwrap()
        .1;

    env::set_var("TD_NODE_SELF_ADDR", td_pubkey_to_td_addr(&td_pubkey_bytes));
    *ABCI_MOCKER.write() = AbciMocker::new();
    TD_MOCKER.write().clean();
}

fn env_refresh_with_path(validator_num: u8, _path_str: String) {
    // make sure the sequence is same as them in BTreeMap
    let td_pubkey_bytes = (0..validator_num)
        .map(|i| vec![i; 32])
        .map(|k| (td_pubkey_to_td_addr(&k), k))
        .collect::<BTreeMap<_, _>>()
        .into_iter()
        .next()
        .unwrap()
        .1;

    env::set_var("TD_NODE_SELF_ADDR", td_pubkey_to_td_addr(&td_pubkey_bytes));

    *ABCI_MOCKER.write() = AbciMocker::new_with_path(None);

    TD_MOCKER.write().clean();
}

// mid-util:
// default send a tx to trigger next block
// if param not empty,trigger n(param) block
macro_rules! trigger_next_block {
    () => {
        let _ = transfer(&ROOT_KEYPAIR, &BLACK_HOLE_PUBKEY, 1).c(d!())?;
        wait_one_block();
    };
    ($n: expr) => {{
        for i in 0..$n {
            trigger_next_block!();
            wait_one_block();
        }
    }};
}

// Basic Scene Without Governance
//
// 0. issue FRA
// 1. update validators
// 2. ...........
// 3. transfer some FRAs to a new addr `x`
//
// 4. use `x` to propose a delegation, and make sure it will fail
// because that all validators have not done self-delegation
//
// 5. make validators to finish their self-delegations
//
// 6. use `x` to propose a delegation
// 7. make sure `x` can continue to propose new delegations
// 8. delegate to different validators
// 9. make sure `x` can do transfer
// 10. make sure the power of co-responding validator is increased
// 11. undelegate
// 12. make sure the power of co-responding validator is decreased
// 13. make sure delegation reward is calculated and paid correctly
//
// 14. use `x` to propose a new delegation
// 15. make sure it can do claim at any time
// 16. make sure it will fail if the claim amount is bigger than total rewards
//
// 17. undelegate and re-delegate at once
// 18. make sure the delegation state turn back to `Bond`
// 19. make sure no rewards will be paid
//
// 20. unstake validator and check if
// the principals of co-responding delegators will be paid back
//
// 21. undelegate partially
//
// 22. transfer FRAs to multi addrs
// 23. make sure the result is correct
// 24. use these addrs to delegate to different validators,
// 25. make sure the power of each validator is increased correctly
// 26. undelegate
// 27. make sure the power of each validator is decreased correctly
//
// 28. re-delegate those multi addrs one by one,
// make sure delegation-rewards-rate is correct in different global delegation levels
//
// 29. make sure the vote power of any vallidator can not exceed 20% of total power
//
// 30. make sure that ordinary users can NOT sent `MintFra` transactions
//
// 31. replay old transactions and make sure all of them is failed
fn staking_scene_1() -> Result<()> {
    const VALIDATORS_NUM: u8 = 11;

    env_refresh(VALIDATORS_NUM);

    // 0. issue FRA

    let tx = fra_gen_initial_tx(&ROOT_KEYPAIR);
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 1. update validators

    let (v_set, kps) = gen_new_validators(VALIDATORS_NUM);
    assert_eq!(v_set.len(), kps.len());

    // update validators at height 2
    let initial_keypairs = INITIAL_KEYPAIR_LIST.iter().collect::<Vec<_>>();
    let tx_hash =
        update_validator(&ROOT_KEYPAIR, &initial_keypairs, 2, v_set.clone()).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // validators will be updated every 4 blocks
    trigger_next_block!(3);

    let td_mocker = TD_MOCKER.read();
    let td_v_set = &td_mocker.validators;
    assert_eq!(v_set.len(), td_v_set.len());
    v_set.iter().for_each(|v| {
        assert_eq!(
            &INITIAL_POWER,
            pnk!(td_v_set.get(&td_pubkey_to_td_addr(&v.td_pubkey)))
        );
    });

    drop(td_mocker);

    // 2. .................

    // 3. transfer some FRAs to a new addr `x`

    let x_kp = gen_keypair();

    let tx_hash = transfer(&ROOT_KEYPAIR, x_kp.get_pk_ref(), 1_0000 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 4. use `x` to propose a delegation, and make sure that
    // it will fail because that all validators have not done self-delegation

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 32 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // undelegation will fail
    let tx_hash = undelegate(&x_kp).c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // 5. make validators to finish their self-delegations

    for (i, kp) in kps.iter().enumerate() {
        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 100 * FRA)
            .c(d!())?;
        wait_one_block();
        assert!(is_successful(&tx_hash));
    }

    // 6. use `x` to propose a delegation

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 32 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 7. make sure `x` can continue to propose new delegations

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 64 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 8. delegate to different validators

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[3].td_pubkey), 84 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 9. make sure `x` can do transfer

    let tx_hash = transfer(&x_kp, &BLACK_HOLE_PUBKEY, 1).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 10. make sure the power of co-responding validator is increased

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

    assert_eq!((32 + 64 + 100) * FRA + INITIAL_POWER, power);

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[3].id).c(d!()));

    assert_eq!((84 + 100) * FRA + INITIAL_POWER, power);

    // 11. undelegate

    let tx_hash = undelegate(&x_kp).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 12. make sure the power of co-responding validator is decreased

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

    assert_eq!(100 * FRA + INITIAL_POWER, power);

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[3].id).c(d!()));

    assert_eq!(100 * FRA + INITIAL_POWER, power);

    // 13. make sure delegation reward is calculated and paid correctly

    let return_rate = ABCI_MOCKER.read().get_current_rate();

    let rewards =
        calculate_delegation_rewards((32 + 64 + 85) * FRA, return_rate).c(d!())? * 10;

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    assert!(
        10000 * FRA - 5 * TX_FEE_MIN
            < ABCI_MOCKER.read().get_owned_balance(x_kp.get_pk_ref())
    );

    assert!(
        10000 * FRA + rewards - 5 * TX_FEE_MIN
            >= ABCI_MOCKER.read().get_owned_balance(x_kp.get_pk_ref())
    );

    // 14. use `x` to propose a new delegation

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 91 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 15. make sure it can do claim at any time

    trigger_next_block!();

    for _ in 0..10 {
        let old_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
        let tx_hash = claim(&x_kp, 1).c(d!())?;
        wait_one_block();
        assert!(is_successful(&tx_hash));

        // waiting to be paid
        trigger_next_block!();

        let new_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
        assert_eq!(old_balance - TX_FEE_MIN + 1, new_balance);
    }

    // 16. make sure it will fail if the claim amount is bigger than total rewards

    let tx_hash = claim(&x_kp, 10 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_failed(&tx_hash));

    // 17. undelegate and re-delegate at once

    // undelegate

    let tx_hash = undelegate(&x_kp).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    {
        let hdr = ABCI_MOCKER.read();
        let hdr = hdr.0.la.read();
        let hdr = hdr.get_committed_state().read();

        let staking = hdr.get_staking();
        let d = staking.delegation_get(&x_kp.get_pk()).c(d!())?;

        assert_eq!(DelegationState::Bond, d.state);
        assert!(d.end_height > staking.cur_height());
        assert!(d.end_height - staking.cur_height() <= UNBOND_BLOCK_CNT);
    }

    let old_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());

    // re-delegate at once

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 41 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // 18. make sure the delegation state turn back to `Bond`

    {
        let hdr = ABCI_MOCKER.read();
        let hdr = hdr.0.la.read();
        let hdr = hdr.get_committed_state().read();

        let staking = hdr.get_staking();
        let d = staking.delegation_get(&x_kp.get_pk()).c(d!())?;

        assert_eq!(DelegationState::Bond, d.state);
        assert_eq!(BLOCK_HEIGHT_MAX, d.end_height);
    }

    // 19. make sure no rewards will be paid

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
    assert_eq!(old_balance - 41 * FRA - TX_FEE_MIN, new_balance);

    // 20. unstake validator and check if
    // the principals of co-responding delegators will be paid back

    let tx_hash = undelegate(&kps[0]).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    trigger_next_block!(4 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
    assert!((old_balance - 2 * TX_FEE_MIN) < new_balance);

    // 21. undelegate partially

    let tx_hash = delegate(&x_kp, td_addr_to_string(&v_set[1].td_addr), FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    trigger_next_block!();

    let old_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());

    let pu = PartialUnDelegation::new(1, x_kp.get_pk(), v_set[1].td_addr.clone());
    let tx_hash = undelegate_x(&x_kp, Some(pu)).c(d!())?;
    wait_one_block();
    // reason: x is in delegation
    assert!(is_failed(&tx_hash));
    assert_eq!(
        ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk()),
        old_balance
    );

    let rand_pk = gen_keypair().get_pk();
    let pu = PartialUnDelegation::new(100, rand_pk, v_set[1].td_addr.clone());
    let tx_hash = undelegate_x(&x_kp, Some(pu)).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    assert_eq!(
        ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk()),
        100 + old_balance - TX_FEE_MIN
    );

    // undelegate and wait to be paid
    let tx_hash = undelegate(&x_kp).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    trigger_next_block!(4 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
    assert!((100 + old_balance - 2 * TX_FEE_MIN) < new_balance);

    // 22. transfer FRAs to multi addrs

    let (a_kp, a_am) = (gen_keypair(), 1 + FRA_TOTAL_AMOUNT * 5 / 100); // 5%, total 5%
    let (b_kp, b_am) = (gen_keypair(), 2 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 15%
    let (c_kp, c_am) = (gen_keypair(), 3 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 25%
    let (d_kp, d_am) = (gen_keypair(), 4 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 35%
    let (e_kp, e_am) = (gen_keypair(), 5 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 45%
    let (f_kp, f_am) = (gen_keypair(), 6 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 55%
    let (g_kp, g_am) = (gen_keypair(), 7 + FRA_TOTAL_AMOUNT * 10 / 100); // 10%, total 65%
    let (h_kp, h_am) = (gen_keypair(), 8 + FRA_TOTAL_AMOUNT * 3 / 100); // 3%, total 68%
    let (i_kp, i_am) = (gen_keypair(), 9 + FRA_TOTAL_AMOUNT * 12 / 100); // 12%, total 80%

    let alloc_table_x = [
        (&a_kp, a_am),
        (&b_kp, b_am),
        (&c_kp, c_am),
        (&d_kp, d_am),
        (&e_kp, e_am),
        (&f_kp, f_am),
        (&g_kp, g_am),
        (&h_kp, h_am),
        (&i_kp, i_am),
    ]
    .iter()
    .map(|(kp, am)| (*kp, *am))
    .collect::<Vec<(&XfrKeyPair, u64)>>();

    let alloc_table = alloc_table_x
        .iter()
        .map(|(kp, am)| (kp.get_pk(), *am))
        .collect::<BTreeMap<_, _>>();

    let tx_hash = transfer_batch(
        &ROOT_KEYPAIR,
        alloc_table.iter().map(|(k, v)| (k, *v)).collect(),
    )
    .c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    trigger_next_block!();

    // 23. make sure the result is correct

    assert!(alloc_table
        .iter()
        .all(|(pk, am)| *am == ABCI_MOCKER.read().get_owned_balance(pk)));

    // 24. use these addrs to delegate to different validators

    for (v, (kp, _)) in v_set.iter().skip(1).zip(alloc_table_x.iter()) {
        let tx_hash =
            delegate(kp, td_pubkey_to_td_addr(&v.td_pubkey), 32 * FRA).c(d!())?;
        wait_one_block();

        assert!(is_successful(&tx_hash));
    }

    // 25. make sure the power of each validator is increased correctly

    let n = alt!(
        v_set.len() - 1 > alloc_table.len(),
        alloc_table.len(),
        v_set.len() - 1
    );

    for v in v_set.iter().skip(1).take(n) {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v.id).c(d!()));

        assert_eq!((32 + 100) * FRA + INITIAL_POWER, power);
    }

    // 26. wait for the end of unbond state

    for (kp, _) in alloc_table_x.iter().take(n) {
        let tx_hash = undelegate(kp).c(d!())?;
        wait_one_block();

        assert!(is_successful(&tx_hash));
    }

    trigger_next_block!(8);

    // 27. make sure the power of each validator is decreased correctly

    for v in v_set.iter().skip(1).take(n) {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v.id).c(d!()));

        assert_eq!(100 * FRA + INITIAL_POWER, power);
    }

    // 28. re-delegate those multi addrs one by one
    // make sure delegation-rewards-rate is correct in different global delegation levels
    // ...........................................
    // .... will be tested in unit-test cases ....
    // ...........................................

    // 29. make sure the vote power of any validator can not exceed 20% of total power

    let tx_hash = delegate(
        &ROOT_KEYPAIR,
        td_pubkey_to_td_addr(&v_set[1].td_pubkey),
        32_0000 * FRA,
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // 30. make sure that user can NOT sent `MintFra` transactions
    let mint_ops = Operation::MintFra(MintFraOps::new(
        0u64,
        vec![
            MintEntry::new(MintKind::Claim, x_kp.get_pk(), None, 100, ASSET_TYPE_FRA),
            MintEntry::new(MintKind::UnStake, x_kp.get_pk(), None, 900, ASSET_TYPE_FRA),
        ],
    ));
    let tx = Transaction::from_operation(mint_ops, get_seq_id());
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_one_block();
    assert!(is_failed(&tx_hash));

    // 31. replay old transactions and make sure all of them is failed
    let old_txs = mem::take(&mut *SUCCESS_TXS.write())
        .into_iter()
        .chain(mem::take(&mut *FAILED_TXS.write()).into_iter())
        .map(|(tx_hash, tx)| send_tx(tx).c(d!()).map(|_| tx_hash))
        .collect::<Result<Vec<_>>>();

    // wait_n_block(10);
    trigger_next_block!(5);
    for tx_hash in old_txs.c(d!())?.iter() {
        assert!(is_failed(tx_hash));
    }

    Ok(())
}

// On-Chain Governance
//
// 0. issue FRA
// 1. update validators
// 2. .............
// 3. do self-delegations
// 4. do a regular delegation to the first validator
// 5. make sure the end-height of delegation is `BLOCK_HEIGHT_MAX`
// 6. governance one of them, and make sure its power is decreased to 1/3
// 7. make sure its delegation principal is punished
// 8. make sure the delegation principal of regular delegator is punished(1/10)
// 9. update validator, remove it from validator list
fn staking_scene_2() -> Result<()> {
    const VALIDATORS_NUM: u8 = 20;

    env_refresh(VALIDATORS_NUM);

    // 0. issue FRA

    let tx = fra_gen_initial_tx(&ROOT_KEYPAIR);
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 1. update validators

    let (mut v_set, mut kps) = gen_new_validators(VALIDATORS_NUM);
    assert_eq!(v_set.len(), kps.len());

    // update validators at height 2
    let initial_keypairs = INITIAL_KEYPAIR_LIST.iter().collect::<Vec<_>>();
    let tx_hash =
        update_validator(&ROOT_KEYPAIR, &initial_keypairs, 2, v_set.clone()).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // validators will be updated every 4 blocks
    for _ in 0..3 {
        trigger_next_block!();
    }

    let td_mocker = TD_MOCKER.read();
    let td_v_set = &td_mocker.validators;
    assert_eq!(v_set.len(), td_v_set.len());
    v_set.iter().for_each(|v| {
        assert_eq!(
            &INITIAL_POWER,
            pnk!(td_v_set.get(&td_pubkey_to_td_addr(&v.td_pubkey)))
        );
    });

    drop(td_mocker);

    // 2. .............

    // 3. do self-delegations

    for (i, kp) in kps.iter().enumerate() {
        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 100 * FRA)
            .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    }

    // 4. do a regular delegation to the first validator

    let x_kp = gen_keypair();

    let tx_hash = transfer(&ROOT_KEYPAIR, &x_kp.get_pk(), 100 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 32 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 5. make sure the end-height of self-delegation is changed to `BLOCK_HEIGHT_MAX` automatically

    for v in v_set.iter() {
        let end_height =
            pnk!(ABCI_MOCKER.read().get_delegation_by_addr(&v.id)).end_height;

        assert_eq!(BLOCK_HEIGHT_MAX, end_height);
    }

    // 6. governance the first one, and make sure its power is decreased to 1/3

    let old_power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

    let tx_hash = governance(
        &ROOT_KEYPAIR,
        &kps.iter().collect::<Vec<_>>(),
        v_set[0].id,
        ByzantineKind::DuplicateVote,
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    let new_power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

    assert!(old_power > new_power);

    // 7. make sure its delegation principle is punished

    let principal = ABCI_MOCKER
        .read()
        .get_owned_delegation_principal(&v_set[0].id);

    assert_eq!(100 * FRA * 95 / 100, principal as u64);

    // 8. make sure the delegation rewards of regular delegator is punished(1/10)

    let user_principal = ABCI_MOCKER
        .read()
        .get_owned_delegation_principal(&x_kp.get_pk());

    assert_eq!(32 * FRA * 95 / 100, user_principal as u64);

    // 9. update validator, remove it from validator list

    let v_set_new = v_set.split_off(1);
    let kps_new = kps.split_off(1);
    let tx_hash = update_validator(
        &ROOT_KEYPAIR,
        &kps_new.iter().collect::<Vec<_>>(),
        6,
        v_set_new,
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    Ok(())
}

// validator test staking process
// *. init env
// *. staking fra
// *. query power
// *. unstaking partially (wait so much)
// *. unstaking
// *. query reward
// *. query power again(query none)
fn staking_scene_3() -> Result<()> {
    // *. init env
    const VALIDATORS_NUM: u8 = 11;
    env_refresh(VALIDATORS_NUM);

    // mint fra to root
    let tx = fra_gen_initial_tx(&ROOT_KEYPAIR);
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // init validator signer
    let (mut v_set, mut kps) = gen_new_validators(VALIDATORS_NUM);
    let mut initial_keypairs = INITIAL_KEYPAIR_LIST.iter().collect::<Vec<_>>();

    let tx_hash =
        update_validator(&ROOT_KEYPAIR, &initial_keypairs, 2, v_set.clone()).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // validators will be updated every 4 blocks
    trigger_next_block!(4);

    let td_mocker = TD_MOCKER.read();
    let td_v_set = &td_mocker.validators;
    assert_eq!(v_set.len(), td_v_set.len());
    v_set.iter().for_each(|v| {
        assert_eq!(
            &INITIAL_POWER,
            pnk!(td_v_set.get(&td_pubkey_to_td_addr(&v.td_pubkey)))
        );
    });
    drop(td_mocker);

    // *. self delegate staking fra
    for (i, kp) in kps.iter().enumerate() {
        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 60_0000 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 60_0000 * FRA).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash =
            delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 1_0000 * FRA)
                .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    }

    // let validator amount - XXX > STAKING_VALIDATOR_MIN_POWER(88_8888*FRA)
    for _ in 0..34 {
        for (i, kp) in kps.iter().enumerate() {
            let tx_hash =
                delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 3_0000 * FRA)
                    .c(d!())?;
            wait_n_block(5);
            assert!(is_successful(&tx_hash));
        }
    }

    // query balance
    let old_balance = ABCI_MOCKER.read().get_owned_balance(&kps[0].get_pk());
    assert_eq!(
        old_balance,
        (120_0000 * FRA - (1_0000 + 34 * 3_0000) * FRA - 35 * TX_FEE_MIN)
    );
    let mut transaction_num = 0;

    // *. query power
    let p = pnk!(ABCI_MOCKER.read().get_validator_power(&kps[0].pub_key));
    assert_eq!(p, INITIAL_POWER + (1_0000 + 34 * 3_0000) * FRA);

    // *. unstaking partially
    let pu = PartialUnDelegation::new(
        100 * FRA,
        gen_keypair().pub_key,
        v_set[0].td_addr.clone(),
    );
    let tx_hash = undelegate_x(&kps[0], Some(pu)).c(d!())?;
    wait_n_block(5);
    transaction_num += 1;
    assert!(is_successful(&tx_hash));
    trigger_next_block!(6);

    // *. unstaking
    let tx_hash = undelegate(&kps[0]).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    // wait
    trigger_next_block!(UNBOND_BLOCK_CNT);
    // *. query reward
    // must before 6 block query reward,if not get reward 0
    let reward = ABCI_MOCKER.read().get_owned_reward(&kps[0].pub_key);

    trigger_next_block!();

    // query balance and judgment
    let new_balance = ABCI_MOCKER.read().get_owned_balance(&kps[0].get_pk());
    assert_eq!(
        new_balance,
        old_balance + reward - transaction_num * TX_FEE_MIN
            + (1_0000 * FRA)
            + (3_0000 * 34) * FRA
    );

    Ok(())
}

// general user test delegate process
// *. init env
// *. delegate fra to validator0 and validator1
// *. query validator0 and validator1 power
// *. view rate
// *. query DelegationInfo
// *. undelegate validator0 and validator1
// *. wait 6 block
// *. delegate fra to validator0
// *. query DelegationInfo again
// *. query reward
// *. get claim
fn staking_scene_4() -> Result<()> {
    // *. init env
    const VALIDATORS_NUM: u8 = 11;
    env_refresh(VALIDATORS_NUM);

    // mint fra to root
    let tx = fra_gen_initial_tx(&ROOT_KEYPAIR);
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // init validator signer
    let (mut v_set, mut kps) = gen_new_validators(VALIDATORS_NUM);
    let mut initial_keypairs = INITIAL_KEYPAIR_LIST.iter().collect::<Vec<_>>();

    let tx_hash =
        update_validator(&ROOT_KEYPAIR, &initial_keypairs, 2, v_set.clone()).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    // validators will be updated every 4 blocks
    for _ in 0..3 {
        trigger_next_block!();
    }

    let td_mocker = TD_MOCKER.read();
    let td_v_set = &td_mocker.validators;
    assert_eq!(v_set.len(), td_v_set.len());
    v_set.iter().for_each(|v| {
        assert_eq!(
            &INITIAL_POWER,
            pnk!(td_v_set.get(&td_pubkey_to_td_addr(&v.td_pubkey)))
        );
    });
    drop(td_mocker);

    for (i, kp) in kps.iter().enumerate() {
        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_one_block();
        assert!(is_successful(&tx_hash));

        let tx_hash = transfer(&ROOT_KEYPAIR, &v_set[i].id, 100 * FRA).c(d!())?;
        wait_one_block();
        assert!(is_successful(&tx_hash));

        let tx_hash = delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 100 * FRA)
            .c(d!())?;
        wait_one_block();
        assert!(is_successful(&tx_hash));
    }

    // create general user
    let x_kp = gen_keypair();
    // root transfer 10000 * FRA to general user
    let tx_hash = transfer(&ROOT_KEYPAIR, x_kp.get_pk_ref(), 1_0000 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));

    let old_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
    assert_eq!(old_balance, 1_0000 * FRA);
    let mut transaction_num = 0;

    // *. delegate fra to validator0 and validator1
    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 100 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[1].td_pubkey), 200 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    // *. query validator0 and validator1 power
    let power_map = pnk!(ABCI_MOCKER.read().get_delegation_power(&x_kp.pub_key));

    let judgment_power = |pk_key_vec: Vec<(&XfrPublicKey, u64)>| {
        for (pk_key, vp) in pk_key_vec {
            power_map.get(pk_key).map(|power| assert_eq!(*power, vp));
        }
    };

    judgment_power(vec![
        (&v_set[0].id, INITIAL_POWER + (100 + 100) * FRA),
        (&v_set[1].id, INITIAL_POWER + (100 + 200) * FRA),
    ]);

    // *. view rate
    let rate = ABCI_MOCKER.read().get_current_rate_str();

    // * query DelegationInfo
    let op = ABCI_MOCKER
        .read()
        .get_validators_by_delegation_addr(&x_kp.get_pk());

    // *. undelegate validator0 and validator1
    let tx_hash = undelegate(&x_kp).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    // *. wait 6 block
    let mut before_undelegation_reward = 0;
    {
        // if redelegation,must wait 6 block commit,
        trigger_next_block!(UNBOND_BLOCK_CNT);

        before_undelegation_reward = ABCI_MOCKER.read().get_owned_reward(&x_kp.pub_key);

        // last block no reward,just confirm undelegation
        trigger_next_block!();
    }

    // *. delegate fra to validator0
    let tx_hash =
        delegate(&x_kp, td_pubkey_to_td_addr(&v_set[0].td_pubkey), 100 * FRA).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    //* query DelegationInfo again
    let op = ABCI_MOCKER
        .read()
        .get_validators_by_delegation_addr(&x_kp.get_pk());

    // *. query reward
    let reward = ABCI_MOCKER.read().get_owned_reward(&x_kp.pub_key);

    // *. get claim
    let tx_hash = claim(&x_kp, reward).c(d!())?;
    wait_one_block();
    assert!(is_successful(&tx_hash));
    transaction_num += 1;

    // waiting to be paid
    trigger_next_block!();

    // query balance and judgment
    let new_balance = ABCI_MOCKER.read().get_owned_balance(&x_kp.get_pk());
    assert_eq!(
        new_balance,
        old_balance + before_undelegation_reward + reward
            - 100 * FRA
            - (transaction_num * TX_FEE_MIN)
    );

    Ok(())
}

// same as scene_1
fn staking_scene_5() -> Result<()> {
    const VALIDATORS_NUM: u8 = 11;

    // addresses are guaranteed to remain unchanged when test reloading
    let path_str = fresh_tmp_dir().to_str().unwrap().to_string();

    env_refresh_with_path(VALIDATORS_NUM, path_str.clone());

    // 0. issue FRA
    // seq_id used to determine whether to overload
    let seq_id = ABCI_MOCKER
        .read()
        .0
        .la
        .read()
        .get_committed_state()
        .read()
        .get_block_commit_count();

    let mut root_keypair = gen_keypair();

    // store v_set and kps
    let mut m = map! {};

    if seq_id == 0 {
        let root_keypair_str = pnk!(serde_json::to_string(&root_keypair));
        m.insert(0, root_keypair_str);

        let tx = fra_gen_initial_tx(&root_keypair);
        let tx_hash = gen_tx_hash(&tx);
        send_tx(tx).c(d!())?;
        wait_n_block(10);
        assert!(is_successful(&tx_hash));
    } else {
        m.get(&0).c(d!()).map(|v| {
            let kp: XfrKeyPair = serde_json::from_str(v.as_str()).unwrap();
            root_keypair = kp;
        });
    }

    let tx_hash =
        transfer(&root_keypair, ROOT_KEYPAIR.get_pk_ref(), 10000 * 100).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 1. update validators

    let (mut v_set, mut kps) = gen_new_validators(VALIDATORS_NUM);
    if seq_id == 0 {
        let v_set_str = pnk!(serde_json::to_string(&v_set));
        m.insert(1, v_set_str);

        let kps_str = pnk!(serde_json::to_string(&kps));
        m.insert(2, kps_str);
    } else {
        m.get(&1).c(d!()).map(|v| {
            let va: Vec<StakingValidator> = serde_json::from_str(v.as_str()).unwrap();
            v_set = va.clone();
        });

        m.get(&2).c(d!()).map(|v| {
            let va: Vec<XfrKeyPair> = serde_json::from_str(v.as_str()).unwrap();
            kps = va.clone();
        });
    }
    assert_eq!(v_set.len(), kps.len());

    // update validators at height 2

    let mut initial_keypairs = INITIAL_KEYPAIR_LIST.iter().collect::<Vec<_>>();

    if seq_id == 0 {
        let tx_hash =
            update_validator(&root_keypair, &initial_keypairs, 2, v_set.clone())
                .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        trigger_next_block!(3);
        //
        let td_mocker = TD_MOCKER.read();
        let td_v_set = &td_mocker.validators;
        assert_eq!(v_set.len(), td_v_set.len());
        v_set.iter().for_each(|v| {
            assert_eq!(
                &INITIAL_POWER,
                pnk!(td_v_set.get(&td_pubkey_to_td_addr(&v.td_pubkey)))
            );
        });

        drop(td_mocker);
    }

    // validators will be updated every 4 blocks

    // 2. .................

    // 3. transfer some FRAs to a new addr `x`

    let x_kp = gen_keypair();

    let tx_hash = transfer(&root_keypair, x_kp.get_pk_ref(), 10000 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 4. use `x` to propose a delegation, and make sure that
    // it will fail because that all validators have not done self-delegation

    let tx_hash = delegate(
        &root_keypair,
        td_pubkey_to_td_addr(&v_set[0].td_pubkey),
        32 * 100,
    )
    .c(d!())?;
    wait_n_block(10);

    assert!(is_failed(&tx_hash));

    // undelegation will fail
    let tx_hash = undelegate(&root_keypair).c(d!())?;
    wait_n_block(10);
    assert!(is_failed(&tx_hash));

    trigger_next_block!(6);

    // 5. make validators to finish their self-delegations

    if seq_id == 0 {
        for (i, kp) in kps.iter().enumerate() {
            let tx_hash = transfer(&root_keypair, &v_set[i].id, 50 * 1000).c(d!())?;
            wait_n_block(10);
            assert!(is_successful(&tx_hash));

            let tx_hash =
                delegate(kp, td_pubkey_to_td_addr(&v_set[i].td_pubkey), 30 * 10)
                    .c(d!())?;
            wait_n_block(10);
            assert!(is_successful(&tx_hash));
        }
    }

    // 6. use `x` to propose a delegation
    // 7. make sure `x` can continue to propose new delegations

    if seq_id == 0 {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[0].td_pubkey),
            32 * 10,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));

        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[0].td_pubkey),
            64 * 10,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    } else {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[4].td_pubkey),
            32 * 10,
        )
        .c(d!())?;
        wait_n_block(10);
        assert!(is_successful(&tx_hash));

        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[4].td_pubkey),
            64 * 10,
        )
        .c(d!())?;
        wait_n_block(10);
    }

    // 8. delegate to different validators

    let tx_hash = delegate(
        &root_keypair,
        td_pubkey_to_td_addr(&v_set[3].td_pubkey),
        84 * 10,
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    // 9. make sure `x` can do transfer

    let tx_hash = transfer(&root_keypair, &BLACK_HOLE_PUBKEY, 1).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    trigger_next_block!();
    wait_n_block(5);

    // 10. make sure the power of co-responding validator is increased
    if seq_id == 0 {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

        // assert_eq!((32 + 64 + 100) * FRA + INITIAL_POWER, power);
        assert_eq!(96 * 10 + INITIAL_POWER + 300, power);
    } else {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[4].id).c(d!()));

        // assert_eq!((32 + 64 + 100) * FRA + INITIAL_POWER, power);
        assert_eq!(96 * 10 + INITIAL_POWER + 300, power);
    }

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[3].id).c(d!()));

    // if seq_id == 0 {
    //     assert_eq!(84 * 10 + INITIAL_POWER + 300, power);
    // } else {
    //     assert_eq!(84 * 10 + INITIAL_POWER + 300, power);
    // }

    assert_eq!(84 * 10 + INITIAL_POWER + 300, power);

    // 11. undelegate

    let tx_hash = undelegate(&root_keypair).c(d!())?;
    wait_n_block(10);
    assert!(is_successful(&tx_hash));

    // 12. make sure the power of co-responding validator is decreased
    let mut old_rewards = 0;
    if seq_id == 0 {
        old_rewards = ABCI_MOCKER.read().get_owned_reward(&root_keypair.pub_key);
        trigger_next_block!(1 + UNBOND_BLOCK_CNT);

        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[0].id).c(d!()));

        assert_eq!(300 + INITIAL_POWER, power);
    } else {
        old_rewards = ABCI_MOCKER.read().get_owned_reward(&root_keypair.pub_key);
        trigger_next_block!(1 + UNBOND_BLOCK_CNT);

        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[4].id).c(d!()));

        assert_eq!(300 + INITIAL_POWER, power);
    }

    let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v_set[3].id).c(d!()));

    // if seq_id == 0 {
    //     assert_eq!(300 + INITIAL_POWER, power);
    // } else {
    //     assert_eq!(300 + INITIAL_POWER, power);
    // }

    assert_eq!(300 + INITIAL_POWER, power);

    // 13. make sure delegation reward is calculated and paid correctly

    let return_rate = ABCI_MOCKER.read().get_current_rate();

    let rewards =
        calculate_delegation_rewards((32 + 64 + 84) * 10, return_rate).c(d!())? * 10;

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    //both 0 ,because amount so small
    assert_eq!(old_rewards, rewards);

    // 14. use `x` to propose a new delegation
    if seq_id == 0 {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[0].td_pubkey),
            91 * FRA,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    } else {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[4].td_pubkey),
            91 * FRA,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    }

    // 15. make sure it can do claim at any time

    trigger_next_block!(6);

    if seq_id == 0 {
        for _ in 0..10 {
            let old_balance =
                ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());
            let tx_hash = claim(&root_keypair, 1).c(d!())?;
            wait_n_block(10);
            assert!(is_successful(&tx_hash));

            // waiting to be paid
            trigger_next_block!();

            let new_balance =
                ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());
            assert_eq!(old_balance - TX_FEE_MIN + 1, new_balance);
        }
    }

    // 16. make sure it will fail if the claim amount is bigger than total rewards

    let tx_hash = claim(&root_keypair, 10 * FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // 17. undelegate and re-delegate at once

    // undelegate

    let tx_hash = undelegate(&root_keypair).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    {
        let hdr = ABCI_MOCKER.read();
        let hdr = hdr.0.la.read();
        let hdr = hdr.get_committed_state().read();

        let staking = hdr.get_staking();
        let d = staking
            .deref()
            .delegation_get(&root_keypair.get_pk())
            .c(d!())?;

        assert_eq!(DelegationState::Bond, d.state);
        assert!(d.end_height > staking.cur_height());
        assert!(d.end_height - staking.cur_height() <= UNBOND_BLOCK_CNT);
    }

    let old_balance = ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());

    // re-delegate at once

    if seq_id == 0 {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[0].td_pubkey),
            41 * FRA,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    } else {
        let tx_hash = delegate(
            &root_keypair,
            td_pubkey_to_td_addr(&v_set[4].td_pubkey),
            41 * FRA,
        )
        .c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    }

    // 18. make sure the delegation state turn back to `Bond`

    {
        let hdr = ABCI_MOCKER.read();
        let hdr = hdr.0.la.read();
        let hdr = hdr.get_committed_state().read();

        let staking = hdr.get_staking();
        let d = staking
            .deref()
            .delegation_get(&root_keypair.get_pk())
            .c(d!())?;

        assert_eq!(DelegationState::Bond, d.state);
        assert_eq!(BLOCK_HEIGHT_MAX, d.end_height);
    }

    // 19. make sure no rewards will be paid

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());
    assert_eq!(old_balance - 41 * FRA - TX_FEE_MIN, new_balance);

    // 20. unstake validator and check if
    // the principals of co-responding delegators will be paid back

    if seq_id == 0 {
        let tx_hash = undelegate(&kps[0]).c(d!())?;
        wait_n_block(5);
        assert!(is_successful(&tx_hash));
    }

    trigger_next_block!(4 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());
    if seq_id == 0 {
        assert!((old_balance - 2 * TX_FEE_MIN) < new_balance);
    }

    // 21. undelegate partially

    let tx_hash =
        delegate(&root_keypair, td_addr_to_string(&v_set[1].td_addr), FRA).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    trigger_next_block!();

    let old_balance = ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());

    let pu =
        PartialUnDelegation::new(1, root_keypair.get_pk(), v_set[1].td_addr.clone());
    let tx_hash = undelegate_x(&root_keypair, Some(pu)).c(d!())?;
    wait_n_block(5);
    // reason: x is in delegation
    assert!(is_failed(&tx_hash));
    assert_eq!(
        ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk()),
        old_balance
    );

    let rand_pk = gen_keypair().get_pk();
    let pu = PartialUnDelegation::new(100, rand_pk, v_set[1].td_addr.clone());
    let tx_hash = undelegate_x(&root_keypair, Some(pu)).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    trigger_next_block!(1 + UNBOND_BLOCK_CNT);

    assert_eq!(
        ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk()),
        100 + old_balance - TX_FEE_MIN
    );

    // undelegate and wait to be paid
    let tx_hash = undelegate(&root_keypair).c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    trigger_next_block!(4 + UNBOND_BLOCK_CNT);

    let new_balance = ABCI_MOCKER.read().get_owned_balance(&root_keypair.get_pk());
    assert!((100 + old_balance - 2 * TX_FEE_MIN) < new_balance);

    // 22. transfer FRAs to multi addrs

    let (a_kp, a_am) = (gen_keypair(), 1 + FRA_TOTAL_AMOUNT * 5 / 1000000); // 5%, total 5%
    let (b_kp, b_am) = (gen_keypair(), 2 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 15%
    let (c_kp, c_am) = (gen_keypair(), 3 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 25%
    let (d_kp, d_am) = (gen_keypair(), 4 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 35%
    let (e_kp, e_am) = (gen_keypair(), 5 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 45%
    let (f_kp, f_am) = (gen_keypair(), 6 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 55%
    let (g_kp, g_am) = (gen_keypair(), 7 + FRA_TOTAL_AMOUNT * 10 / 1000000); // 10%, total 65%
    let (h_kp, h_am) = (gen_keypair(), 8 + FRA_TOTAL_AMOUNT * 3 / 1000000); // 3%, total 68%
    let (i_kp, i_am) = (gen_keypair(), 9 + FRA_TOTAL_AMOUNT * 12 / 1000000); // 12%, total 80%

    let alloc_table_x = [
        (&a_kp, a_am),
        (&b_kp, b_am),
        (&c_kp, c_am),
        (&d_kp, d_am),
        (&e_kp, e_am),
        (&f_kp, f_am),
        (&g_kp, g_am),
        (&h_kp, h_am),
        (&i_kp, i_am),
    ]
    .iter()
    .map(|(kp, am)| (*kp, *am))
    .collect::<Vec<(&XfrKeyPair, u64)>>();

    let alloc_table = alloc_table_x
        .iter()
        .map(|(kp, am)| (kp.get_pk(), *am))
        .collect::<BTreeMap<_, _>>();

    let tx_hash = transfer_batch(
        &root_keypair,
        alloc_table.iter().map(|(k, v)| (k, *v)).collect(),
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_successful(&tx_hash));

    trigger_next_block!();

    // 23. make sure the result is correct

    assert!(alloc_table
        .iter()
        .all(|(pk, am)| *am == ABCI_MOCKER.read().get_owned_balance(pk)));

    // 24. use these addrs to delegate to different validators

    for (v, (kp, _)) in v_set.iter().skip(1).zip(alloc_table_x.iter()) {
        let tx_hash =
            delegate(kp, td_pubkey_to_td_addr(&v.td_pubkey), 44 * 100).c(d!())?;
        wait_n_block(5);

        assert!(is_successful(&tx_hash));
    }

    // 25. make sure the power of each validator is increased correctly

    let n = alt!(
        v_set.len() - 1 > alloc_table.len(),
        alloc_table.len(),
        v_set.len() - 1
    );

    for v in v_set.iter().skip(1).take(n) {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v.id).c(d!()));

        assert_eq!((44 + 3) * 100 + INITIAL_POWER, power);
    }

    // 26. wait for the end of unbond state

    for (kp, _) in alloc_table_x.iter().take(n) {
        let tx_hash = undelegate(kp).c(d!())?;
        wait_n_block(5);

        assert!(is_successful(&tx_hash));
    }

    trigger_next_block!(8);

    // 27. make sure the power of each validator is decreased correctly

    for v in v_set.iter().skip(1).take(n) {
        let power = pnk!(ABCI_MOCKER.read().get_validator_power(&v.id).c(d!()));

        assert_eq!(300 + INITIAL_POWER, power);
    }

    // 28. re-delegate those multi addrs one by one
    // make sure delegation-rewards-rate is correct in different global delegation levels
    // ...........................................
    // .... will be tested in unit-test cases ....
    // ...........................................

    // 29. make sure the vote power of any validator can not exceed 20% of total power

    let tx_hash = delegate(
        &root_keypair,
        td_pubkey_to_td_addr(&v_set[1].td_pubkey),
        32_0000 * FRA,
    )
    .c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // 30. make sure that user can NOT sent `MintFra` transactions
    let mint_ops = Operation::MintFra(MintFraOps::new(
        0u64,
        vec![
            MintEntry::new(MintKind::Claim, x_kp.get_pk(), None, 100, ASSET_TYPE_FRA),
            MintEntry::new(MintKind::UnStake, x_kp.get_pk(), None, 900, ASSET_TYPE_FRA),
        ],
    ));
    let tx = Transaction::from_operation(mint_ops, get_seq_id());
    let tx_hash = gen_tx_hash(&tx);
    send_tx(tx).c(d!())?;
    wait_n_block(5);
    assert!(is_failed(&tx_hash));

    // 31. replay old transactions and make sure all of them is failed
    let old_txs = mem::take(&mut *SUCCESS_TXS.write())
        .into_iter()
        .chain(mem::take(&mut *FAILED_TXS.write()).into_iter())
        .map(|(tx_hash, tx)| send_tx(tx).c(d!()).map(|_| tx_hash))
        .collect::<Result<Vec<_>>>();

    wait_n_block(5);
    trigger_next_block!(5);

    for tx_hash in old_txs.c(d!())?.iter() {
        assert!(is_failed(tx_hash));
    }

    Ok(())
}

#[test]
fn staking_integration_abci_mock() {
    pnk!(staking_scene_1());
    pnk!(staking_scene_2());
    pnk!(staking_scene_3());
    pnk!(staking_scene_4());
    pnk!(staking_scene_5());
}
