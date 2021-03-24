#![allow(unused)]
#![deny(warnings)]

use quickcheck::{Arbitrary, Gen, QuickCheck, StdGen};
use std::iter::{once, repeat};
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use cryptohash::sha256::Digest as BitDigest;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::store::*;
use ledger::{inp_fail, zei_fail};
use ledger_api_service::RestfulLedgerAccess;
use network::LedgerStandalone;
use rand_chacha::ChaChaRng;
use rand_core::{RngCore, SeedableRng};
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
#[cfg(test)]
use std::ffi::OsString;
use std::mem;
use std::path::PathBuf;
#[cfg(test)]
use std::thread;
use std::time;
use structopt::StructOpt;
use submission_api::RestfulLedgerUpdate;
use submission_server::{TxnHandle, TxnStatus};
use subprocess::Popen;
#[cfg(test)]
use subprocess::PopenConfig;
use utils::{HasInvariants, HashOf};
use zei::api::anon_creds::ACCommitment;
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{
    build_blind_asset_record, open_blind_asset_record, AssetRecordType,
};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSignature};
use zei::xfr::structs::{
    AssetRecord, AssetRecordTemplate, OpenAssetRecord, OwnerMemo, TracingPolicy,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UserName(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UnitName(pub String);

impl Arbitrary for UserName {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        UserName(String::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.0.shrink().map(UserName))
    }
}

impl Arbitrary for UnitName {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        UnitName(String::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.0.shrink().map(UnitName))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum AccountsCommand {
    NewUser(UserName),                         // name
    NewUnit(UnitName, UserName),               // name, issuer
    Mint(usize, UnitName),                     // count, unit
    Send(UserName, usize, UnitName, UserName), // source,count,unit,dest
    ToggleConfTypes(),
    ToggleConfAmts(),
}

fn rename_user(old: &UserName, new: &UserName, ac: AccountsCommand) -> AccountsCommand {
    use AccountsCommand::*;
    match ac {
        NewUnit(unit, name) => {
            NewUnit(unit, if name == *old { new.clone() } else { name })
        }
        Send(src, count, unit, dest) => Send(
            if src == *old { new.clone() } else { src },
            count,
            unit,
            if dest == *old { new.clone() } else { dest },
        ),
        _ => ac,
    }
}

fn rename_unit(old: &UnitName, new: &UnitName, ac: AccountsCommand) -> AccountsCommand {
    use AccountsCommand::*;
    match ac {
        Mint(count, unit) => Mint(count, if unit == *old { new.clone() } else { unit }),
        Send(src, count, unit, dest) => Send(
            src,
            count,
            if unit == *old { new.clone() } else { unit },
            dest,
        ),
        // Ignore NewUnit(..), since re-declaring a unit should be invalid.
        _ => ac,
    }
}

fn rename_by(
    old: &AccountsCommand,
    new: &AccountsCommand,
    ac: AccountsCommand,
) -> AccountsCommand {
    use AccountsCommand::*;
    match (old, new) {
        (NewUser(old_name), NewUser(new_name)) => rename_user(old_name, new_name, ac),
        (NewUnit(old_name, iss1), NewUnit(new_name, iss2)) => {
            rename_user(iss1, iss2, rename_unit(old_name, new_name, ac))
        }
        _ => ac,
    }
}

// Arbitrary and Gen are from QuickCheck (see
// https://docs.rs/quickcheck/0.9.0/quickcheck/) -- This lets us fuzz-test
// anything that takes `AccountsCommand`s (or things built out of them
// which also implement `Arbitrary`)
impl Arbitrary for AccountsCommand {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // Use a random number from `g` to select what kind of operation, then
        // use the `Arbitrary` instance for the parameters' types to generate
        // random parameters.
        match g.next_u32() % 10 {
            0 => AccountsCommand::NewUser(UserName::arbitrary(g)),
            1 => {
                AccountsCommand::NewUnit(UnitName::arbitrary(g), UserName::arbitrary(g))
            }
            2..=4 => AccountsCommand::Mint(usize::arbitrary(g), UnitName::arbitrary(g)),
            5..=9 => AccountsCommand::Send(
                UserName::arbitrary(g),
                usize::arbitrary(g),
                UnitName::arbitrary(g),
                UserName::arbitrary(g),
            ),
            // 10 => AccountsCommand::ToggleConfAmts(),
            // 11 => AccountsCommand::ToggleConfTypes(),
            _ => panic!("Out of range"),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        use AccountsCommand::*;
        // Generate shrunken-by-one-step commands. The strategy is to try one
        // parameter at a time (each `chain`ed iterator is for a single
        // parameter) to shrink while all other parameters are held constant
        // (via the `repeat` iterators).
        match self {
            NewUser(name) => Box::new(name.shrink().map(NewUser)),
            NewUnit(name, issuer) => {
                Box::new(
                    // shrink name
                    name.shrink()
                        .zip(repeat(issuer.clone()))
                        // shrink issuer
                        .chain(repeat(name.clone()).zip(issuer.shrink()))
                        .map(|(n, i)| NewUnit(n, i)),
                )
            }
            Mint(amt, unit) => {
                Box::new(
                    // shrink amt
                    amt.shrink()
                        .zip(repeat(unit.clone()))
                        // shrink unit
                        .chain(repeat(*amt).zip(unit.shrink()))
                        .map(|(a, u)| Mint(a, u)),
                )
            }
            Send(src, amt, unit, dst) => {
                Box::new(
                    // shrink src
                    src.shrink()
                        .zip(repeat(*amt))
                        .zip(repeat(unit.clone()).zip(repeat(dst.clone())))
                        .chain(
                            // shrink amt
                            repeat(src.clone())
                                .zip(amt.shrink())
                                .zip(repeat(unit.clone()).zip(repeat(dst.clone()))),
                        )
                        .chain(
                            // shrink unit
                            repeat(src.clone())
                                .zip(repeat(*amt))
                                .zip(unit.shrink().zip(repeat(dst.clone()))),
                        )
                        .chain(
                            // shrink dst
                            repeat(src.clone())
                                .zip(repeat(*amt))
                                .zip(repeat(unit.clone()).zip(dst.shrink())),
                        )
                        .map(|((src, amt), (unit, dst))| Send(src, amt, unit, dst)),
                )
            }
            ToggleConfAmts() => Box::new(std::iter::empty()),
            ToggleConfTypes() => Box::new(std::iter::empty()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
struct SimpleAccountsState {
    accounts: HashMap<UserName, ()>,
    units_to_users: HashMap<UnitName, UserName>,
    users_to_units: HashMap<UserName, HashSet<UnitName>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
struct AccountsState {
    accounts: HashMap<UserName, HashMap<UnitName, usize>>,
    units_to_users: HashMap<UnitName, UserName>,
    users_to_units: HashMap<UserName, HashSet<UnitName>>,
}

impl HasInvariants for SimpleAccountsState {
    fn fast_invariant_check(&self) -> Result<()> {
        if self.accounts.len() != self.users_to_units.len() {
            return Err(eg!());
        }

        Ok(())
    }

    fn deep_invariant_check(&self) -> Result<()> {
        self.fast_invariant_check().c(d!())?;

        for (name, _) in self.accounts.iter() {
            for unit in self.users_to_units.get(name).c(d!())?.iter() {
                if name != self.units_to_users.get(unit).c(d!())? {
                    return Err(eg!());
                }
            }

            for (unit, _) in self.units_to_users.iter() {
                let orig_user = self.units_to_users.get(unit).c(d!())?;
                if !self.users_to_units.get(orig_user).c(d!())?.contains(unit) {
                    return Err(eg!());
                }

                self.accounts.get(orig_user).c(d!())?;
            }
        }

        Ok(())
    }
}

impl HasInvariants for AccountsState {
    fn fast_invariant_check(&self) -> Result<()> {
        if self.accounts.len() != self.users_to_units.len() {
            return Err(eg!());
        }

        Ok(())
    }

    fn deep_invariant_check(&self) -> Result<()> {
        self.fast_invariant_check().c(d!())?;

        SimpleAccountsState {
            accounts: self
                .accounts
                .iter()
                .map(|(acc, _)| (acc.clone(), ()))
                .collect(),
            units_to_users: self.units_to_users.clone(),
            users_to_units: self.users_to_units.clone(),
        }
        .deep_invariant_check()
        .c(d!())?;

        for (_, balances) in self.accounts.iter() {
            for (unit, _) in self.units_to_users.iter() {
                balances.get(unit).c(d!())?;
            }
        }

        Ok(())
    }
}

trait InterpretAccounts<ErrT> {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()>;
}

impl InterpretAccounts<()> for SimpleAccountsState {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        match cmd {
            AccountsCommand::NewUser(name) => {
                self.accounts.get(name).c(d!())?;
                self.users_to_units.get(name).c(d!())?;

                self.accounts.insert(name.clone(), ());
                self.users_to_units.insert(name.clone(), HashSet::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                self.accounts.get(issuer).c(d!())?;
                let unit_set = self.users_to_units.get_mut(issuer).c(d!())?;
                self.units_to_users.get(name).c(d!())?;

                unit_set.insert(name.clone());
                self.units_to_users.insert(name.clone(), issuer.clone());
            }
            AccountsCommand::Mint(_, unit) => {
                self.accounts
                    .get(self.units_to_users.get(unit).c(d!())?)
                    .c(d!())?;
            }
            AccountsCommand::Send(src, _, unit, dst) => {
                self.accounts.get(src).c(d!())?;
                self.accounts.get(dst).c(d!())?;
                self.units_to_users.get(unit).c(d!())?;
            }
            AccountsCommand::ToggleConfAmts() => {}
            AccountsCommand::ToggleConfTypes() => {}
        }
        Ok(())
    }
}

impl InterpretAccounts<()> for AccountsState {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        match cmd {
            AccountsCommand::NewUser(name) => {
                self.accounts.get(name).c(d!())?;
                self.users_to_units.get(name).c(d!())?;

                self.accounts.insert(
                    name.clone(),
                    self.units_to_users
                        .iter()
                        .map(|(unit, _)| (unit.clone(), 0))
                        .collect(),
                );
                self.users_to_units.insert(name.clone(), HashSet::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                self.accounts.get(issuer).c(d!())?;
                let unit_set = self.users_to_units.get_mut(issuer).c(d!())?;
                self.units_to_users.get(name).c(d!())?;

                unit_set.insert(name.clone());
                self.units_to_users.insert(name.clone(), issuer.clone());

                for (_, acc) in self.accounts.iter_mut() {
                    acc.insert(name.clone(), 0);
                }
            }
            AccountsCommand::Mint(amt, unit) => {
                let acct = self
                    .accounts
                    .get_mut(self.units_to_users.get(unit).c(d!())?)
                    .c(d!())?;
                *acct.get_mut(unit).c(d!())? += amt;
            }
            AccountsCommand::Send(src, amt, unit, dst) => {
                {
                    let dst_acct = self.accounts.get(dst).c(d!())?;
                    dst_acct.get(unit).c(d!())?;
                }
                {
                    let src_acct = self.accounts.get_mut(src).c(d!())?;
                    let src_column = src_acct.get_mut(unit).c(d!())?;
                    if *src_column < *amt {
                        return Err(eg!());
                    }
                    *src_column -= amt;
                }
                {
                    let dst_acct = self.accounts.get_mut(dst).c(d!())?;
                    let dst_column = dst_acct.get_mut(unit).c(d!())?;

                    *dst_column += amt;
                }
            }
            AccountsCommand::ToggleConfAmts() => {}
            AccountsCommand::ToggleConfTypes() => {}
        }
        Ok(())
    }
}

struct LedgerAccounts {
    ledger: LedgerState,
    accounts: HashMap<UserName, XfrKeyPair>,
    balances: HashMap<UserName, HashMap<UnitName, u64>>,
    utxos: HashMap<UserName, VecDeque<TxoSID>>, // by account
    units: HashMap<UnitName, (UserName, AssetTypeCode)>, // user, data
    owner_memos: HashMap<TxoSID, OwnerMemo>,
    // These only affect new issuances
    confidential_amounts: bool,
    confidential_types: bool,
}

impl InterpretAccounts<PlatformError> for LedgerAccounts {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        let conf_amts = self.confidential_amounts;
        let conf_types = self.confidential_types;
        let iss_art = AssetRecordType::from_flags(conf_amts, false);
        let art = AssetRecordType::from_flags(conf_amts, conf_types);
        // dbg!(cmd);
        match cmd {
            AccountsCommand::NewUser(name) => {
                let keypair = XfrKeyPair::generate(self.ledger.get_prng());

                self.accounts.get(name).c(d!(inp_fail!()))?;

                // dbg!("New user", &name, &keypair);

                self.accounts.insert(name.clone(), keypair);
                self.utxos.insert(name.clone(), VecDeque::new());
                self.balances.insert(name.clone(), HashMap::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

                self.units.get(name).c(d!(inp_fail!()))?;

                let code = AssetTypeCode::gen_random();

                // dbg!("New unit", &name, &issuer, &code);

                let mut properties: Asset = Asset {
                    code,
                    ..Default::default()
                };
                properties.issuer.key = *pubkey;
                properties.memo.0 =
                    format!("asset '{:?}' issued by '{:?}'", name, issuer);

                let body = DefineAssetBody {
                    asset: Box::new(properties),
                };

                let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair })
                    .c(d!())?;
                let seq_id = self.ledger.get_block_commit_count();
                let txn = Transaction {
                    body: TransactionBody {
                        no_replay_token: NoReplayToken::new(
                            self.ledger.get_prng(),
                            seq_id,
                        ),
                        operations: vec![Operation::DefineAsset(op)],
                        credentials: vec![],
                        memos: vec![],
                        policy_options: None,
                    },
                    signatures: vec![],
                };

                let eff = TxnEffect::compute_effect(txn).c(d!())?;

                {
                    let mut block = self.ledger.start_block().c(d!())?;
                    if let Err(e) = self.ledger.apply_transaction(&mut block, eff) {
                        self.ledger.abort_block(block);
                        return Err(eg!(e));
                    }
                    self.ledger.finish_block(block).c(d!())?;
                }

                self.units.insert(name.clone(), (issuer.clone(), code));

                for (_, bal) in self.balances.iter_mut() {
                    bal.insert(name.clone(), 0);
                }
            }
            AccountsCommand::Mint(amt, unit) => {
                let amt = *amt as u64;
                let (issuer, code) = self.units.get(unit).c(d!(inp_fail!()))?;

                let new_seq_num = self.ledger.get_issuance_num(&code).c(d!())?;

                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
                let utxos = self.utxos.get_mut(issuer).c(d!())?;

                *self
                    .balances
                    .get_mut(issuer)
                    .c(d!())?
                    .get_mut(unit)
                    .unwrap() += amt;
                let seq_id = self.ledger.get_block_commit_count();
                let mut tx = Transaction::from_seq_id(seq_id);

                let ar = AssetRecordTemplate::with_no_asset_tracing(
                    amt, code.val, iss_art, *pubkey,
                );
                let params = PublicParams::default();
                let (ba, _, owner_memo) = build_blind_asset_record(
                    self.ledger.get_prng(),
                    &params.pc_gens,
                    &ar,
                    vec![],
                );

                let asset_issuance_body = IssueAssetBody::new(
                    &code,
                    new_seq_num,
                    &[(
                        TxOutput {
                            id: None,
                            record: ba,
                            lien: None,
                        },
                        owner_memo.clone(),
                    )],
                )
                .c(d!())?;

                let asset_issuance_operation = IssueAsset::new(
                    asset_issuance_body,
                    &IssuerKeyPair { keypair: &keypair },
                )
                .c(d!())?;

                let issue_op = Operation::IssueAsset(asset_issuance_operation);

                tx.body.operations.push(issue_op);
                let effect = TxnEffect::compute_effect(tx).c(d!())?;

                let mut block = self.ledger.start_block().c(d!())?;
                let temp_sid =
                    self.ledger.apply_transaction(&mut block, effect).c(d!())?;

                let (_, txos) = self
                    .ledger
                    .finish_block(block)
                    .c(d!())?
                    .remove(&temp_sid)
                    .unwrap();

                debug_assert!(txos.len() == 1);
                if let Some(memo) = owner_memo {
                    self.owner_memos.insert(txos[0], memo);
                }
                utxos.extend(txos.iter());
            }
            AccountsCommand::Send(src, amt, unit, dst) => {
                let amt = *amt as u64;
                let src_keypair = self.accounts.get(src).c(d!(inp_fail!()))?;
                let (src_pub, src_priv) =
                    (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
                let dst_keypair = self.accounts.get(dst).c(d!(inp_fail!()))?;
                let (dst_pub, dst_priv) =
                    (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
                let (_, unit_code) = self.units.get(unit).c(d!(inp_fail!()))?;

                if *self.balances.get(src).c(d!())?.get(unit).c(d!())? < amt {
                    return Err(eg!(inp_fail!()));
                }
                if amt == 0 {
                    return Ok(());
                }

                *self.balances.get_mut(src).c(d!())?.get_mut(unit).c(d!())? -= amt;
                *self.balances.get_mut(dst).c(d!())?.get_mut(unit).c(d!())? += amt;

                let mut src_records: Vec<OpenAssetRecord> = Vec::new();
                let mut total_sum = 0u64;
                let avail = self.utxos.get_mut(src).c(d!())?;
                let mut to_use: Vec<TxoSID> = Vec::new();
                let mut to_skip: Vec<TxoSID> = Vec::new();

                while total_sum < amt && !avail.is_empty() {
                    let sid = avail.pop_front().c(d!())?;
                    let blind_rec = &(self.ledger.get_utxo(sid).c(d!())?.utxo.0).record;
                    let memo = self.owner_memos.get(&sid).cloned();
                    let open_rec =
                        open_blind_asset_record(&blind_rec, &memo, &src_keypair)
                            .c(d!())?;
                    // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
                    if *open_rec.get_asset_type() != unit_code.val {
                        to_skip.push(sid);
                        continue;
                    }

                    debug_assert!(*open_rec.get_asset_type() == unit_code.val);
                    to_use.push(sid);
                    total_sum += *open_rec.get_amount();
                    src_records.push(open_rec);
                }
                // dbg!(&to_skip, &to_use);
                avail.extend(to_skip.into_iter());

                debug_assert!(total_sum >= amt);

                let mut src_outputs: Vec<AssetRecord> = Vec::new();
                let mut dst_outputs: Vec<AssetRecord> = Vec::new();
                let mut all_outputs: Vec<AssetRecord> = Vec::new();
                {
                    // Simple output to dst
                    let template = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.ledger.get_prng(),
                        &template,
                    )
                    .c(d!())?;
                    dst_outputs.push(ar);

                    let template = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.ledger.get_prng(),
                        &template,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                if total_sum > amt {
                    // Extras left over go back to src
                    let template = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.ledger.get_prng(),
                        &template,
                    )
                    .c(d!())?;
                    src_outputs.push(ar);

                    let template = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.ledger.get_prng(),
                        &template,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                let src_outputs = src_outputs;
                let dst_outputs = dst_outputs;
                let all_outputs = all_outputs;
                debug_assert!(!src_records.is_empty());
                // dbg!(unit_code.val);
                // for (ix, rec) in src_records.iter().enumerate() {
                // dbg!(ix,
                //      rec.get_asset_type(),
                //      rec.get_amount(),
                //      rec.get_pub_key());
                // }

                let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

                for _ in to_use.iter() {
                    sig_keys.push(
                        XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes())
                            .c(d!(PlatformError::ZeiError(None)))?,
                    );
                }

                let src_records: Vec<AssetRecord> = src_records
                    .iter()
                    .map(|oar| {
                        AssetRecord::from_open_asset_record_no_asset_tracing(oar.clone())
                    })
                    .collect();

                let transfer_body = TransferAssetBody::new(
                    self.ledger.get_prng(),
                    to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                    src_records.as_slice(),
                    all_outputs.as_slice(),
                    None,
                    vec![],
                    TransferType::Standard,
                )
                .c(d!())?;

                let mut owners_memos = transfer_body.transfer.owners_memos.clone();
                // dbg!(&transfer_body);
                let transfer_sig =
                    transfer_body.compute_body_signature(&src_keypair, None);

                let transfer = TransferAsset {
                    body: transfer_body,
                    body_signatures: vec![transfer_sig],
                };
                let seq_id = self.ledger.get_block_commit_count();
                let txn = Transaction {
                    body: TransactionBody {
                        no_replay_token: NoReplayToken::new(
                            self.ledger.get_prng(),
                            seq_id,
                        ),
                        operations: vec![Operation::TransferAsset(transfer)],
                        credentials: vec![],
                        memos: vec![],
                        policy_options: None,
                    },
                    signatures: vec![],
                };

                let effect = TxnEffect::compute_effect(txn).c(d!())?;

                let mut block = self.ledger.start_block().c(d!())?;
                let temp_sid =
                    self.ledger.apply_transaction(&mut block, effect).c(d!())?;

                let (_, txos) = self
                    .ledger
                    .finish_block(block)
                    .c(d!())?
                    .remove(&temp_sid)
                    .unwrap();

                debug_assert!(txos.len() == src_outputs.len() + dst_outputs.len());

                self.utxos
                    .get_mut(dst)
                    .c(d!())?
                    .extend(&txos[..dst_outputs.len()]);
                self.utxos
                    .get_mut(src)
                    .c(d!())?
                    .extend(&txos[dst_outputs.len()..]);

                for (txo_sid, owner_memo) in txos.iter().zip(owners_memos.drain(..)) {
                    if let Some(memo) = owner_memo {
                        self.owner_memos.insert(*txo_sid, memo);
                    }
                }
            }
            AccountsCommand::ToggleConfAmts() => {
                self.confidential_amounts = !conf_amts;
            }
            AccountsCommand::ToggleConfTypes() => {
                self.confidential_types = !conf_types;
            }
        }
        Ok(())
    }
}

// TODO(joe): OneBigLienAccounts, since you can have liens of liens
// It *should* behave the same if there is only ever exactly one UTXO on the ledger, with each
// user's "owning lien" as one of the bound inputs of the top-level lien.
//
// This version only keeps simple liens, but more complex lien assignment questions should be
// tested somehow
#[allow(clippy::type_complexity)]
struct LienAccounts {
    ledger: LedgerState,
    accounts: HashMap<UserName, XfrKeyPair>,
    balances: HashMap<UserName, HashMap<UnitName, u64>>,
    // by account -- lien txo, inputs
    utxos: HashMap<UserName, (TxoSID, Option<Vec<(TxOutput, Option<OwnerMemo>)>>)>,
    units: HashMap<UnitName, (UserName, AssetTypeCode)>, // user, data
    owner_memos: HashMap<TxoSID, OwnerMemo>,
    // These only affect new issuances
    confidential_amounts: bool,
    confidential_types: bool,
}

impl InterpretAccounts<PlatformError> for LienAccounts {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        // dbg!(&cmd);
        let conf_amts = self.confidential_amounts;
        let conf_types = self.confidential_types;
        let iss_art = AssetRecordType::from_flags(conf_amts, false);
        let ctrt_art = AssetRecordType::from_flags(false, false);
        let art = AssetRecordType::from_flags(conf_amts, conf_types);
        // dbg!(cmd);
        match cmd {
            AccountsCommand::NewUser(name) => {
                let keypair = XfrKeyPair::generate(self.ledger.get_prng());

                let ctrt_txo = {
                    // lien contract asset type
                    let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
                    let hash = HashOf::new(name).0.hash;
                    let amt = ((hash.0.iter().fold(0, |x, y| (x as u64) ^ (*y as u64))
                        % 999)
                        + 1);
                    let code = AssetTypeCode::new_from_str(&b64enc(&hash.0));

                    let op1 = {
                        let mut properties: Asset = Asset {
                            code,
                            ..Default::default()
                        };
                        properties.issuer.key = *pubkey;
                        properties.memo.0 = format!("lien contract for '{:?}'", name);

                        let body = DefineAssetBody {
                            asset: Box::new(properties),
                        };

                        Operation::DefineAsset(
                            DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair })
                                .c(d!())?,
                        )
                    };
                    let op2 = {
                        let ar = AssetRecordTemplate::with_no_asset_tracing(
                            amt, code.val, ctrt_art, *pubkey,
                        );
                        let params = PublicParams::default();
                        let (ba, _, owner_memo) = build_blind_asset_record(
                            self.ledger.get_prng(),
                            &params.pc_gens,
                            &ar,
                            vec![],
                        );

                        let asset_issuance_body = IssueAssetBody::new(
                            &code,
                            0,
                            &[(
                                TxOutput {
                                    id: None,
                                    record: ba,
                                    lien: None,
                                },
                                owner_memo,
                            )],
                        )
                        .c(d!())?;

                        let asset_issuance_operation = IssueAsset::new(
                            asset_issuance_body,
                            &IssuerKeyPair { keypair: &keypair },
                        )
                        .c(d!())?;

                        Operation::IssueAsset(asset_issuance_operation)
                    };

                    let seq_id = self.ledger.get_block_commit_count();
                    let txn = Transaction {
                        body: TransactionBody {
                            no_replay_token: NoReplayToken::new(
                                self.ledger.get_prng(),
                                seq_id,
                            ),
                            operations: vec![op1, op2],
                            credentials: vec![],
                            memos: vec![],
                            policy_options: None,
                        },
                        signatures: vec![],
                    };

                    let eff = TxnEffect::compute_effect(txn).c(d!())?;

                    {
                        let mut block = self.ledger.start_block().c(d!())?;

                        let temp_sid =
                            self.ledger.apply_transaction(&mut block, eff).c(d!())?;

                        let (_, txos) = self
                            .ledger
                            .finish_block(block)
                            .c(d!())?
                            .remove(&temp_sid)
                            .unwrap();

                        debug_assert!(txos.len() == 1);

                        txos[0]
                    }
                };

                self.accounts.get(name).c(d!(inp_fail!()))?;

                self.accounts.insert(name.clone(), keypair);
                self.utxos.insert(name.clone(), (ctrt_txo, None));
                self.balances.insert(name.clone(), HashMap::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

                self.units.get(name).c(d!(inp_fail!()))?;

                let code = AssetTypeCode::gen_random();

                let mut properties: Asset = Asset {
                    code,
                    ..Default::default()
                };
                properties.issuer.key = *pubkey;
                properties.memo.0 =
                    format!("asset '{:?}' issued by '{:?}'", name, issuer);

                let body = DefineAssetBody {
                    asset: Box::new(properties),
                };

                let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair })
                    .c(d!())?;

                let seq_id = self.ledger.get_block_commit_count();
                let txn = Transaction {
                    body: TransactionBody {
                        no_replay_token: NoReplayToken::new(
                            self.ledger.get_prng(),
                            seq_id,
                        ),
                        operations: vec![Operation::DefineAsset(op)],
                        credentials: vec![],
                        memos: vec![],
                        policy_options: None,
                    },
                    signatures: vec![],
                };

                let eff = TxnEffect::compute_effect(txn).c(d!())?;

                {
                    let mut block = self.ledger.start_block().c(d!())?;
                    if let Err(e) = self.ledger.apply_transaction(&mut block, eff) {
                        self.ledger.abort_block(block);
                        return Err(eg!(e));
                    }
                    self.ledger.finish_block(block).c(d!())?;
                }

                self.units.insert(name.clone(), (issuer.clone(), code));

                for (_, bal) in self.balances.iter_mut() {
                    bal.insert(name.clone(), 0);
                }
            }
            AccountsCommand::Mint(amt, unit) => {
                let amt = *amt as u64;
                let (issuer, code) = self.units.get(unit).c(d!(inp_fail!()))?;

                let new_seq_num = self.ledger.get_issuance_num(&code).c(d!())?;

                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
                let (ctrt_txo, already_bound) = self.utxos.get_mut(issuer).c(d!())?;
                let ctrt_record = {
                    let txo = (self.ledger.get_utxo(*ctrt_txo).c(d!())?.utxo.0);
                    let memo = self.owner_memos.get(&ctrt_txo).cloned();
                    let open_rec =
                        open_blind_asset_record(&txo.record, &memo, &keypair).c(d!())?;

                    AssetRecord::from_open_asset_record_no_asset_tracing(open_rec)
                };

                *self
                    .balances
                    .get_mut(issuer)
                    .c(d!())?
                    .get_mut(unit)
                    .unwrap() += amt;

                let mut tx =
                    Transaction::from_seq_id(self.ledger.get_block_commit_count());

                let (iss_record, issue_op) = {
                    let params = PublicParams::default();
                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        amt, code.val, iss_art, *pubkey,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.ledger.get_prng(),
                        &ar,
                    )
                    .c(d!())?;
                    let (ba, owner_memo) = (
                        ar.open_asset_record.blind_asset_record.clone(),
                        ar.owner_memo.clone(),
                    );

                    let iss_txo = TxOutput {
                        id: None,
                        record: ba,
                        lien: None,
                    };

                    let asset_issuance_body = IssueAssetBody::new(
                        &code,
                        new_seq_num,
                        &[(iss_txo, owner_memo)],
                    )
                    .c(d!())?;

                    let asset_issuance_operation = IssueAsset::new(
                        asset_issuance_body,
                        &IssuerKeyPair { keypair: &keypair },
                    )
                    .c(d!())?;

                    (ar, Operation::IssueAsset(asset_issuance_operation))
                };
                tx.body.operations.push(issue_op);

                let (lien_ops, new_lien, lien_record) = {
                    let mut rel_ops = vec![];
                    let mut bind_ctrt = TxoRef::Absolute(*ctrt_txo);
                    let mut bind_inp_refs = vec![TxoRef::Relative(0)];
                    let mut bind_inps = vec![iss_record];
                    match already_bound {
                        None => {}
                        Some(inps) => {
                            let (rel_in_records, rel_out_records) = {
                                let mut in_records = vec![];
                                let mut out_records = vec![];

                                {
                                    // contract
                                    in_records.push(ctrt_record.clone());
                                    let open_rec = ctrt_record.open_asset_record.clone();

                                    let amt = *open_rec.get_amount();
                                    let unit_code = *open_rec.get_asset_type();
                                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                                        amt, unit_code, ctrt_art, *pubkey,
                                    );
                                    let ar =
                                        AssetRecord::from_template_no_identity_tracing(
                                            self.ledger.get_prng(),
                                            &ar,
                                        )
                                        .c(d!())?;
                                    out_records.push(ar);
                                }

                                for (ix, (i, memo)) in inps.iter().enumerate() {
                                    let blind_rec = i.record.clone();

                                    let open_rec = open_blind_asset_record(
                                        &blind_rec, &memo, &keypair,
                                    )
                                    .c(d!())?;

                                    in_records.push(AssetRecord::from_open_asset_record_no_asset_tracing(open_rec.clone()));

                                    let amt = *open_rec.get_amount();
                                    let unit_code = *open_rec.get_asset_type();
                                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                                        amt, unit_code, art, *pubkey,
                                    );
                                    let ar =
                                        AssetRecord::from_template_no_identity_tracing(
                                            self.ledger.get_prng(),
                                            &ar,
                                        )
                                        .c(d!())?;
                                    out_records.push(ar.clone());
                                    bind_inps.push(ar);
                                    bind_inp_refs.push(TxoRef::Relative(
                                        (inps.len() - 1 - ix) as u64,
                                    ))
                                }

                                (in_records, out_records)
                            };

                            let rel_op = {
                                let body = ReleaseAssetsBody::new(
                                    self.ledger.get_prng(),
                                    bind_ctrt,
                                    HashOf::new(
                                        &inps
                                            .iter()
                                            .map(|(txo, _)| txo)
                                            .cloned()
                                            .collect(),
                                    ),
                                    vec![],
                                    &rel_in_records,
                                    &rel_out_records,
                                )
                                .c(d!())?;
                                let sig = body.compute_body_signature(&keypair, None);
                                Operation::ReleaseAssets(ReleaseAssets {
                                    body,
                                    body_signatures: vec![sig],
                                })
                            };

                            bind_inp_refs[0] =
                                TxoRef::Relative((1 /* contract */ + inps.len()) as u64);
                            bind_ctrt = TxoRef::Relative(inps.len() as u64);

                            rel_ops.push(rel_op);
                        }
                    }

                    let (bind_op, lien_record) = {
                        let out_rec = {
                            // contract
                            let open_rec = ctrt_record.open_asset_record.clone();

                            let amt = *open_rec.get_amount();
                            let unit_code = *open_rec.get_asset_type();
                            let ar = AssetRecordTemplate::with_no_asset_tracing(
                                amt, unit_code, ctrt_art, *pubkey,
                            );
                            AssetRecord::from_template_no_identity_tracing(
                                self.ledger.get_prng(),
                                &ar,
                            )
                            .c(d!())?
                        };

                        // dbg!(&bind_ctrt, &bind_inp_refs, &bind_inps);
                        let body = BindAssetsBody::new(
                            self.ledger.get_prng(),
                            bind_ctrt,
                            bind_inp_refs.iter().map(|x| (*x, None)).collect(),
                            &(once(ctrt_record)
                                .chain(bind_inps.iter().cloned())
                                .collect::<Vec<_>>()),
                            &out_rec,
                        )
                        .c(d!())?;
                        let sig = body.compute_body_signature(&keypair, None);
                        (
                            Operation::BindAssets(BindAssets {
                                body,
                                body_signatures: vec![sig],
                            }),
                            out_rec,
                        )
                    };

                    rel_ops.push(bind_op);
                    (rel_ops, bind_inps, lien_record)
                };

                tx.body.operations.extend(lien_ops);

                let effect = TxnEffect::compute_effect(tx).c(d!())?;

                let mut block = self.ledger.start_block().c(d!())?;
                let temp_sid =
                    self.ledger.apply_transaction(&mut block, effect).c(d!())?;

                let (_, txos) = self
                    .ledger
                    .finish_block(block)
                    .c(d!())?
                    .remove(&temp_sid)
                    .unwrap();

                debug_assert!(txos.len() == 1);
                if let Some(memo) = lien_record.owner_memo {
                    self.owner_memos.insert(txos[0], memo);
                }
                *ctrt_txo = txos[0];
                *already_bound = Some(
                    new_lien
                        .into_iter()
                        .map(|x| {
                            (
                                TxOutput {
                                    id: None,
                                    record: x.open_asset_record.blind_asset_record,
                                    lien: None,
                                },
                                x.owner_memo,
                            )
                        })
                        .collect(),
                );
            }
            AccountsCommand::Send(src, amt, unit, dst) => {
                let amt = *amt as u64;
                let src_keypair = self.accounts.get(src).c(d!(inp_fail!()))?;
                let (src_pub, src_priv) =
                    (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
                let dst_keypair = self.accounts.get(dst).c(d!(inp_fail!()))?;
                let (dst_pub, dst_priv) =
                    (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
                let (_, unit_code) = self.units.get(unit).c(d!(inp_fail!()))?;

                if *self.balances.get(src).c(d!())?.get(unit).c(d!())? < amt {
                    return Err(eg!(inp_fail!()));
                }
                if amt == 0 {
                    return Ok(());
                }

                *self.balances.get_mut(src).c(d!())?.get_mut(unit).c(d!())? -= amt;

                let (src_ctrt, src_txos) = self.utxos.remove(src).c(d!())?;

                let src_txos = match src_txos {
                    Some(txos) => txos,
                    None => {
                        return Err(eg!(inp_fail!()));
                    }
                };

                let (sent_txo, sent_record) = {
                    let mut txn_txos = vec![];
                    let mut ops = vec![];

                    // release src records, get avail and get the index in txn_txos of
                    // the src contract
                    let (mut avail, new_src_ctrt_ix) = {
                        let ctrt_txo = src_ctrt;
                        let ctrt_record = {
                            let txo = (self.ledger.get_utxo(src_ctrt).c(d!())?.utxo.0);
                            let memo = self.owner_memos.get(&src_ctrt).cloned();
                            let open_rec = open_blind_asset_record(
                                &txo.record,
                                &memo,
                                &src_keypair,
                            )
                            .c(d!())?;

                            AssetRecord::from_open_asset_record_no_asset_tracing(
                                open_rec,
                            )
                        };
                        let mut avail = VecDeque::new();
                        let (rel_in_records, rel_out_records, new_ctrt) = {
                            let mut in_records = vec![];
                            let mut out_records = vec![];

                            let new_ctrt_ix = {
                                // contract
                                let open_rec = ctrt_record.open_asset_record.clone();
                                in_records.push(ctrt_record);

                                let amt = *open_rec.get_amount();
                                let unit_code = *open_rec.get_asset_type();
                                let ar = AssetRecordTemplate::with_no_asset_tracing(
                                    amt, unit_code, ctrt_art, *src_pub,
                                );
                                let ar = AssetRecord::from_template_no_identity_tracing(
                                    self.ledger.get_prng(),
                                    &ar,
                                )
                                .c(d!())?;
                                let ix = txn_txos.len();
                                out_records.push(ar.clone());
                                txn_txos.push(Some(ar));
                                ix
                            };

                            for (i, memo) in src_txos.iter() {
                                let blind_rec = i.record.clone();

                                let open_rec = open_blind_asset_record(
                                    &blind_rec,
                                    &memo,
                                    &src_keypair,
                                )
                                .c(d!())?;

                                in_records.push(
                                    AssetRecord::from_open_asset_record_no_asset_tracing(
                                        open_rec.clone(),
                                    ),
                                );

                                let amt = *open_rec.get_amount();
                                let unit_code = *open_rec.get_asset_type();
                                let ar = AssetRecordTemplate::with_no_asset_tracing(
                                    amt, unit_code, art, *src_pub,
                                );
                                let ar = AssetRecord::from_template_no_identity_tracing(
                                    self.ledger.get_prng(),
                                    &ar,
                                )
                                .c(d!())?;
                                let ix = txn_txos.len();
                                out_records.push(ar.clone());
                                txn_txos.push(Some(ar));
                                avail.push_back(ix);
                            }

                            (in_records, out_records, new_ctrt_ix)
                        };

                        let rel_op = {
                            let body = ReleaseAssetsBody::new(
                                self.ledger.get_prng(),
                                TxoRef::Absolute(src_ctrt),
                                HashOf::new(
                                    &src_txos
                                        .iter()
                                        .map(|(txo, _)| txo)
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                ),
                                vec![],
                                &rel_in_records,
                                &rel_out_records,
                            )
                            .c(d!())?;
                            let sig = body.compute_body_signature(&src_keypair, None);
                            Operation::ReleaseAssets(ReleaseAssets {
                                body,
                                body_signatures: vec![sig],
                            })
                        };

                        ops.push(rel_op);
                        (avail, new_ctrt)
                    };

                    // ================
                    // Now we have all src records in `txn_txos`, and a list of usable
                    // (ie, non-contract) indices in `avail`

                    // Transfer
                    let sent_record = {
                        let mut total_sum = 0u64;
                        let mut src_records: Vec<AssetRecord> = Vec::new();
                        let mut to_use = Vec::new();
                        let mut to_skip = Vec::new();

                        while total_sum < amt && !avail.is_empty() {
                            let txo_ix = avail.pop_front().c(d!())?;
                            let open_rec = txn_txos
                                .get(txo_ix)
                                .c(d!())?
                                .clone()
                                .unwrap()
                                .open_asset_record;

                            if *open_rec.get_asset_type() != unit_code.val {
                                to_skip.push(txo_ix);
                                continue;
                            }

                            debug_assert!(*open_rec.get_asset_type() == unit_code.val);
                            to_use.push(txo_ix);
                            total_sum += *open_rec.get_amount();
                            src_records.push(
                                mem::replace(txn_txos.get_mut(txo_ix).c(d!())?, None)
                                    .unwrap(),
                            );
                        }
                        avail.extend(to_skip);

                        let inp_txo_len = txn_txos.len();
                        debug_assert!(total_sum >= amt);

                        let mut src_outputs = Vec::new();
                        let mut dst_outputs = Vec::new();
                        let mut all_outputs: Vec<AssetRecord> = Vec::new();

                        {
                            // Simple output to dst
                            let template = AssetRecordTemplate::with_no_asset_tracing(
                                amt,
                                unit_code.val,
                                art,
                                *dst_pub,
                            );
                            let ar = AssetRecord::from_template_no_identity_tracing(
                                self.ledger.get_prng(),
                                &template,
                            )
                            .c(d!())?;
                            dst_outputs.push(txn_txos.len());
                            txn_txos.push(Some(ar.clone()));

                            all_outputs.push(ar);
                        }

                        if total_sum > amt {
                            // Extras left over go back to src
                            let template = AssetRecordTemplate::with_no_asset_tracing(
                                total_sum - amt,
                                unit_code.val,
                                art,
                                *src_pub,
                            );
                            let ar = AssetRecord::from_template_no_identity_tracing(
                                self.ledger.get_prng(),
                                &template,
                            )
                            .c(d!())?;
                            src_outputs.push(txn_txos.len());
                            txn_txos.push(Some(ar.clone()));

                            all_outputs.push(ar);
                        }

                        let src_outputs = src_outputs;
                        let dst_outputs = dst_outputs;
                        let all_outputs = all_outputs;
                        debug_assert!(!src_records.is_empty());

                        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

                        for _ in to_use.iter() {
                            sig_keys.push(
                                XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes())
                                    .c(d!())?,
                            );
                        }

                        let transfer_body = TransferAssetBody::new(
                            self.ledger.get_prng(),
                            to_use
                                .iter()
                                .cloned()
                                .map(|i| TxoRef::Relative((inp_txo_len - 1 - i) as u64))
                                .collect(),
                            src_records.as_slice(),
                            all_outputs.as_slice(),
                            None,
                            vec![],
                            TransferType::Standard,
                        )
                        .c(d!())?;

                        debug_assert!(transfer_body.transfer.outputs.len() <= 2);

                        assert_eq!(dst_outputs.len(), 1);
                        avail.extend(src_outputs.iter());

                        // dbg!(&transfer_body);
                        let transfer_sig =
                            transfer_body.compute_body_signature(&src_keypair, None);

                        let transfer = Operation::TransferAsset(TransferAsset {
                            body: transfer_body,
                            body_signatures: vec![transfer_sig],
                        });

                        ops.push(transfer);
                        txn_txos[dst_outputs[0]].clone().c(d!())?
                    };

                    // sanity check `avail`:
                    {
                        let mut avail_total = 0;
                        for ix in avail.iter() {
                            let rec = txn_txos
                                .get(*ix)
                                .c(d!())?
                                .clone()
                                .unwrap()
                                .open_asset_record;
                            if *rec.get_asset_type() == unit_code.val {
                                avail_total += rec.amount;
                            }
                            debug_assert!(&rec.blind_asset_record.public_key == src_pub);
                        }
                        assert_eq!(
                            avail_total,
                            *self.balances.get(src).c(d!())?.get(unit).c(d!())?
                        );
                    }

                    let mut lien_records = vec![];
                    let (sent_ix, lien_ix, src_ctrt) = if !avail.is_empty() {
                        // bind src's stuff back together
                        let (new_src_ctrt_ix, new_src_ctrt) = {
                            let (bind_op, lien_ix, new_ctrt) = {
                                let mut in_records = vec![];
                                let mut in_refs = vec![];
                                let mut lien = vec![];

                                let (contract_ref, out_record) = {
                                    let ix = new_src_ctrt_ix;
                                    let rec = mem::replace(
                                        txn_txos.get_mut(ix).c(d!())?,
                                        None,
                                    )
                                    .unwrap();
                                    let open_rec = &rec.open_asset_record;
                                    let amt = *open_rec.get_amount();
                                    let unit_code = *open_rec.get_asset_type();

                                    in_records.push(rec);
                                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                                        amt, unit_code, ctrt_art, *src_pub,
                                    );
                                    let ar =
                                        AssetRecord::from_template_no_identity_tracing(
                                            self.ledger.get_prng(),
                                            &ar,
                                        )
                                        .c(d!())?;

                                    (
                                        TxoRef::Relative(
                                            (txn_txos.len() - 1 - ix) as u64,
                                        ),
                                        ar,
                                    )
                                };

                                for ix in avail {
                                    let rec = mem::replace(
                                        txn_txos.get_mut(ix).c(d!())?,
                                        None,
                                    )
                                    .unwrap();

                                    lien.push(TxOutput {
                                        id: None,
                                        record: rec
                                            .open_asset_record
                                            .blind_asset_record
                                            .clone(),
                                        lien: None,
                                    });
                                    lien_records.push(rec.clone());
                                    in_refs.push((
                                        TxoRef::Relative(
                                            (txn_txos.len() - 1 - ix) as u64,
                                        ),
                                        None,
                                    ));
                                    in_records.push(rec);
                                }

                                let lien_ix = txn_txos.len();
                                txn_txos.push(Some(out_record.clone()));
                                let new_ctrt = out_record.clone();

                                let body = BindAssetsBody::new(
                                    self.ledger.get_prng(),
                                    contract_ref,
                                    in_refs,
                                    &in_records,
                                    &out_record,
                                )
                                .c(d!())?;

                                let sig =
                                    body.compute_body_signature(&src_keypair, None);
                                (
                                    Operation::BindAssets(BindAssets {
                                        body,
                                        body_signatures: vec![sig],
                                    }),
                                    lien_ix,
                                    new_ctrt,
                                )
                            };

                            ops.push(bind_op);

                            (lien_ix, new_ctrt)
                        };

                        // we released the lien into [ctrt,entries...] then sent the
                        // entries into a single TXO, then rebound things together
                        // [None,None,None,...,None,dst_txo,lien]
                        // Thus the index lookup for the dst txo will be 0 and the ctrt
                        // will be 1
                        (0, 1, new_src_ctrt)
                    } else {
                        let ix = new_src_ctrt_ix;
                        let rec = txn_txos.get(ix).c(d!())?.clone().c(d!())?;

                        // we released the lien into [ctrt,entries...] then sent the
                        // entries into a single TXO, so our final transaction will
                        // have TXOs:
                        // [ctrt,None,None,...,None,dst_txo]
                        // Thus the index lookup for the dst txo will be 1 and the ctrt
                        // will be 0
                        (1, 0, rec)
                    };

                    // ========
                    // Now we should know:
                    //  - Src's stuff has been bound back together, and its record is
                    //    at new_src_ctrt_ix
                    //  - The only remaining live TXOs are the TXO sent to dst, and the
                    //    TXO for src's lien
                    //
                    // So the only thing we need to do is open dst's lien and rebind it
                    // together
                    // ========

                    // STRATEGY:
                    //  - submit this as a single transaction
                    //  - submit rebind of dst

                    let seq_id = self.ledger.get_block_commit_count();
                    let txn = Transaction {
                        body: TransactionBody {
                            no_replay_token: NoReplayToken::new(
                                self.ledger.get_prng(),
                                seq_id,
                            ),
                            operations: ops,
                            credentials: vec![],
                            memos: vec![],
                            policy_options: None,
                        },
                        signatures: vec![],
                    };

                    let effect = TxnEffect::compute_effect(txn).c(d!())?;

                    let mut block = self.ledger.start_block().c(d!())?;
                    let temp_sid =
                        self.ledger.apply_transaction(&mut block, effect).c(d!())?;

                    let (_, txos) = self
                        .ledger
                        .finish_block(block)
                        .c(d!())?
                        .remove(&temp_sid)
                        .unwrap();
                    assert_eq!(txos.len(), 2);

                    {
                        // update src's entry in utxos
                        let sent_txo = txos[sent_ix];
                        let new_src_lien_txo = txos[lien_ix];

                        let lien_entry = lien_records
                            .into_iter()
                            .map(|rec| {
                                (
                                    TxOutput {
                                        id: None,
                                        record: rec.open_asset_record.blind_asset_record,
                                        lien: None,
                                    },
                                    rec.owner_memo,
                                )
                            })
                            .collect::<Vec<_>>();
                        let lien_entry = if lien_entry.is_empty() {
                            None
                        } else {
                            Some(lien_entry)
                        };

                        self.utxos
                            .insert(src.clone(), (new_src_lien_txo, lien_entry));
                        if let Some(memo) = src_ctrt.owner_memo {
                            self.owner_memos.insert(new_src_lien_txo, memo);
                        }

                        (sent_txo, sent_record)
                    }
                };

                // We've pulled sent_txo out of src, wrap it back into dst.
                *self.balances.get_mut(dst).c(d!())?.get_mut(unit).c(d!())? += amt;

                let (dst_ctrt, dst_txos) = self.utxos.remove(dst).c(d!())?;

                //////// WRONG!!!!! ////////
                // Need to handle dst_txos == None

                {
                    let mut ops = vec![];
                    let ctrt_record = {
                        let txo = (self.ledger.get_utxo(dst_ctrt).c(d!())?.utxo.0);
                        let memo = self.owner_memos.get(&dst_ctrt).cloned();
                        let open_rec =
                            open_blind_asset_record(&txo.record, &memo, &dst_keypair)
                                .c(d!())?;

                        AssetRecord::from_open_asset_record_no_asset_tracing(open_rec)
                    };
                    let (dst_ctrt_ref, dst_ctrt, dst_txo_refs, dst_txos) =
                        if let Some(dst_txos) = dst_txos {
                            let (rel_in_records, rel_out_records) = {
                                let mut in_records = vec![];
                                let mut out_records = vec![];

                                {
                                    // contract
                                    let open_rec = ctrt_record.open_asset_record.clone();

                                    in_records.push(ctrt_record);

                                    let amt = *open_rec.get_amount();
                                    let unit_code = *open_rec.get_asset_type();
                                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                                        amt, unit_code, ctrt_art, *dst_pub,
                                    );
                                    let ar =
                                        AssetRecord::from_template_no_identity_tracing(
                                            self.ledger.get_prng(),
                                            &ar,
                                        )
                                        .c(d!())?;
                                    out_records.push(ar);
                                }

                                for (i, memo) in dst_txos.iter() {
                                    let blind_rec = i.record.clone();

                                    let open_rec = open_blind_asset_record(
                                        &blind_rec,
                                        &memo,
                                        &dst_keypair,
                                    )
                                    .c(d!())?;

                                    in_records.push(
                                    AssetRecord::from_open_asset_record_no_asset_tracing(
                                        open_rec.clone(),
                                    ),
                                );

                                    let amt = *open_rec.get_amount();
                                    let unit_code = *open_rec.get_asset_type();
                                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                                        amt, unit_code, art, *dst_pub,
                                    );
                                    let ar =
                                        AssetRecord::from_template_no_identity_tracing(
                                            self.ledger.get_prng(),
                                            &ar,
                                        )
                                        .c(d!())?;
                                    out_records.push(ar.clone());
                                }

                                (in_records, out_records)
                            };

                            let rel_op = {
                                let body = ReleaseAssetsBody::new(
                                    self.ledger.get_prng(),
                                    TxoRef::Absolute(dst_ctrt),
                                    HashOf::new(
                                        &dst_txos
                                            .iter()
                                            .map(|(txo, _)| txo)
                                            .cloned()
                                            .collect::<Vec<_>>(),
                                    ),
                                    vec![],
                                    &rel_in_records,
                                    &rel_out_records,
                                )
                                .c(d!())?;
                                let sig =
                                    body.compute_body_signature(&dst_keypair, None);
                                Operation::ReleaseAssets(ReleaseAssets {
                                    body,
                                    body_signatures: vec![sig],
                                })
                            };

                            ops.push(rel_op);
                            (
                                TxoRef::Relative((rel_out_records.len() - 1) as u64),
                                rel_out_records[0].clone(),
                                (1..rel_out_records.len())
                                    .map(|i| {
                                        TxoRef::Relative(
                                            (rel_out_records.len() - 1 - i) as u64,
                                        )
                                    })
                                    .collect(),
                                rel_out_records[1..].to_vec(),
                            )
                        } else {
                            (TxoRef::Absolute(dst_ctrt), ctrt_record, vec![], vec![])
                        };

                    let mut lien_records = vec![];
                    {
                        let mut in_records = vec![];
                        let mut in_refs = vec![];

                        let (contract_ref, out_record) = {
                            let rec = dst_ctrt.clone();
                            let open_rec = &rec.open_asset_record;
                            let amt = *open_rec.get_amount();
                            let unit_code = *open_rec.get_asset_type();

                            in_records.push(rec.clone());
                            let ar = AssetRecordTemplate::with_no_asset_tracing(
                                amt, unit_code, ctrt_art, *dst_pub,
                            );
                            let ar = AssetRecord::from_template_no_identity_tracing(
                                self.ledger.get_prng(),
                                &ar,
                            )
                            .c(d!())?;

                            (dst_ctrt_ref, ar)
                        };

                        for (txo_ref, txo) in
                            dst_txo_refs.into_iter().zip(dst_txos.into_iter())
                        {
                            in_refs.push(txo_ref);
                            lien_records.push(txo.clone());
                            in_records.push(txo.clone());
                        }

                        in_refs.push(TxoRef::Absolute(sent_txo));
                        lien_records.push(sent_record.clone());
                        in_records.push(sent_record);

                        let body = BindAssetsBody::new(
                            self.ledger.get_prng(),
                            contract_ref,
                            in_refs.into_iter().map(|x| (x, None)).collect(),
                            &in_records,
                            &out_record,
                        )
                        .c(d!())?;

                        let sig = body.compute_body_signature(&dst_keypair, None);
                        ops.push(Operation::BindAssets(BindAssets {
                            body,
                            body_signatures: vec![sig],
                        }));
                    }

                    let seq_id = self.ledger.get_block_commit_count();
                    let txn = Transaction {
                        body: TransactionBody {
                            no_replay_token: NoReplayToken::new(
                                self.ledger.get_prng(),
                                seq_id,
                            ),
                            operations: ops,
                            credentials: vec![],
                            memos: vec![],
                            policy_options: None,
                        },
                        signatures: vec![],
                    };

                    let effect = TxnEffect::compute_effect(txn).c(d!())?;

                    let mut block = self.ledger.start_block().c(d!())?;
                    let temp_sid =
                        self.ledger.apply_transaction(&mut block, effect).c(d!())?;

                    let (_, txos) = self
                        .ledger
                        .finish_block(block)
                        .c(d!())?
                        .remove(&temp_sid)
                        .unwrap();
                    assert_eq!(txos.len(), 1);

                    {
                        // update dst's entry in utxos
                        let new_dst_lien_txo = txos[0];

                        let lien_entry = lien_records
                            .into_iter()
                            .map(|rec| {
                                (
                                    TxOutput {
                                        id: None,
                                        record: rec.open_asset_record.blind_asset_record,
                                        lien: None,
                                    },
                                    rec.owner_memo,
                                )
                            })
                            .collect::<Vec<_>>();
                        let lien_entry = if lien_entry.is_empty() {
                            None
                        } else {
                            Some(lien_entry)
                        };

                        self.utxos
                            .insert(dst.clone(), (new_dst_lien_txo, lien_entry));
                        if let Some(memo) = dst_ctrt.owner_memo {
                            self.owner_memos.insert(new_dst_lien_txo, memo);
                        }
                    }
                }
            }
            AccountsCommand::ToggleConfAmts() => {
                self.confidential_amounts = !conf_amts;
            }
            AccountsCommand::ToggleConfTypes() => {
                self.confidential_types = !conf_types;
            }
        }
        Ok(())
    }
}

struct OneBigTxnAccounts {
    base_ledger: LedgerState,
    txn: Transaction,
    accounts: HashMap<UserName, XfrKeyPair>,
    balances: HashMap<UserName, HashMap<UnitName, u64>>,
    txos: Vec<(TxOutput, Option<OwnerMemo>)>,
    utxos: HashMap<UserName, VecDeque<usize>>, // by account
    units: HashMap<UnitName, (UserName, AssetTypeCode, u64)>, // user, data, issuance num
    // These only affect new issuances
    confidential_amounts: bool,
    confidential_types: bool,
}

impl InterpretAccounts<PlatformError> for OneBigTxnAccounts {
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        let conf_amts = self.confidential_amounts;
        let conf_types = self.confidential_types;
        let iss_art = AssetRecordType::from_flags(conf_amts, false);
        let art = AssetRecordType::from_flags(conf_amts, conf_types);
        // dbg!(cmd);
        match cmd {
            AccountsCommand::NewUser(name) => {
                let keypair = XfrKeyPair::generate(self.base_ledger.get_prng());

                self.accounts.get(name).c(d!(inp_fail!()))?;

                // dbg!("New user", &name, &keypair);

                self.accounts.insert(name.clone(), keypair);
                self.utxos.insert(name.clone(), VecDeque::new());
                self.balances.insert(name.clone(), HashMap::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

                self.units.get(name).c(d!(inp_fail!()))?;

                let code = AssetTypeCode::gen_random();

                // dbg!("New unit", &name, &issuer, &code);

                let mut properties: Asset = Asset {
                    code,
                    ..Default::default()
                };
                properties.issuer.key = *pubkey;
                properties.memo.0 =
                    format!("asset '{:?}' issued by '{:?}'", name, issuer);

                let body = DefineAssetBody {
                    asset: Box::new(properties),
                };

                let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair })
                    .c(d!())?;

                self.txn.body.operations.push(Operation::DefineAsset(op));
                let eff = TxnEffect::compute_effect(self.txn.clone()).c(d!())?;
                let eff = self
                    .base_ledger
                    .TESTING_get_status()
                    .TESTING_check_txn_effects(eff)
                    .c(d!())?;

                self.txn = eff.txn;

                self.units.insert(name.clone(), (issuer.clone(), code, 0));

                for (_, bal) in self.balances.iter_mut() {
                    bal.insert(name.clone(), 0);
                }
            }
            AccountsCommand::Mint(amt, unit) => {
                let amt = *amt as u64;
                let (issuer, code, new_seq_num) =
                    self.units.get_mut(unit).c(d!(inp_fail!()))?;
                *new_seq_num += 1;
                let new_seq_num = *new_seq_num - 1;

                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
                let utxos = self.utxos.get_mut(issuer).c(d!())?;

                *self
                    .balances
                    .get_mut(issuer)
                    .c(d!())?
                    .get_mut(unit)
                    .unwrap() += amt;

                let ar = AssetRecordTemplate::with_no_asset_tracing(
                    amt, code.val, iss_art, *pubkey,
                );
                let params = PublicParams::default();
                let (ba, _, owner_memo) = build_blind_asset_record(
                    self.base_ledger.get_prng(),
                    &params.pc_gens,
                    &ar,
                    vec![],
                );

                let asset_issuance_body = IssueAssetBody::new(
                    &code,
                    new_seq_num,
                    &[(
                        TxOutput {
                            id: None,
                            record: ba,
                            lien: None,
                        },
                        owner_memo.clone(),
                    )],
                )
                .c(d!())?;

                let asset_issuance_operation = IssueAsset::new(
                    asset_issuance_body,
                    &IssuerKeyPair { keypair: &keypair },
                )
                .c(d!())?;

                let issue_op = Operation::IssueAsset(asset_issuance_operation);

                self.txn.body.operations.push(issue_op);
                let effect = TxnEffect::compute_effect(self.txn.clone()).c(d!())?;
                let effect = self
                    .base_ledger
                    .TESTING_get_status()
                    .TESTING_check_txn_effects(effect)
                    .c(d!())?;
                self.txn = effect.txn;

                debug_assert!(effect.txos.last().c(d!())?.is_some());
                debug_assert!(effect.txos.len() == self.txos.len() + 1);
                utxos.push_back(effect.txos.len() - 1);

                self.txos.push((
                    effect.txos[self.txos.len()].as_ref().c(d!())?.clone(),
                    owner_memo,
                ));
            }
            AccountsCommand::Send(src, amt, unit, dst) => {
                let amt = *amt as u64;
                let src_keypair = self.accounts.get(src).c(d!(inp_fail!()))?;
                let (src_pub, src_priv) =
                    (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
                let dst_keypair = self.accounts.get(dst).c(d!(inp_fail!()))?;
                let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
                let (_, unit_code, _) = self.units.get(unit).c(d!(inp_fail!()))?;

                if *self.balances.get(src).c(d!())?.get(unit).c(d!())? < amt {
                    return Err(eg!(inp_fail!()));
                }
                if amt == 0 {
                    return Ok(());
                }

                *self.balances.get_mut(src).c(d!())?.get_mut(unit).c(d!())? -= amt;
                *self.balances.get_mut(dst).c(d!())?.get_mut(unit).c(d!())? += amt;

                let mut src_records: Vec<OpenAssetRecord> = Vec::new();
                let mut total_sum = 0u64;
                let avail = self.utxos.get_mut(src).c(d!())?;
                let mut to_use: Vec<usize> = Vec::new();
                let mut to_skip: Vec<usize> = Vec::new();

                while total_sum < amt && !avail.is_empty() {
                    let sid = avail.pop_front().c(d!())?;
                    let blind_rec = &((self.txos.get(sid).c(d!())?.0).record);
                    let memo = &(self.txos.get(sid).c(d!())?.1);
                    let open_rec =
                        open_blind_asset_record(&blind_rec, &memo, &src_keypair)
                            .c(d!())?;
                    // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
                    if *open_rec.get_asset_type() != unit_code.val {
                        to_skip.push(sid);
                        continue;
                    }

                    debug_assert!(*open_rec.get_asset_type() == unit_code.val);
                    to_use.push(self.txos.len() - 1 - sid);
                    total_sum += *open_rec.get_amount();
                    src_records.push(open_rec);
                }
                // dbg!(&to_skip, &to_use);
                avail.extend(to_skip.into_iter());

                debug_assert!(total_sum >= amt);

                let mut src_outputs: Vec<AssetRecord> = Vec::new();
                let mut dst_outputs: Vec<AssetRecord> = Vec::new();
                let mut all_outputs: Vec<AssetRecord> = Vec::new();

                {
                    // Simple output to dst
                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.base_ledger.get_prng(),
                        &ar,
                    )
                    .c(d!())?;
                    dst_outputs.push(ar);

                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.base_ledger.get_prng(),
                        &ar,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                if total_sum > amt {
                    // Extras left over go back to src
                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.base_ledger.get_prng(),
                        &ar,
                    )
                    .c(d!())?;
                    src_outputs.push(ar);

                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        self.base_ledger.get_prng(),
                        &ar,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                let src_outputs = src_outputs;
                let dst_outputs = dst_outputs;
                let all_outputs = all_outputs;
                debug_assert!(!src_records.is_empty());
                // dbg!(unit_code.val);
                // for (ix, rec) in src_records.iter().enumerate() {
                // dbg!(ix,
                //      rec.get_asset_type(),
                //      rec.get_amount(),
                //      rec.get_pub_key());
                // }

                let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

                for _ in to_use.iter() {
                    sig_keys.push(
                        XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes())
                            .c(d!(PlatformError::ZeiError(None)))?,
                    );
                }

                let src_records: Vec<AssetRecord> = src_records
                    .iter()
                    .map(|oar| {
                        AssetRecord::from_open_asset_record_no_asset_tracing(oar.clone())
                    })
                    .collect();

                let transfer_body = TransferAssetBody::new(
                    self.base_ledger.get_prng(),
                    to_use
                        .iter()
                        .cloned()
                        .map(|x| TxoRef::Relative(x as u64))
                        .collect(),
                    src_records.as_slice(),
                    all_outputs.as_slice(),
                    None,
                    vec![],
                    TransferType::Standard,
                )
                .c(d!())?;
                let owners_memos = transfer_body.transfer.owners_memos.clone();
                // dbg!(&transfer_body);
                let transfer_sig =
                    transfer_body.compute_body_signature(&src_keypair, None);

                let transfer = TransferAsset {
                    body: transfer_body,
                    body_signatures: vec![transfer_sig],
                };

                self.txn
                    .body
                    .operations
                    .push(Operation::TransferAsset(transfer));

                let effect = TxnEffect::compute_effect(self.txn.clone()).c(d!())?;
                let effect = self
                    .base_ledger
                    .TESTING_get_status()
                    .TESTING_check_txn_effects(effect)
                    .c(d!())?;
                self.txn = effect.txn;

                let txos = effect.txos[self.txos.len()..]
                    .iter()
                    .cloned()
                    .map(|x| x.unwrap())
                    .collect::<Vec<TxOutput>>();
                debug_assert!(txos.len() == src_outputs.len() + dst_outputs.len());

                let txo_sids = (0..txos.len())
                    .map(|x| x + self.txos.len())
                    .collect::<Vec<usize>>();

                self.utxos
                    .get_mut(dst)
                    .c(d!())?
                    .extend(&txo_sids[..dst_outputs.len()]);
                self.utxos
                    .get_mut(src)
                    .c(d!())?
                    .extend(&txo_sids[dst_outputs.len()..]);
                self.txos.extend(
                    txos.iter()
                        .zip(owners_memos.iter())
                        .map(|(txo, memo)| (txo.clone(), memo.as_ref().cloned()))
                        .collect::<Vec<(TxOutput, Option<OwnerMemo>)>>(),
                );
            }
            AccountsCommand::ToggleConfAmts() => {
                self.confidential_amounts = !conf_amts;
            }
            AccountsCommand::ToggleConfTypes() => {
                self.confidential_types = !conf_types;
            }
        }
        Ok(())
    }
}

struct LedgerStandaloneAccounts<T>
where
    T: RestfulLedgerUpdate + RestfulLedgerAccess,
{
    prng: rand_chacha::ChaChaRng,
    client: T,
    accounts: HashMap<UserName, XfrKeyPair>,
    balances: HashMap<UserName, HashMap<UnitName, u64>>,
    utxos: HashMap<UserName, VecDeque<TxoSID>>, // by account
    units: HashMap<UnitName, (UserName, AssetTypeCode)>, // user, data
    owner_memos: HashMap<TxoSID, OwnerMemo>,
    // These only affect new issuances
    confidential_amounts: bool,
    confidential_types: bool,
}

impl<T> LedgerStandaloneAccounts<T>
where
    T: RestfulLedgerAccess + RestfulLedgerUpdate,
{
    fn fetch_seq_id(&mut self) -> u64 {
        self.client.get_block_commit_count().unwrap()
    }
}

impl<T> InterpretAccounts<PlatformError> for LedgerStandaloneAccounts<T>
where
    T: RestfulLedgerAccess + RestfulLedgerUpdate,
{
    fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<()> {
        let conf_amts = self.confidential_amounts;
        let conf_types = self.confidential_types;
        let iss_art = AssetRecordType::from_flags(conf_amts, false);
        let art = AssetRecordType::from_flags(conf_amts, conf_types);
        match cmd {
            AccountsCommand::NewUser(name) => {
                let keypair = XfrKeyPair::generate(&mut self.prng);

                self.accounts.get(name).c(d!(inp_fail!()))?;

                self.accounts.insert(name.clone(), keypair);
                self.utxos.insert(name.clone(), VecDeque::new());
                self.balances.insert(name.clone(), HashMap::new());
            }
            AccountsCommand::NewUnit(name, issuer) => {
                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

                self.units.get(name).c(d!(inp_fail!()))?;

                let code = AssetTypeCode::gen_random();

                // dbg!("New unit", &name, &issuer, &code);

                let mut properties: Asset = Asset {
                    code,
                    ..Default::default()
                };
                properties.issuer.key = *pubkey;
                properties.memo.0 =
                    format!("asset '{:?}' issued by '{:?}'", name, issuer);

                let body = DefineAssetBody {
                    asset: Box::new(properties),
                };

                let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair })
                    .c(d!())?;
                let mut prng = ChaChaRng::from_entropy();
                {
                    let seq_id = self.fetch_seq_id();
                    let txn = Transaction {
                        body: TransactionBody {
                            no_replay_token: NoReplayToken::new(&mut prng, seq_id),
                            operations: vec![Operation::DefineAsset(op)],
                            credentials: vec![],
                            memos: vec![],
                            policy_options: None,
                        },
                        signatures: vec![],
                    };
                    let txn_handle = self.client.submit_transaction(&txn).c(d!())?;
                    self.client.force_end_block().c(d!())?;
                    match self.client.txn_status(&txn_handle).c(d!())? {
                        TxnStatus::Committed((_sid, txos)) => {}
                        _ => panic!("Pending status found when Committed expected"),
                    }
                }

                self.units.insert(name.clone(), (issuer.clone(), code));

                for (_, bal) in self.balances.iter_mut() {
                    bal.insert(name.clone(), 0);
                }
            }
            AccountsCommand::Mint(amt, unit) => {
                let seq_id = self.fetch_seq_id();
                let amt = *amt as u64;
                let (issuer, code) = self.units.get(unit).c(d!(inp_fail!()))?;

                let new_seq_num = self.client.get_issuance_num(&code).c(d!())?;

                let keypair = self.accounts.get(issuer).c(d!(inp_fail!()))?;
                let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
                let utxos = self.utxos.get_mut(issuer).c(d!())?;

                *self
                    .balances
                    .get_mut(issuer)
                    .c(d!())?
                    .get_mut(unit)
                    .unwrap() += amt;

                let mut tx = Transaction::from_seq_id(seq_id);

                let ar = AssetRecordTemplate::with_no_asset_tracing(
                    amt, code.val, iss_art, *pubkey,
                );
                let params = PublicParams::default();
                let (ba, _, owner_memo) = build_blind_asset_record(
                    &mut self.prng,
                    &params.pc_gens,
                    &ar,
                    vec![],
                );

                let asset_issuance_body = IssueAssetBody::new(
                    &code,
                    new_seq_num,
                    &[(
                        TxOutput {
                            id: None,
                            record: ba,
                            lien: None,
                        },
                        owner_memo.clone(),
                    )],
                )
                .c(d!())?;

                let asset_issuance_operation = IssueAsset::new(
                    asset_issuance_body,
                    &IssuerKeyPair { keypair: &keypair },
                )
                .c(d!())?;

                let issue_op = Operation::IssueAsset(asset_issuance_operation);

                tx.body.operations.push(issue_op);

                let txos = {
                    let txn_handle = self.client.submit_transaction(&tx).c(d!())?;
                    self.client.force_end_block().c(d!())?;
                    match self.client.txn_status(&txn_handle).c(d!())? {
                        TxnStatus::Committed((_sid, txos)) => txos,
                        _ => panic!("Pending status found when Committed expected"),
                    }
                };

                debug_assert!(txos.len() == 1);
                if let Some(memo) = owner_memo {
                    self.owner_memos.insert(txos[0], memo);
                }
                utxos.extend(txos.iter());
            }
            AccountsCommand::Send(src, amt, unit, dst) => {
                let amt = *amt as u64;
                let src_keypair = self.accounts.get(src).c(d!(inp_fail!()))?;
                let (src_pub, src_priv) =
                    (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
                let dst_keypair = self.accounts.get(dst).c(d!(inp_fail!()))?;
                let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
                let (_, unit_code) = self.units.get(unit).c(d!(inp_fail!()))?;

                if *self.balances.get(src).c(d!())?.get(unit).c(d!())? < amt {
                    return Err(eg!(inp_fail!()));
                }
                if amt == 0 {
                    return Ok(());
                }

                *self.balances.get_mut(src).c(d!())?.get_mut(unit).c(d!())? -= amt;
                *self.balances.get_mut(dst).c(d!())?.get_mut(unit).c(d!())? += amt;

                // Release the src lien, get a set of available records.

                let mut src_records: Vec<OpenAssetRecord> = Vec::new();
                let mut total_sum = 0u64;
                let avail = self.utxos.get_mut(src).c(d!())?;
                let mut to_use: Vec<TxoSID> = Vec::new();
                let mut to_skip: Vec<TxoSID> = Vec::new();
                while total_sum < amt && !avail.is_empty() {
                    let sid = avail.pop_front().c(d!())?;
                    let blind_rec = (self.client.get_utxo(sid).c(d!())?.utxo.0).record;
                    let memo = self.owner_memos.get(&sid).cloned();
                    let open_rec =
                        open_blind_asset_record(&blind_rec, &memo, &src_keypair)
                            .c(d!())?;
                    // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
                    if *open_rec.get_asset_type() != unit_code.val {
                        to_skip.push(sid);
                        continue;
                    }

                    debug_assert!(*open_rec.get_asset_type() == unit_code.val);
                    to_use.push(sid);
                    total_sum += *open_rec.get_amount();
                    src_records.push(open_rec);
                }
                // dbg!(&to_skip, &to_use);
                avail.extend(to_skip.into_iter());

                debug_assert!(total_sum >= amt);

                let mut src_outputs: Vec<AssetRecord> = Vec::new();
                let mut dst_outputs: Vec<AssetRecord> = Vec::new();
                let mut all_outputs: Vec<AssetRecord> = Vec::new();

                {
                    // Simple output to dst
                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        &mut self.prng,
                        &ar,
                    )
                    .c(d!())?;
                    dst_outputs.push(ar);

                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        amt,
                        unit_code.val,
                        art,
                        *dst_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        &mut self.prng,
                        &ar,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                if total_sum > amt {
                    // Extras left over go back to src
                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        &mut self.prng,
                        &ar,
                    )
                    .c(d!())?;
                    src_outputs.push(ar);

                    let ar = AssetRecordTemplate::with_no_asset_tracing(
                        total_sum - amt,
                        unit_code.val,
                        art,
                        *src_pub,
                    );
                    let ar = AssetRecord::from_template_no_identity_tracing(
                        &mut self.prng,
                        &ar,
                    )
                    .c(d!())?;
                    all_outputs.push(ar);
                }

                let src_outputs = src_outputs;
                let dst_outputs = dst_outputs;
                let all_outputs = all_outputs;
                debug_assert!(!src_records.is_empty());

                let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

                for _ in to_use.iter() {
                    sig_keys.push(
                        XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes())
                            .c(d!(PlatformError::ZeiError(None)))?,
                    );
                }

                let src_records: Vec<AssetRecord> = src_records
                    .iter()
                    .map(|oar| {
                        AssetRecord::from_open_asset_record_no_asset_tracing(oar.clone())
                    })
                    .collect();

                let transfer_body = TransferAssetBody::new(
                    &mut self.prng,
                    to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                    src_records.as_slice(),
                    all_outputs.as_slice(),
                    None,
                    vec![],
                    TransferType::Standard,
                )
                .c(d!())?;

                let mut owners_memos = transfer_body.transfer.owners_memos.clone();
                let transfer_sig =
                    transfer_body.compute_body_signature(&src_keypair, None);

                let transfer = TransferAsset {
                    body: transfer_body,
                    body_signatures: vec![transfer_sig],
                };

                let seq_id = self.fetch_seq_id();
                let mut prng = ChaChaRng::from_entropy();
                let txn = Transaction {
                    body: TransactionBody {
                        no_replay_token: NoReplayToken::new(&mut prng, seq_id),
                        operations: vec![Operation::TransferAsset(transfer)],
                        credentials: vec![],
                        memos: vec![],
                        policy_options: None,
                    },
                    signatures: vec![],
                };

                let txos = {
                    let txn_handle = self.client.submit_transaction(&txn).c(d!())?;
                    self.client.force_end_block().c(d!())?;
                    match self.client.txn_status(&txn_handle).c(d!())? {
                        TxnStatus::Committed((_sid, txos)) => txos,
                        _ => panic!("Pending status found when Committed expected"),
                    }
                };

                debug_assert!(txos.len() == src_outputs.len() + dst_outputs.len());

                self.utxos
                    .get_mut(dst)
                    .c(d!())?
                    .extend(&txos[..dst_outputs.len()]);
                self.utxos
                    .get_mut(src)
                    .c(d!())?
                    .extend(&txos[dst_outputs.len()..]);
                for (txo_sid, owner_memo) in txos.iter().zip(owners_memos.drain(..)) {
                    if let Some(memo) = owner_memo {
                        self.owner_memos.insert(*txo_sid, memo);
                    }
                }
            }
            AccountsCommand::ToggleConfAmts() => {
                self.confidential_amounts = !conf_amts;
            }
            AccountsCommand::ToggleConfTypes() => {
                self.confidential_types = !conf_types;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct AccountsScenario {
    // These only affect new issuances
    confidential_amounts: bool,
    confidential_types: bool,
    cmds: Vec<AccountsCommand>,
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
struct AccountsScenarioBuilder {
    confidential_amounts: bool,
    confidential_types: bool,
    cmds: Vec<AccountsCommand>,
    users: HashSet<UserName>,
    user_vec: Vec<UserName>,
    unit_amounts: HashMap<UnitName, usize>,
    unit_users: HashMap<UnitName, UserName>,
    unit_vec: Vec<UnitName>,
}

impl AccountsScenarioBuilder {
    pub fn new(confidential_amounts: bool, confidential_types: bool) -> Self {
        AccountsScenarioBuilder {
            confidential_amounts,
            confidential_types,
            ..Default::default()
        }
    }
    pub fn build(self) -> AccountsScenario {
        AccountsScenario {
            confidential_amounts: self.confidential_amounts,
            confidential_types: self.confidential_types,
            cmds: self.cmds,
        }
    }

    pub fn add_user(&mut self, name: UserName) {
        if self.users.contains(&name) {
            return;
        }
        self.users.insert(name.clone());
        self.user_vec.push(name.clone());
        self.cmds.push(AccountsCommand::NewUser(name));
    }

    pub fn add_unit(&mut self, name: UnitName, user_ix: usize, amt: usize) {
        if self.unit_amounts.contains_key(&name) {
            return;
        }
        self.unit_vec.push(name.clone());
        self.unit_amounts.insert(name.clone(), amt);

        let user = self.user_vec[user_ix % self.user_vec.len()].clone();
        self.unit_users.insert(name.clone(), user.clone());

        self.cmds.push(AccountsCommand::NewUnit(name.clone(), user));
        self.cmds.push(AccountsCommand::Mint(amt, name));
    }

    pub fn add_activity(
        &mut self,
        roll: u32,
        src: usize,
        count: usize,
        unit: usize,
        dst: usize,
    ) {
        let src = self.user_vec[src % self.user_vec.len()].clone();
        let dst = self.user_vec[dst % self.user_vec.len()].clone();
        let unit_ix = unit % self.unit_vec.len();
        let unit = self.unit_vec[unit_ix].clone();
        let amt = self.unit_amounts.get_mut(&unit).unwrap();
        let send_amt = if *amt / self.user_vec.len() > 0 {
            *amt / self.user_vec.len()
        } else {
            *amt
        };
        match roll % 23 {
            1..=19 => {
                let count = if send_amt != 0 { count % send_amt } else { 0 };
                self.cmds.push(AccountsCommand::Send(src, count, unit, dst));
            }
            0 => {
                *amt += count;
                self.cmds.push(AccountsCommand::Mint(count, unit.clone()));
                self.cmds.push(AccountsCommand::Send(
                    self.unit_users.get(&unit).unwrap().clone(),
                    count,
                    unit,
                    dst,
                ));
            }
            20 | 21 => {
                self.cmds.push(AccountsCommand::ToggleConfAmts());
            }
            22 => {
                self.cmds.push(AccountsCommand::ToggleConfTypes());
            }
            _ => panic!("A dice roll landed on an edge!"),
        }
    }
}

impl Arbitrary for AccountsScenario {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let mut b = AccountsScenarioBuilder::new(bool::arbitrary(g), bool::arbitrary(g));
        // A random AccountsScenario has...
        // Some users (at least one)...
        b.add_user(UserName::arbitrary(g));
        for extra_user in Vec::<UserName>::arbitrary(g) {
            b.add_user(extra_user);
        }

        // Some defined units-of-some-asset (at least one) issued by those
        // users...
        b.add_unit(
            UnitName::arbitrary(g),
            usize::arbitrary(g),
            usize::arbitrary(g),
        );
        for (name, ix1, ix2) in Vec::<(UnitName, usize, usize)>::arbitrary(g) {
            b.add_unit(name, ix1, ix2);
        }

        // And some activity.
        for (roll, src, count, unit, dst) in
            Vec::<(u32, usize, usize, usize, usize)>::arbitrary(g)
        {
            b.add_activity(roll, src, count, unit, dst);
        }

        b.build()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let ix = 0..(self.cmds.len());
        let base_cmds = self.cmds.clone();
        let conf_amounts = self.confidential_amounts;
        let conf_types = self.confidential_types;

        let quarters = if self.cmds.len() > 10 {
            Box::new((0..(4_usize)).rev()) as Box<dyn Iterator<Item = usize>>
        } else {
            Box::new(std::iter::empty()) as Box<dyn Iterator<Item = usize>>
        };

        Box::new({
            // try deleting a quarter of the commands
            let quartering = repeat((conf_amounts, conf_types))
                .zip(quarters)
                .zip(repeat(base_cmds.clone()))
                .map(|(((conf_amt, conf_type), i), base_cmds)| AccountsScenario {
                    confidential_amounts: conf_amt,
                    confidential_types: conf_type,
                    cmds: base_cmds[..((i * base_cmds.len()) / 4)]
                        .iter()
                        .chain(&base_cmds[(((i + 1) * base_cmds.len()) / 4)..])
                        .cloned()
                        .collect(),
                });

            // then try deleting each individual command from the sequence
            let splits = repeat((conf_amounts, conf_types))
                .zip(ix.clone().rev())
                .zip(repeat(base_cmds.clone()))
                .map(|(((conf_amt, conf_type), i), base_cmds)| AccountsScenario {
                    confidential_amounts: conf_amt,
                    confidential_types: conf_type,
                    cmds: base_cmds[..i]
                        .iter()
                        .chain(&base_cmds[(i + 1)..])
                        .cloned()
                        .collect(),
                });

            // then try shrinking each remaining command
            let shrink_inside = repeat((conf_amounts, conf_types))
                .zip(ix)
                .zip(repeat(base_cmds.clone()))
                .flat_map(|(((conf_amt, conf_type), i), base_cmds)| {
                    let old_cmd = base_cmds[i].clone();
                    repeat((conf_amt, conf_type))
                        .zip(base_cmds[i].shrink())
                        .zip(repeat((old_cmd, base_cmds, i)))
                        .map(|(((conf_amt, conf_type), x), (old_cmd, base_cmds, i))| {
                            AccountsScenario {
                                confidential_amounts: conf_amt,
                                confidential_types: conf_type,
                                cmds: base_cmds[..i]
                                    .iter()
                                    .chain(&vec![x.clone()])
                                    .cloned()
                                    .chain(
                                        (base_cmds[(i + 1)..]).iter().map(|cmd| {
                                            rename_by(&old_cmd, &x, cmd.clone())
                                        }),
                                    )
                                    .collect(),
                            }
                        })
                });
            // then try "shrinking" the confidentiality parameters
            let shrink_conf = repeat(conf_amounts)
                .zip(conf_types.shrink())
                .chain(conf_amounts.shrink().zip(repeat(conf_types)))
                .zip(repeat(base_cmds))
                .map(|((conf_amt, conf_type), base_cmds)| AccountsScenario {
                    confidential_amounts: conf_amt,
                    confidential_types: conf_type,
                    cmds: base_cmds,
                });

            quartering
                .chain(if self.cmds.len() < 5 {
                    Box::new(shrink_inside.chain(splits))
                        as Box<dyn Iterator<Item = AccountsScenario>>
                } else {
                    Box::new(splits.chain(shrink_inside))
                        as Box<dyn Iterator<Item = AccountsScenario>>
                })
                .chain(shrink_conf)
        })
    }
}

#[derive(StructOpt, Debug)]
#[structopt(
    about = "Procedurally generate account-style transactions",
    rename_all = "kebab-case"
)]
enum Actions {
    Generate {
        /// RNG seed
        #[structopt(short, long)]
        seed: Option<u64>,
        user_count: usize,
        unit_count: usize,
        activity_count: usize,
        #[structopt(short, long)]
        confidential: bool,
        /// Only output commands that succeed
        #[structopt(short, long)]
        filter_success: bool,
        /// Size of numbers generated
        #[structopt(short, long, default_value = "1000")]
        gen_size: usize,
        /// Output file for the AccountsScenario
        #[structopt(parse(from_os_str))]
        scenario_out: PathBuf,
        /// Output file for the transaction log
        #[structopt(parse(from_os_str))]
        log_output: PathBuf,
    },
    Replay {
        /// File containing the AccountsScenario
        #[structopt(parse(from_os_str))]
        scenario_file: PathBuf,
        /// Output file for the transaction log
        #[structopt(parse(from_os_str))]
        log_output: PathBuf,
    },
}

fn main() {
    let action = Actions::from_args();

    use Actions::*;
    let (scenario, filter_cmds, log_outfile) = match &action {
        Generate {
            seed,
            gen_size,
            user_count,
            unit_count,
            activity_count,
            confidential,
            scenario_out,
            log_output,
            filter_success,
        } => {
            let prng = seed
                .map(rand_chacha::ChaChaRng::seed_from_u64)
                .unwrap_or_else(rand_chacha::ChaChaRng::from_entropy);
            let mut gen = StdGen::new(prng, *gen_size);
            let g = &mut gen;

            let mut b = AccountsScenarioBuilder::new(*confidential, false);
            let user_count = if *user_count < 1 { 1 } else { *user_count };
            let unit_count = if *unit_count < 1 { 1 } else { *unit_count };

            for _ in 0..user_count {
                b.add_user(UserName::arbitrary(g));
            }

            for _ in 0..unit_count {
                b.add_unit(
                    UnitName::arbitrary(g),
                    usize::arbitrary(g),
                    usize::arbitrary(g),
                );
            }
            for _ in 0..*activity_count {
                let (roll, src, count, unit, dst) =
                    <(u32, usize, usize, usize, usize)>::arbitrary(g);
                b.add_activity(roll, src, count, unit, dst);
            }

            let ret = b.build();
            (ret, *filter_success, log_output.clone())
        }

        Replay {
            scenario_file,
            log_output,
        } => {
            let ret =
                serde_json::from_reader(std::fs::File::open(scenario_file).unwrap())
                    .unwrap();
            (ret, false, log_output.clone())
        }
    };

    let mut final_scenario = AccountsScenario {
        cmds: vec![],
        ..scenario
    };
    let scenario_outfile = if let Generate { scenario_out, .. } = &action {
        Some(std::fs::File::create(scenario_out).unwrap())
    } else {
        None
    };

    let mut ledger = Box::new(LedgerAccounts {
        ledger: LedgerState::test_ledger(),
        accounts: HashMap::new(),
        utxos: HashMap::new(),
        units: HashMap::new(),
        balances: HashMap::new(),
        owner_memos: HashMap::new(),
        confidential_amounts: scenario.confidential_amounts,
        confidential_types: scenario.confidential_types,
    });

    let txn_log_path = ledger.ledger.txn_log_path().unwrap();

    for cmd in scenario.cmds.iter() {
        let ledger_res = ledger.run_account_command(&cmd);

        if !filter_cmds || ledger_res.is_ok() {
            final_scenario.cmds.push(cmd.clone());
        }
    }

    std::fs::copy(txn_log_path, log_outfile).unwrap();

    if let Some(outfd) = scenario_outfile {
        serde_json::to_writer(outfd, &final_scenario).unwrap();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use lazy_static::lazy_static;

    use parking_lot::Mutex;

    lazy_static! {
        static ref LEDGER_STANDALONE_LOCK: Mutex<()> = Mutex::new(());
    }

    // #[quickcheck] tests that function with randomized input (then shrinks
    // the input if it fails).
    #[quickcheck]
    #[allow(non_snake_case)]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn SimpleAccounts_simplifies(cmds: AccountsScenario) {
        let cmds = cmds.cmds;
        let mut simple: SimpleAccountsState = Default::default();
        let mut normal: AccountsState = Default::default();

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();

        for cmd in cmds {
            simple.fast_invariant_check().unwrap();
            normal.fast_invariant_check().unwrap();

            let simpl_res = simple.run_account_command(&cmd);
            let zero_cmd = match cmd {
                AccountsCommand::Mint(_, unit) => {
                    //assert!(simpl_res.is_err());
                    AccountsCommand::Mint(0, unit)
                }
                AccountsCommand::Send(src, _, unit, dst) => {
                    //assert!(simpl_res.is_err());
                    AccountsCommand::Send(src, 0, unit, dst)
                }
                _ => cmd,
            };

            let normal_res = normal.run_account_command(&zero_cmd);
            match (simpl_res, normal_res) {
                (Ok(_), Ok(_)) => {}
                (Err(a), Err(b)) => {
                    assert!(a.eq_any(b.as_ref()));
                }
                _ => pnk!(Err(eg!())),
            }
        }

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();
    }

    #[quickcheck]
    #[allow(non_snake_case)]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn SimpleAccounts_simplifies_with_amounts(cmds: AccountsScenario) {
        let cmds = cmds.cmds;
        let mut prev_simple: SimpleAccountsState = Default::default();
        let mut prev_normal: AccountsState = Default::default();

        let mut simple: SimpleAccountsState = Default::default();
        let mut normal: AccountsState = Default::default();

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();

        for cmd in cmds {
            simple.fast_invariant_check().unwrap();
            normal.fast_invariant_check().unwrap();

            let simple_res = simple.run_account_command(&cmd);
            let normal_res = normal.run_account_command(&cmd);
            assert!(simple_res.is_ok() || normal_res.is_err());

            if simple_res.is_err() != normal_res.is_err() {
                simple = prev_simple.clone();
                normal = prev_normal.clone();
            } else if simple_res.is_ok() {
                prev_simple.run_account_command(&cmd).unwrap();
                prev_normal.run_account_command(&cmd).unwrap();
            }
        }

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();
    }

    fn ledger_simulates_accounts(
        cmds: AccountsScenario,
        with_standalone: bool,
        echo: bool,
    ) {
        if echo {
            println!(
                "conf amts: {}, conf types: {}",
                cmds.confidential_amounts, cmds.confidential_types
            );
            for c in cmds.cmds.iter() {
                println!("{:?}", c);
            }
        }

        let _x = if with_standalone {
            Some(LEDGER_STANDALONE_LOCK.lock())
        } else {
            None
        };

        let with_one_big_txn = cmds.cmds.len() < 20;

        let wait_time = time::Duration::from_millis(1000);
        let mut ledger = Box::new(LedgerAccounts {
            ledger: LedgerState::test_ledger(),
            accounts: HashMap::new(),
            utxos: HashMap::new(),
            units: HashMap::new(),
            balances: HashMap::new(),
            owner_memos: HashMap::new(),
            confidential_amounts: cmds.confidential_amounts,
            confidential_types: cmds.confidential_types,
        });
        let mut big_txn = Box::new(OneBigTxnAccounts {
            base_ledger: LedgerState::test_ledger(),
            txn: Transaction::from_seq_id(0), // Should be OK, starting with clean ledger
            txos: Default::default(),
            accounts: HashMap::new(),
            utxos: HashMap::new(),
            units: HashMap::new(),
            balances: HashMap::new(),
            confidential_amounts: cmds.confidential_amounts,
            confidential_types: cmds.confidential_types,
        });

        let mut lien = Box::new(LienAccounts {
            ledger: LedgerState::test_ledger(),
            owner_memos: Default::default(),
            accounts: HashMap::new(),
            balances: HashMap::new(),
            utxos: HashMap::new(),
            units: HashMap::new(),
            confidential_amounts: cmds.confidential_amounts,
            confidential_types: cmds.confidential_types,
        });

        let mut active_ledger = if !with_standalone {
            None
        } else {
            Some(Box::new(LedgerStandaloneAccounts {
                client: LedgerStandalone::new_mock(1),
                prng: rand_chacha::ChaChaRng::from_entropy(),
                accounts: HashMap::new(),
                utxos: HashMap::new(),
                units: HashMap::new(),
                balances: HashMap::new(),
                owner_memos: HashMap::new(),
                confidential_amounts: cmds.confidential_amounts,
                confidential_types: cmds.confidential_types,
            }))
        };

        if with_standalone {
            thread::sleep(wait_time);
        }

        let mut prev_simple: SimpleAccountsState = Default::default();
        let cmds = cmds.cmds;
        // dbg!(&cmds);

        let mut simple: SimpleAccountsState = Default::default();
        let mut normal: AccountsState = Default::default();

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();
        ledger.ledger.deep_invariant_check().unwrap();
        lien.ledger.deep_invariant_check().unwrap();

        for cmd in cmds {
            simple.fast_invariant_check().unwrap();
            normal.fast_invariant_check().unwrap();

            let simple_res = simple.run_account_command(&cmd);
            let normal_res = normal.run_account_command(&cmd);
            assert!(simple_res.is_ok() || normal_res.is_err());
            let ledger_res = ledger.run_account_command(&cmd);
            assert!(ledger_res.is_ok() == normal_res.is_ok());
            let lien_res = lien.run_account_command(&cmd);
            assert!(lien_res.is_ok() == normal_res.is_ok());
            let big_txn_res = if with_one_big_txn {
                let res = big_txn.run_account_command(&cmd);
                assert!(res.is_ok() == normal_res.is_ok());
                Some(res)
            } else {
                None
            };
            if with_standalone {
                let active_ledger_res =
                    active_ledger.as_mut().unwrap().run_account_command(&cmd);
                // dbg!(&active_ledger_res);
                assert!(active_ledger_res.is_ok() == normal_res.is_ok());
            }

            if simple_res.is_err() != normal_res.is_err() {
                assert!(simple_res.is_ok());
                simple = prev_simple.clone();
            } else if simple_res.is_ok() {
                prev_simple.run_account_command(&cmd).unwrap();
                ledger_res.unwrap();
                lien_res.unwrap();
                if with_one_big_txn {
                    big_txn_res.unwrap().unwrap();
                }
            }
        }

        simple.deep_invariant_check().unwrap();
        normal.deep_invariant_check().unwrap();
        ledger.ledger.deep_invariant_check().unwrap();
    }

    #[allow(dead_code)]
    fn ledger_simulates_accounts_with_standalone(cmds: AccountsScenario) {
        println!("{} commands...", cmds.cmds.len());
        ledger_simulates_accounts(cmds, true, true)
    }

    #[allow(dead_code)]
    fn ledger_simulates_accounts_no_standalone(cmds: AccountsScenario) {
        ledger_simulates_accounts(cmds, false, false)
    }

    fn regression_quickcheck_found(
        with_standalone: bool,
        conf_amts: bool,
        conf_types: bool,
    ) {
        use AccountsCommand::*;
        ledger_simulates_accounts(
            AccountsScenario {
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Send(
                        UserName("".into()),
                        0,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
                confidential_types: conf_types,
                confidential_amounts: conf_amts,
            },
            with_standalone,
            true,
        );
        ledger_simulates_accounts(
            AccountsScenario {
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Mint(1, UnitName("".into())),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
                confidential_types: conf_types,
                confidential_amounts: conf_amts,
            },
            with_standalone,
            true,
        );

        ledger_simulates_accounts(
            AccountsScenario {
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Mint(1, UnitName("".into())),
                    // ToggleConfTypes(),
                    Mint(1, UnitName("".into())),
                    Send(
                        UserName("".into()),
                        2,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
                confidential_types: conf_types,
                confidential_amounts: conf_amts,
            },
            with_standalone,
            true,
        );

        ledger_simulates_accounts(
            AccountsScenario {
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Mint(1, UnitName("".into())),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
                confidential_types: conf_types,
                confidential_amounts: conf_amts,
            },
            with_standalone,
            true,
        );
        ledger_simulates_accounts(
            AccountsScenario {
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Mint(1, UnitName("".into())),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
                confidential_types: conf_types,
                confidential_amounts: conf_amts,
            },
            with_standalone,
            true,
        );

        ledger_simulates_accounts(
            AccountsScenario {
                confidential_amounts: conf_types,
                confidential_types: conf_amts,
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUser(UserName("\u{0}".into())),
                    NewUnit(UnitName("".into()), UserName("\u{0}".into())),
                    Mint(34, UnitName("".into())),
                    Send(
                        UserName("\u{0}".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
            },
            with_standalone,
            true,
        );

        ledger_simulates_accounts(
            AccountsScenario {
                confidential_amounts: conf_types,
                confidential_types: conf_amts,
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUser(UserName("\u{0}".into())),
                    NewUnit(UnitName("".into()), UserName("\u{0}".into())),
                    Mint(34, UnitName("".into())),
                    Send(
                        UserName("\u{0}".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
            },
            with_standalone,
            true,
        );

        ledger_simulates_accounts(
            AccountsScenario {
                confidential_amounts: conf_types,
                confidential_types: conf_amts,
                cmds: vec![
                    NewUser(UserName("".into())),
                    NewUnit(UnitName("".into()), UserName("".into())),
                    Mint(32, UnitName("".into())),
                    Send(
                        UserName("".into()),
                        1,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                    Send(
                        UserName("".into()),
                        2,
                        UnitName("".into()),
                        UserName("".into()),
                    ),
                ],
            },
            with_standalone,
            true,
        );
    }

    #[test]
    fn regression_quickcheck_found_no_standalone00() {
        regression_quickcheck_found(false, false, false)
    }

    #[test]
    fn regression_quickcheck_found_no_standalone01() {
        regression_quickcheck_found(false, false, true)
    }

    #[test]
    fn regression_quickcheck_found_no_standalone11() {
        regression_quickcheck_found(false, true, true)
    }

    #[test]
    fn regression_quickcheck_found_no_standalone10() {
        regression_quickcheck_found(false, true, false)
    }

    #[test]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn regression_quickcheck_found_with_standalone00() {
        regression_quickcheck_found(true, false, false)
    }

    #[test]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn regression_quickcheck_found_with_standalone01() {
        regression_quickcheck_found(true, false, true)
    }

    #[test]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn regression_quickcheck_found_with_standalone11() {
        regression_quickcheck_found(true, true, true)
    }

    #[test]
    // This test passes, but we ignore it since it's slow
    // Redmine issue: #47
    #[ignore]
    fn regression_quickcheck_found_with_standalone10() {
        regression_quickcheck_found(true, true, false)
    }

    #[test]
    #[ignore]
    // (brian) Ignoring this because I see
    // ---- test::quickcheck_ledger_simulates stdout ----
    // thread 'test::quickcheck_ledger_simulates' panicked at 'read_response_json failed during deserialization', ...

    fn quickcheck_ledger_simulates() {
        QuickCheck::new().tests(1).quickcheck(
            ledger_simulates_accounts_with_standalone
                     //.quickcheck(ledger_simulates_accounts_no_standalone
                                 as fn(AccountsScenario) -> (),
        );
    }
}
