#![deny(warnings)]
#![allow(unused)]
use quickcheck::{Arbitrary, Gen, QuickCheck, StdGen};
use std::iter::repeat;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::error_location;
use ledger::store::*;
use ledger_api_service::RestfulLedgerAccess;
use network::LedgerStandalone;
use rand_chacha::ChaChaRng;
use rand_core::{RngCore, SeedableRng};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
#[cfg(test)]
use std::ffi::OsString;
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
use utils::HasInvariants;
use zei::api::anon_creds::ACCommitment;
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{
  AssetRecord, AssetRecordTemplate, AssetTracingPolicy, OpenAssetRecord, OwnerMemo,
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
    NewUnit(unit, name) => NewUnit(unit, if name == *old { new.clone() } else { name }),
    Send(src, count, unit, dest) => Send(if src == *old { new.clone() } else { src },
                                         count,
                                         unit,
                                         if dest == *old { new.clone() } else { dest }),
    _ => ac,
  }
}

fn rename_unit(old: &UnitName, new: &UnitName, ac: AccountsCommand) -> AccountsCommand {
  use AccountsCommand::*;
  match ac {
    Mint(count, unit) => Mint(count, if unit == *old { new.clone() } else { unit }),
    Send(src, count, unit, dest) => Send(src,
                                         count,
                                         if unit == *old { new.clone() } else { unit },
                                         dest),
    // Ignore NewUnit(..), since re-declaring a unit should be invalid.
    _ => ac,
  }
}

fn rename_by(old: &AccountsCommand, new: &AccountsCommand, ac: AccountsCommand) -> AccountsCommand {
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
      1 => AccountsCommand::NewUnit(UnitName::arbitrary(g), UserName::arbitrary(g)),
      2..=4 => AccountsCommand::Mint(usize::arbitrary(g), UnitName::arbitrary(g)),
      5..=9 => AccountsCommand::Send(UserName::arbitrary(g),
                                     usize::arbitrary(g),
                                     UnitName::arbitrary(g),
                                     UserName::arbitrary(g)),
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
        Box::new(// shrink name
                 name.shrink()
                     .zip(repeat(issuer.clone()))
                     // shrink issuer
                     .chain(repeat(name.clone()).zip(issuer.shrink()))
                     .map(|(n, i)| NewUnit(n, i)))
      }
      Mint(amt, unit) => {
        Box::new(// shrink amt
                 amt.shrink()
                    .zip(repeat(unit.clone()))
                    // shrink unit
                    .chain(repeat(*amt).zip(unit.shrink()))
                    .map(|(a, u)| Mint(a, u)))
      }
      Send(src, amt, unit, dst) => {
        Box::new(// shrink src
                 src.shrink()
                    .zip(repeat(*amt))
                    .zip(repeat(unit.clone()).zip(repeat(dst.clone())))
                    .chain(// shrink amt
                           repeat(src.clone()).zip(amt.shrink())
                                              .zip(repeat(unit.clone()).zip(repeat(dst.clone()))))
                    .chain(// shrink unit
                           repeat(src.clone()).zip(repeat(*amt))
                                              .zip(unit.shrink().zip(repeat(dst.clone()))))
                    .chain(// shrink dst
                           repeat(src.clone()).zip(repeat(*amt))
                                              .zip(repeat(unit.clone()).zip(dst.shrink())))
                    .map(|((src, amt), (unit, dst))| Send(src, amt, unit, dst)))
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

impl HasInvariants<()> for SimpleAccountsState {
  fn fast_invariant_check(&self) -> Result<(), ()> {
    if self.accounts.len() != self.users_to_units.len() {
      return Err(());
    }

    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), ()> {
    self.fast_invariant_check()?;

    for (name, _) in self.accounts.iter() {
      for unit in self.users_to_units.get(name).ok_or(())?.iter() {
        if name != self.units_to_users.get(unit).ok_or(())? {
          return Err(());
        }
      }

      for (unit, _) in self.units_to_users.iter() {
        let orig_user = self.units_to_users.get(unit).ok_or(())?;
        if !self.users_to_units.get(orig_user).ok_or(())?.contains(unit) {
          return Err(());
        }

        self.accounts.get(orig_user).ok_or(())?;
      }
    }

    Ok(())
  }
}

impl HasInvariants<()> for AccountsState {
  fn fast_invariant_check(&self) -> Result<(), ()> {
    if self.accounts.len() != self.users_to_units.len() {
      return Err(());
    }

    Ok(())
  }

  fn deep_invariant_check(&self) -> Result<(), ()> {
    self.fast_invariant_check()?;

    SimpleAccountsState { accounts: self.accounts
                                        .iter()
                                        .map(|(acc, _)| (acc.clone(), ()))
                                        .collect(),
                          units_to_users: self.units_to_users.clone(),
                          users_to_units: self.users_to_units.clone() }.deep_invariant_check()?;

    for (_, balances) in self.accounts.iter() {
      for (unit, _) in self.units_to_users.iter() {
        balances.get(unit).ok_or(())?;
      }
    }

    Ok(())
  }
}

trait InterpretAccounts<ErrT> {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), ErrT>;
}

impl InterpretAccounts<()> for SimpleAccountsState {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), ()> {
    match cmd {
      AccountsCommand::NewUser(name) => {
        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;
        self.users_to_units
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;

        self.accounts.insert(name.clone(), ());
        self.users_to_units.insert(name.clone(), HashSet::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        self.accounts.get(issuer).ok_or(())?;
        let unit_set = self.users_to_units.get_mut(issuer).ok_or(())?;
        self.units_to_users
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;

        unit_set.insert(name.clone());
        self.units_to_users.insert(name.clone(), issuer.clone());
      }
      AccountsCommand::Mint(_, unit) => {
        self.accounts
            .get(self.units_to_users.get(unit).ok_or(())?)
            .ok_or(())?;
      }
      AccountsCommand::Send(src, _, unit, dst) => {
        self.accounts.get(src).ok_or(())?;
        self.accounts.get(dst).ok_or(())?;
        self.units_to_users.get(unit).ok_or(())?;
      }
      AccountsCommand::ToggleConfAmts() => {}
      AccountsCommand::ToggleConfTypes() => {}
    }
    Ok(())
  }
}

impl InterpretAccounts<()> for AccountsState {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), ()> {
    match cmd {
      AccountsCommand::NewUser(name) => {
        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;
        self.users_to_units
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;

        self.accounts.insert(name.clone(),
                             self.units_to_users
                                 .iter()
                                 .map(|(unit, _)| (unit.clone(), 0))
                                 .collect());
        self.users_to_units.insert(name.clone(), HashSet::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        self.accounts.get(issuer).ok_or(())?;
        let unit_set = self.users_to_units.get_mut(issuer).ok_or(())?;
        self.units_to_users
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(()))?;

        unit_set.insert(name.clone());
        self.units_to_users.insert(name.clone(), issuer.clone());

        for (_, acc) in self.accounts.iter_mut() {
          acc.insert(name.clone(), 0);
        }
      }
      AccountsCommand::Mint(amt, unit) => {
        let acct = self.accounts
                       .get_mut(self.units_to_users.get(unit).ok_or(())?)
                       .ok_or(())?;
        *acct.get_mut(unit).ok_or(())? += amt;
      }
      AccountsCommand::Send(src, amt, unit, dst) => {
        {
          let dst_acct = self.accounts.get(dst).ok_or(())?;
          dst_acct.get(unit).ok_or(())?;
        }
        {
          let src_acct = self.accounts.get_mut(src).ok_or(())?;
          let src_column = src_acct.get_mut(unit).ok_or(())?;
          if *src_column < *amt {
            return Err(());
          }
          *src_column -= amt;
        }
        {
          let dst_acct = self.accounts.get_mut(dst).unwrap();
          let dst_column = dst_acct.get_mut(unit).unwrap();

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
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = self.confidential_types;
    let iss_art = AssetRecordType::from_booleans(conf_amts, false);
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    // dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(self.ledger.get_prng());

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError(error_location!())))?;

        // dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units.get(name).map_or_else(|| Ok(()),
                                          |_| Err(PlatformError::InputsError(error_location!())))?;

        let code = AssetTypeCode::gen_random();

        // dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        let txn = Transaction { body: TransactionBody { operations:
                                                          vec![Operation::DefineAsset(op)],
                                                        credentials: vec![],
                                                        memos: vec![],
                                                        policy_options: None },
                                signatures: vec![] };

        let eff = TxnEffect::compute_effect(txn).unwrap();

        {
          let mut block = self.ledger.start_block()?;
          if let Err(e) = self.ledger.apply_transaction(&mut block, eff) {
            self.ledger.abort_block(block);
            return Err(e);
          }
          self.ledger.finish_block(block).unwrap();
        }

        self.units.insert(name.clone(), (issuer.clone(), code));

        for (_, bal) in self.balances.iter_mut() {
          bal.insert(name.clone(), 0);
        }
      }
      AccountsCommand::Mint(amt, unit) => {
        let amt = *amt as u64;
        let (issuer, code) = self.units
                                 .get(unit)
                                 .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

        let new_seq_num = self.ledger.get_issuance_num(&code).unwrap();

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let mut tx = Transaction::default();

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, iss_art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(self.ledger.get_prng(), &params.pc_gens, &ar, None);

        let asset_issuance_body =
          IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)], None).unwrap();

        let asset_issuance_operation =
          IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        tx.body.operations.push(issue_op);
        let effect = TxnEffect::compute_effect(tx).unwrap();

        let mut block = self.ledger.start_block().unwrap();
        let temp_sid = self.ledger.apply_transaction(&mut block, effect).unwrap();

        let (_, txos) = self.ledger
                            .finish_block(block)
                            .unwrap()
                            .remove(&temp_sid)
                            .unwrap();

        assert!(txos.len() == 1);
        if let Some(memo) = owner_memo {
          self.owner_memos.insert(txos[0], memo);
        }
        utxos.extend(txos.iter());
      }
      AccountsCommand::Send(src, amt, unit, dst) => {
        let amt = *amt as u64;
        let src_keypair = self.accounts
                              .get(src)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts
                              .get(dst)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code) = self.units
                                 .get(unit)
                                 .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError(error_location!()));
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut input_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<TxoSID> = Vec::new();
        let mut to_skip: Vec<TxoSID> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = &(self.ledger.get_utxo(sid).unwrap().0).0;
          let memo = self.owner_memos.get(&sid).cloned();
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
          input_identity_commitments.push(None);
        }
        // dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();
        let mut output_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();
        {
          // Simple output to dst
          let template =
            AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.ledger.get_prng(),
                                                                   &template).unwrap();
          dst_outputs.push(ar);

          let template =
            AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.ledger.get_prng(),
                                                                   &template).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        if total_sum > amt {
          // Extras left over go back to src
          let template = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                                     unit_code.val,
                                                                     art,
                                                                     *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.ledger.get_prng(),
                                                                   &template).unwrap();
          src_outputs.push(ar);

          let template = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                                     unit_code.val,
                                                                     art,
                                                                     *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.ledger.get_prng(),
                                                                   &template).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        // dbg!(unit_code.val);
        // for (ix, rec) in src_records.iter().enumerate() {
        // dbg!(ix,
        //      rec.get_asset_type(),
        //      rec.get_amount(),
        //      rec.get_pub_key());
        // }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let src_records: Vec<AssetRecord> =
          src_records.iter()
                     .map(|oar| AssetRecord::from_open_asset_record_no_asset_tracking(oar.clone()))
                     .collect();

        let transfer_body =
          TransferAssetBody::new(self.ledger.get_prng(),
                                 to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                                 src_records.as_slice(),
                                 input_identity_commitments,
                                 all_outputs.as_slice(),
                                 output_identity_commitments,
                                 TransferType::Standard).unwrap();

        let mut owners_memos = transfer_body.transfer.owners_memos.clone();
        // dbg!(&transfer_body);
        let transfer_sig = transfer_body.compute_body_signature(&src_keypair, None);

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig] };
        let txn = Transaction { body: TransactionBody { operations:
                                                          vec![Operation::TransferAsset(transfer)],
                                                        credentials: vec![],
                                                        memos: vec![],
                                                        policy_options: None },
                                signatures: vec![] };

        let effect = TxnEffect::compute_effect(txn).unwrap();

        let mut block = self.ledger.start_block().unwrap();
        let temp_sid = self.ledger.apply_transaction(&mut block, effect).unwrap();

        let (_, txos) = self.ledger
                            .finish_block(block)
                            .unwrap()
                            .remove(&temp_sid)
                            .unwrap();

        assert!(txos.len() == src_outputs.len() + dst_outputs.len());

        self.utxos
            .get_mut(dst)
            .unwrap()
            .extend(&txos[..dst_outputs.len()]);
        self.utxos
            .get_mut(src)
            .unwrap()
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
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = self.confidential_types;
    let iss_art = AssetRecordType::from_booleans(conf_amts, false);
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    // dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(self.base_ledger.get_prng());

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError(error_location!())))?;

        // dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units.get(name).map_or_else(|| Ok(()),
                                          |_| Err(PlatformError::InputsError(error_location!())))?;

        let code = AssetTypeCode::gen_random();

        // dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        self.txn.body.operations.push(Operation::DefineAsset(op));
        let eff = TxnEffect::compute_effect(self.txn.clone()).unwrap();
        let eff = self.base_ledger
                      .TESTING_get_status()
                      .TESTING_check_txn_effects(eff)
                      .unwrap();

        self.txn = eff.txn;

        self.units.insert(name.clone(), (issuer.clone(), code, 0));

        for (_, bal) in self.balances.iter_mut() {
          bal.insert(name.clone(), 0);
        }
      }
      AccountsCommand::Mint(amt, unit) => {
        let amt = *amt as u64;
        let (issuer, code, new_seq_num) =
          self.units
              .get_mut(unit)
              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        *new_seq_num += 1;
        let new_seq_num = *new_seq_num - 1;

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, iss_art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(self.base_ledger.get_prng(), &params.pc_gens, &ar, None);

        let asset_issuance_body =
          IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)], None).unwrap();

        let asset_issuance_operation =
          IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        self.txn.body.operations.push(issue_op);
        let effect = TxnEffect::compute_effect(self.txn.clone()).unwrap();
        let effect = self.base_ledger
                         .TESTING_get_status()
                         .TESTING_check_txn_effects(effect)
                         .unwrap();
        self.txn = effect.txn;

        assert!(effect.txos.last().unwrap().is_some());
        assert!(effect.txos.len() == self.txos.len() + 1);
        utxos.push_back(effect.txos.len() - 1);

        self.txos
            .push((effect.txos[self.txos.len()].as_ref().unwrap().clone(), owner_memo));
      }
      AccountsCommand::Send(src, amt, unit, dst) => {
        let amt = *amt as u64;
        let src_keypair = self.accounts
                              .get(src)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts
                              .get(dst)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code, _) = self.units
                                    .get(unit)
                                    .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError(error_location!()));
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut input_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<usize> = Vec::new();
        let mut to_skip: Vec<usize> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = &((self.txos.get(sid).unwrap().0).0);
          let memo = &(self.txos.get(sid).unwrap().1);
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(self.txos.len() - 1 - sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
          input_identity_commitments.push(None);
        }
        // dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();
        let mut output_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();

        {
          // Simple output to dst
          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.base_ledger.get_prng(),
                                                                   &ar).unwrap();
          dst_outputs.push(ar);

          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.base_ledger.get_prng(),
                                                                   &ar).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        if total_sum > amt {
          // Extras left over go back to src
          let ar = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                               unit_code.val,
                                                               art,
                                                               *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.base_ledger.get_prng(),
                                                                   &ar).unwrap();
          src_outputs.push(ar);

          let ar = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                               unit_code.val,
                                                               art,
                                                               *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(self.base_ledger.get_prng(),
                                                                   &ar).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        // dbg!(unit_code.val);
        // for (ix, rec) in src_records.iter().enumerate() {
        // dbg!(ix,
        //      rec.get_asset_type(),
        //      rec.get_amount(),
        //      rec.get_pub_key());
        // }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let src_records: Vec<AssetRecord> =
          src_records.iter()
                     .map(|oar| AssetRecord::from_open_asset_record_no_asset_tracking(oar.clone()))
                     .collect();

        let transfer_body = TransferAssetBody::new(self.base_ledger.get_prng(),
                                                   to_use.iter()
                                                         .cloned()
                                                         .map(|x| TxoRef::Relative(x as u64))
                                                         .collect(),
                                                   src_records.as_slice(),
                                                   input_identity_commitments,
                                                   all_outputs.as_slice(),
                                                   output_identity_commitments,
                                                   TransferType::Standard).unwrap();
        let owners_memos = transfer_body.transfer.owners_memos.clone();
        // dbg!(&transfer_body);
        let transfer_sig = transfer_body.compute_body_signature(&src_keypair, None);

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig] };

        self.txn
            .body
            .operations
            .push(Operation::TransferAsset(transfer));

        let effect = TxnEffect::compute_effect(self.txn.clone()).unwrap();
        let effect = self.base_ledger
                         .TESTING_get_status()
                         .TESTING_check_txn_effects(effect)
                         .unwrap();
        self.txn = effect.txn;

        let txos = effect.txos[self.txos.len()..].iter()
                                                 .cloned()
                                                 .map(|x| x.unwrap())
                                                 .collect::<Vec<TxOutput>>();
        assert!(txos.len() == src_outputs.len() + dst_outputs.len());

        let txo_sids = (0..txos.len()).map(|x| x + self.txos.len())
                                      .collect::<Vec<usize>>();

        self.utxos
            .get_mut(dst)
            .unwrap()
            .extend(&txo_sids[..dst_outputs.len()]);
        self.utxos
            .get_mut(src)
            .unwrap()
            .extend(&txo_sids[dst_outputs.len()..]);
        self.txos.extend(txos.iter()
                             .zip(owners_memos.iter())
                             .map(|(txo, memo)| (txo.clone(), memo.as_ref().cloned()))
                             .collect::<Vec<(TxOutput, Option<OwnerMemo>)>>());
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
  where T: RestfulLedgerUpdate + RestfulLedgerAccess
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

impl<T> InterpretAccounts<PlatformError> for LedgerStandaloneAccounts<T>
  where T: RestfulLedgerAccess + RestfulLedgerUpdate
{
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = self.confidential_types;
    let iss_art = AssetRecordType::from_booleans(conf_amts, false);
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    // dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(&mut self.prng);

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError(error_location!())))?;

        // dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units.get(name).map_or_else(|| Ok(()),
                                          |_| Err(PlatformError::InputsError(error_location!())))?;

        let code = AssetTypeCode::gen_random();

        // dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        let txn = Transaction { body: TransactionBody { operations:
                                                          vec![Operation::DefineAsset(op)],
                                                        credentials: vec![],
                                                        memos: vec![],
                                                        policy_options: None },
                                signatures: vec![] };

        {
          let txn_handle = self.client.submit_transaction(&txn).unwrap();
          self.client.force_end_block().unwrap();
          match self.client.txn_status(&txn_handle).unwrap() {
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
        let amt = *amt as u64;
        let (issuer, code) = self.units
                                 .get(unit)
                                 .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

        let new_seq_num = self.client.get_issuance_num(&code).unwrap();

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let mut tx = Transaction::default();

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, iss_art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(&mut self.prng, &params.pc_gens, &ar, None);

        let asset_issuance_body =
          IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)], None).unwrap();

        let asset_issuance_operation =
          IssueAsset::new(asset_issuance_body, &IssuerKeyPair { keypair: &keypair }).unwrap();

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        tx.body.operations.push(issue_op);

        let txos = {
          let txn_handle = self.client.submit_transaction(&tx).unwrap();
          self.client.force_end_block().unwrap();
          match self.client.txn_status(&txn_handle).unwrap() {
            TxnStatus::Committed((_sid, txos)) => txos,
            _ => panic!("Pending status found when Committed expected"),
          }
        };

        assert!(txos.len() == 1);
        if let Some(memo) = owner_memo {
          self.owner_memos.insert(txos[0], memo);
        }
        utxos.extend(txos.iter());
      }
      AccountsCommand::Send(src, amt, unit, dst) => {
        let amt = *amt as u64;
        let src_keypair = self.accounts
                              .get(src)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts
                              .get(dst)
                              .ok_or_else(|| PlatformError::InputsError(error_location!()))?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code) = self.units
                                 .get(unit)
                                 .ok_or_else(|| PlatformError::InputsError(error_location!()))?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError(error_location!()));
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut input_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<TxoSID> = Vec::new();
        let mut to_skip: Vec<TxoSID> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = (self.client.get_utxo(sid).unwrap().0).0;
          let memo = self.owner_memos.get(&sid).cloned();
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          // dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
          input_identity_commitments.push(None);
        }
        // dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();
        let mut output_identity_commitments: Vec<Option<ACCommitment>> = Vec::new();

        {
          // Simple output to dst
          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          dst_outputs.push(ar);

          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        if total_sum > amt {
          // Extras left over go back to src
          let ar = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                               unit_code.val,
                                                               art,
                                                               *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          src_outputs.push(ar);

          let ar = AssetRecordTemplate::with_no_asset_tracking(total_sum - amt,
                                                               unit_code.val,
                                                               art,
                                                               *src_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          all_outputs.push(ar);
          output_identity_commitments.push(None);
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        // dbg!(unit_code.val);
        // for (ix, rec) in src_records.iter().enumerate() {
        // dbg!(ix,
        //      rec.get_asset_type(),
        //      rec.get_amount(),
        //      rec.get_pub_key());
        // }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let src_records: Vec<AssetRecord> =
          src_records.iter()
                     .map(|oar| AssetRecord::from_open_asset_record_no_asset_tracking(oar.clone()))
                     .collect();

        let transfer_body =
          TransferAssetBody::new(&mut self.prng,
                                 to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                                 src_records.as_slice(),
                                 input_identity_commitments,
                                 all_outputs.as_slice(),
                                 output_identity_commitments,
                                 TransferType::Standard).unwrap();

        let mut owners_memos = transfer_body.transfer.owners_memos.clone();
        // dbg!(&transfer_body);
        let transfer_sig = transfer_body.compute_body_signature(&src_keypair, None);

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig] };
        let txn = Transaction { body: TransactionBody { operations:
                                                          vec![Operation::TransferAsset(transfer)],
                                                        credentials: vec![],
                                                        memos: vec![],
                                                        policy_options: None },
                                signatures: vec![] };

        let txos = {
          let txn_handle = self.client.submit_transaction(&txn).unwrap();
          self.client.force_end_block().unwrap();
          match self.client.txn_status(&txn_handle).unwrap() {
            TxnStatus::Committed((_sid, txos)) => txos,
            _ => panic!("Pending status found when Committed expected"),
          }
        };

        assert!(txos.len() == src_outputs.len() + dst_outputs.len());

        self.utxos
            .get_mut(dst)
            .unwrap()
            .extend(&txos[..dst_outputs.len()]);
        self.utxos
            .get_mut(src)
            .unwrap()
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
    AccountsScenarioBuilder { confidential_amounts,
                              confidential_types,
                              ..Default::default() }
  }
  pub fn build(self) -> AccountsScenario {
    AccountsScenario { confidential_amounts: self.confidential_amounts,
                       confidential_types: self.confidential_types,
                       cmds: self.cmds }
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

  pub fn add_activity(&mut self, roll: u32, src: usize, count: usize, unit: usize, dst: usize) {
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
        self.cmds
            .push(AccountsCommand::Send(self.unit_users.get(&unit).unwrap().clone(),
                                        count,
                                        unit,
                                        dst));
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
    b.add_unit(UnitName::arbitrary(g),
               usize::arbitrary(g),
               usize::arbitrary(g));
    for (name, ix1, ix2) in Vec::<(UnitName, usize, usize)>::arbitrary(g) {
      b.add_unit(name, ix1, ix2);
    }

    // And some activity.
    for (roll, src, count, unit, dst) in Vec::<(u32, usize, usize, usize, usize)>::arbitrary(g) {
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
      Box::new((0..(4 as usize)).rev()) as Box<dyn Iterator<Item = usize>>
    } else {
      Box::new(std::iter::empty()) as Box<dyn Iterator<Item = usize>>
    };

    Box::new({
      // try deleting a quarter of the commands
      let quartering = repeat((conf_amounts,conf_types)).zip(quarters)
        .zip(repeat(base_cmds.clone()))
        .map(|(((conf_amt,conf_type),i),base_cmds)| AccountsScenario {
          confidential_amounts: conf_amt,
          confidential_types: conf_type,
          cmds: base_cmds[..((i*base_cmds.len())/4)].iter().chain(&base_cmds[(((i+1)*base_cmds.len())/4)..]).cloned().collect()
        });

      // then try deleting each individual command from the sequence
      let splits =
        repeat((conf_amounts, conf_types)).zip(ix.clone().rev())
                                          .zip(repeat(base_cmds.clone()))
                                          .map(|(((conf_amt, conf_type), i), base_cmds)| {
                                            AccountsScenario { confidential_amounts: conf_amt,
                                                               confidential_types: conf_type,
                                                               cmds:
                                                                 base_cmds[..i].iter()
                                                                               .chain(&base_cmds
                                                                                        [(i + 1)..])
                                                                               .cloned()
                                                                               .collect() }
                                          });

      // then try shrinking each remaining command
      let shrink_inside = repeat((conf_amounts,conf_types)).zip(ix)
        .zip(repeat(base_cmds.clone())).flat_map(
          |(((conf_amt,conf_type),i),base_cmds)|
          {
            let old_cmd = base_cmds[i].clone();
            repeat((conf_amt,conf_type)).zip(base_cmds[i].shrink())
              .zip(repeat((old_cmd,base_cmds,i))).map(|(((conf_amt,conf_type),x),(old_cmd,base_cmds,i))|
                AccountsScenario {
                  confidential_amounts: conf_amt,
                  confidential_types: conf_type,
                  cmds: base_cmds[..i].iter().chain(&vec![x.clone()]).cloned()
                    .chain((base_cmds[(i+1)..]).iter()
                      .map(|cmd| rename_by(&old_cmd,&x,cmd.clone())))
                    .collect()
                }
            )
          }
      );
      // then try "shrinking" the confidentiality parameters
      let shrink_conf = repeat(conf_amounts).zip(conf_types.shrink())
                                            .chain(conf_amounts.shrink().zip(repeat(conf_types)))
                                            .zip(repeat(base_cmds))
                                            .map(|((conf_amt, conf_type), base_cmds)| {
                                              AccountsScenario { confidential_amounts: conf_amt,
                                                                 confidential_types: conf_type,
                                                                 cmds: base_cmds }
                                            });

      quartering.chain(if self.cmds.len() < 5 {
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
#[structopt(about = "Procedurally generate account-style transactions",
            rename_all = "kebab-case")]
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
    Generate { seed,
               gen_size,
               user_count,
               unit_count,
               activity_count,
               confidential,
               scenario_out,
               log_output,
               filter_success, } => {
      let prng = seed.map(rand_chacha::ChaChaRng::seed_from_u64)
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
        b.add_unit(UnitName::arbitrary(g),
                   usize::arbitrary(g),
                   usize::arbitrary(g));
      }
      for _ in 0..*activity_count {
        let (roll, src, count, unit, dst) = <(u32, usize, usize, usize, usize)>::arbitrary(g);
        b.add_activity(roll, src, count, unit, dst);
      }

      let ret = b.build();
      (ret, *filter_success, log_output.clone())
    }

    Replay { scenario_file,
             log_output, } => {
      let ret = serde_json::from_reader(std::fs::File::open(scenario_file).unwrap()).unwrap();
      (ret, false, log_output.clone())
    }
  };

  let mut final_scenario = AccountsScenario { cmds: vec![],
                                              ..scenario };
  let scenario_outfile = if let Generate { scenario_out, .. } = &action {
    Some(std::fs::File::create(scenario_out).unwrap())
  } else {
    None
  };

  let mut ledger = Box::new(LedgerAccounts { ledger: LedgerState::test_ledger(),
                                             accounts: HashMap::new(),
                                             utxos: HashMap::new(),
                                             units: HashMap::new(),
                                             balances: HashMap::new(),
                                             owner_memos: HashMap::new(),
                                             confidential_amounts:
                                               scenario.confidential_amounts,
                                             confidential_types: scenario.confidential_types });

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
  use quickcheck;

  use std::sync::Mutex;

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
      assert_eq!(simpl_res, normal_res);
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
      } else {
        if simple_res.is_ok() {
          prev_simple.run_account_command(&cmd).unwrap();
          prev_normal.run_account_command(&cmd).unwrap();
        }
      }
    }

    simple.deep_invariant_check().unwrap();
    normal.deep_invariant_check().unwrap();
  }

  fn ledger_simulates_accounts(cmds: AccountsScenario, with_standalone: bool) {
    let _ = if with_standalone {
      Some(LEDGER_STANDALONE_LOCK.lock().unwrap())
    } else {
      None
    };

    let with_one_big_txn = cmds.cmds.len() < 20;

    let wait_time = time::Duration::from_millis(1000);
    let mut ledger = Box::new(LedgerAccounts { ledger: LedgerState::test_ledger(),
                                               accounts: HashMap::new(),
                                               utxos: HashMap::new(),
                                               units: HashMap::new(),
                                               balances: HashMap::new(),
                                               owner_memos: HashMap::new(),
                                               confidential_amounts: cmds.confidential_amounts,
                                               confidential_types: cmds.confidential_types });
    let mut big_txn = Box::new(OneBigTxnAccounts { base_ledger: LedgerState::test_ledger(),
                                                   txn: Transaction::default(),
                                                   txos: Default::default(),
                                                   accounts: HashMap::new(),
                                                   utxos: HashMap::new(),
                                                   units: HashMap::new(),
                                                   balances: HashMap::new(),
                                                   confidential_amounts:
                                                     cmds.confidential_amounts,
                                                   confidential_types: cmds.confidential_types });

    let mut active_ledger = if !with_standalone {
      None
    } else {
      Some(Box::new(LedgerStandaloneAccounts { client: LedgerStandalone::new_mock(1),
                                               prng: rand_chacha::ChaChaRng::from_entropy(),
                                               accounts: HashMap::new(),
                                               utxos: HashMap::new(),
                                               units: HashMap::new(),
                                               balances: HashMap::new(),
                                               owner_memos: HashMap::new(),
                                               confidential_amounts: cmds.confidential_amounts,
                                               confidential_types: cmds.confidential_types }))
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

    for cmd in cmds {
      simple.fast_invariant_check().unwrap();
      normal.fast_invariant_check().unwrap();

      let simple_res = simple.run_account_command(&cmd);
      let normal_res = normal.run_account_command(&cmd);
      assert!(simple_res.is_ok() || normal_res.is_err());
      let ledger_res = ledger.run_account_command(&cmd);
      assert!(ledger_res.is_ok() == normal_res.is_ok());
      let big_txn_res = if with_one_big_txn {
        let res = big_txn.run_account_command(&cmd);
        assert!(res.is_ok() == normal_res.is_ok());
        Some(res)
      } else {
        None
      };
      if with_standalone {
        let active_ledger_res = active_ledger.as_mut().unwrap().run_account_command(&cmd);
        // dbg!(&active_ledger_res);
        assert!(active_ledger_res.is_ok() == normal_res.is_ok());
      }

      if simple_res.is_err() != normal_res.is_err() {
        assert!(simple_res.is_ok());
        simple = prev_simple.clone();
      } else if simple_res.is_ok() {
        prev_simple.run_account_command(&cmd).unwrap();
        ledger_res.unwrap();
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
    ledger_simulates_accounts(cmds, true)
  }

  #[allow(dead_code)]
  fn ledger_simulates_accounts_no_standalone(cmds: AccountsScenario) {
    ledger_simulates_accounts(cmds, false)
  }

  fn regression_quickcheck_found(with_standalone: bool) {
    use AccountsCommand::*;
    for conf_amts in vec![false, true] {
      for conf_types in vec![false, true] {
        ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Send(UserName("".into()),
                                                                     0,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))],
                                                     confidential_types: conf_types,
                                                     confidential_amounts: conf_amts },
                                  with_standalone);
        ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Mint(1, UnitName("".into())),
                                                                Send(UserName("".into()),
                                                                     1,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))],
                                                     confidential_types: conf_types,
                                                     confidential_amounts: conf_amts },
                                  with_standalone);

        ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Mint(1, UnitName("".into())),
                                                                // ToggleConfTypes(),
                                                                Mint(1, UnitName("".into())),
                                                                Send(UserName("".into()),
                                                                     2,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))],
                                                     confidential_types: conf_types,
                                                     confidential_amounts: conf_amts },
                                  with_standalone);

        ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Mint(1, UnitName("".into())),
                                                                Send(UserName("".into()),
                                                                     1,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))],
                                                     confidential_types: conf_types,
                                                     confidential_amounts: conf_amts },
                                  with_standalone);
        ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Mint(1, UnitName("".into())),
                                                                Send(UserName("".into()),
                                                                     1,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))],
                                                     confidential_types: conf_types,
                                                     confidential_amounts: conf_amts },
                                  with_standalone);

        ledger_simulates_accounts(AccountsScenario { confidential_amounts: conf_types,
                                                     confidential_types: conf_amts,
                                                     cmds: vec![NewUser(UserName("".into())),
                                                                NewUser(UserName("\u{0}".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("\u{0}".into())),
                                                                Mint(34, UnitName("".into())),
                                                                Send(UserName("\u{0}".into()),
                                                                    1,
                                                                    UnitName("".into()),
                                                                    UserName("".into())),
                                                                Send(UserName("".into()),
                                                                    1,
                                                                    UnitName("".into()),
                                                                    UserName("".into()))] },
                                  with_standalone);

        ledger_simulates_accounts(AccountsScenario { confidential_amounts: conf_types,
                                                     confidential_types: conf_amts,
                                                     cmds: vec![NewUser(UserName("".into())),
                                                                NewUser(UserName("\u{0}".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("\u{0}".into())),
                                                                Mint(34, UnitName("".into())),
                                                                Send(UserName("\u{0}".into()),
                                                                    1,
                                                                    UnitName("".into()),
                                                                    UserName("".into())),
                                                                Send(UserName("".into()),
                                                                    1,
                                                                    UnitName("".into()),
                                                                    UserName("".into()))] },
                                  with_standalone);

        ledger_simulates_accounts(AccountsScenario { confidential_amounts: conf_types,
                                                     confidential_types: conf_amts,
                                                     cmds: vec![NewUser(UserName("".into())),
                                                                NewUnit(UnitName("".into()),
                                                                        UserName("".into())),
                                                                Mint(32, UnitName("".into())),
                                                                Send(UserName("".into()),
                                                                     1,
                                                                     UnitName("".into()),
                                                                     UserName("".into())),
                                                                Send(UserName("".into()),
                                                                     2,
                                                                     UnitName("".into()),
                                                                     UserName("".into()))] },
                                  with_standalone);
      }
    }
  }

  #[test]
  fn regression_quickcheck_found_no_standalone() {
    regression_quickcheck_found(false)
  }

  #[test]
  // This test passes, but we ignore it since it's slow
  // Redmine issue: #47
  #[ignore]
  fn regression_quickcheck_found_with_standalone() {
    regression_quickcheck_found(true)
  }

  #[test]
  fn quickcheck_ledger_simulates() {
    QuickCheck::new().tests(1).quickcheck(
                                          ledger_simulates_accounts_with_standalone
                     //.quickcheck(ledger_simulates_accounts_no_standalone
                                 as fn(AccountsScenario) -> (),
    );
  }
}
