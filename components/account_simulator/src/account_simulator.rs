#![deny(warnings)]
#[cfg(test)]
use quickcheck::{Arbitrary, Gen, QuickCheck};
#[cfg(test)]
use std::iter::repeat;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use findora::HasInvariants;
use ledger::data_model::compute_signature;
use ledger::data_model::errors::PlatformError;
use ledger::data_model::*;
use ledger::store::*;
#[cfg(test)]
use rand_core::SeedableRng;
use std::collections::{HashMap, HashSet, VecDeque};
#[cfg(test)]
use std::ffi::OsString;
#[cfg(test)]
use std::thread;
use std::time;
use submission_server::{TxnHandle, TxnStatus};
use subprocess::Popen;
#[cfg(test)]
use subprocess::PopenConfig;
use zei::serialization::ZeiFromToBytes;
use zei::setup::PublicParams;
use zei::xfr::asset_record::{build_blind_asset_record, open_blind_asset_record, AssetRecordType};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{AssetRecord, AssetRecordTemplate, OpenAssetRecord, OwnerMemo};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct UserName(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnitName(pub String);

#[cfg(test)]
impl Arbitrary for UserName {
  fn arbitrary<G: Gen>(g: &mut G) -> Self {
    UserName(String::arbitrary(g))
  }
  fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    Box::new(self.0.shrink().map(UserName))
  }
}

#[cfg(test)]
impl Arbitrary for UnitName {
  fn arbitrary<G: Gen>(g: &mut G) -> Self {
    UnitName(String::arbitrary(g))
  }
  fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    Box::new(self.0.shrink().map(UnitName))
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AccountsCommand {
  NewUser(UserName),           // name
  NewUnit(UnitName, UserName), // name, issuer
  Mint(usize, UnitName),       // count, unit
  Send(UserName, usize, UnitName, UserName), // source,count,unit,dest
                               // ToggleConfTypes(),
                               // ToggleConfAmts(),
}

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
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
#[cfg(test)]
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
                    .chain(repeat(amt.clone()).zip(unit.shrink()))
                    .map(|(a, u)| Mint(a, u)))
      }
      Send(src, amt, unit, dst) => {
        Box::new(// shrink src
                 src.shrink()
                    .zip(repeat(amt.clone()))
                    .zip(repeat(unit.clone()).zip(repeat(dst.clone())))
                    .chain(// shrink amt
                           repeat(src.clone()).zip(amt.shrink())
                                              .zip(repeat(unit.clone()).zip(repeat(dst.clone()))))
                    .chain(// shrink unit
                           repeat(src.clone()).zip(repeat(amt.clone()))
                                              .zip(unit.shrink().zip(repeat(dst.clone()))))
                    .chain(// shrink dst
                           repeat(src.clone()).zip(repeat(amt.clone()))
                                              .zip(repeat(unit.clone()).zip(dst.shrink())))
                    .map(|((src, amt), (unit, dst))| Send(src, amt, unit, dst)))
      }
      // ToggleConfAmts() => Box::new(std::iter::empty()),
      // ToggleConfTypes() => Box::new(std::iter::empty()),
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
      // AccountsCommand::ToggleConfAmts() => {},
      // AccountsCommand::ToggleConfTypes() => {},
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
      // AccountsCommand::ToggleConfAmts() => {},
      // AccountsCommand::ToggleConfTypes() => {},
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
  #[allow(unused)]
  confidential_types: bool,
}

impl InterpretAccounts<PlatformError> for LedgerAccounts {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = false; // && self.confidential_types;
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(self.ledger.get_prng());

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        let code = AssetTypeCode::gen_random();

        dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let sig = compute_signature(privkey, pubkey, &body);

        let op = DefineAsset { body,
                               pubkey: IssuerPublicKey { key: *pubkey },
                               signature: sig };

        let txn = Transaction { operations: vec![Operation::DefineAsset(op)],
                                credentials: vec![],
                                memos: vec![] };

        let eff = TxnEffect::compute_effect(self.ledger.get_prng(), txn).unwrap();

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
        let (issuer, code) = self.units.get(unit).ok_or(PlatformError::InputsError)?;

        let new_seq_num = self.ledger.get_issuance_num(&code).unwrap();

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let mut tx = Transaction::default();

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(self.ledger.get_prng(), &params.pc_gens, &ar, None);

        let asset_issuance_body = IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)]).unwrap();

        let sign = compute_signature(&privkey, &pubkey, &asset_issuance_body);

        let asset_issuance_operation = IssueAsset { body: asset_issuance_body,
                                                    pubkey: IssuerPublicKey { key: *pubkey },
                                                    signature: sign };

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        tx.operations.push(issue_op);
        let effect = TxnEffect::compute_effect(self.ledger.get_prng(), tx).unwrap();

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
        let src_keypair = self.accounts.get(src).ok_or(PlatformError::InputsError)?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts.get(dst).ok_or(PlatformError::InputsError)?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code) = self.units.get(unit).ok_or(PlatformError::InputsError)?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError);
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<TxoSID> = Vec::new();
        let mut to_skip: Vec<TxoSID> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = &(self.ledger.get_utxo(sid).unwrap().0).0;
          let memo = self.owner_memos.get(&sid).cloned();
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
        }
        dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();
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
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        dbg!(unit_code.val);
        for (ix, rec) in src_records.iter().enumerate() {
          dbg!(ix,
               rec.get_asset_type(),
               rec.get_amount(),
               rec.get_pub_key());
        }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let transfer_body =
          TransferAssetBody::new(self.ledger.get_prng(),
                                 to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                                 src_records.as_slice(),
                                 all_outputs.as_slice()).unwrap();

        let mut owners_memos = transfer_body.transfer.owners_memos.clone();
        dbg!(&transfer_body);
        let transfer_sig =
          SignedAddress { address: XfrAddress { key: *src_pub },
                          signature: compute_signature(src_priv, src_pub, &transfer_body) };

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig],
                                       transfer_type: TransferType::Standard };
        let txn = Transaction { operations: vec![Operation::TransferAsset(transfer)],
                                credentials: vec![],
                                memos: vec![] };

        let effect = TxnEffect::compute_effect(self.ledger.get_prng(), txn).unwrap();

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
      } // AccountsCommand::ToggleConfAmts() => {
        //     self.confidential_amounts = !conf_amts;
        // }
        // AccountsCommand::ToggleConfTypes() => {
        //     self.confidential_types = !conf_types;
        // }
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
  #[allow(unused)]
  confidential_types: bool,
}

impl InterpretAccounts<PlatformError> for OneBigTxnAccounts {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = false;
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(self.base_ledger.get_prng());

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        let code = AssetTypeCode::gen_random();

        dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let sig = compute_signature(privkey, pubkey, &body);

        let op = DefineAsset { body,
                               pubkey: IssuerPublicKey { key: *pubkey },
                               signature: sig };

        self.txn.operations.push(Operation::DefineAsset(op));
        let eff = TxnEffect::compute_effect(self.base_ledger.get_prng(), self.txn.clone()).unwrap();
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
          self.units.get_mut(unit).ok_or(PlatformError::InputsError)?;
        *new_seq_num += 1;
        let new_seq_num = *new_seq_num - 1;

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(self.base_ledger.get_prng(), &params.pc_gens, &ar, None);

        let asset_issuance_body = IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)]).unwrap();

        let sign = compute_signature(&privkey, &pubkey, &asset_issuance_body);

        let asset_issuance_operation = IssueAsset { body: asset_issuance_body,
                                                    pubkey: IssuerPublicKey { key: *pubkey },
                                                    signature: sign };

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        self.txn.operations.push(issue_op);
        let effect =
          TxnEffect::compute_effect(self.base_ledger.get_prng(), self.txn.clone()).unwrap();
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
        let src_keypair = self.accounts.get(src).ok_or(PlatformError::InputsError)?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts.get(dst).ok_or(PlatformError::InputsError)?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code, _) = self.units.get(unit).ok_or(PlatformError::InputsError)?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError);
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<usize> = Vec::new();
        let mut to_skip: Vec<usize> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = &((self.txos.get(sid).unwrap().0).0);
          let memo = &(self.txos.get(sid).unwrap().1);
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(self.txos.len() - 1 - sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
        }
        dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();

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
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        dbg!(unit_code.val);
        for (ix, rec) in src_records.iter().enumerate() {
          dbg!(ix,
               rec.get_asset_type(),
               rec.get_amount(),
               rec.get_pub_key());
        }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let transfer_body = TransferAssetBody::new(self.base_ledger.get_prng(),
                                                   to_use.iter()
                                                         .cloned()
                                                         .map(|x| TxoRef::Relative(x as u64))
                                                         .collect(),
                                                   src_records.as_slice(),
                                                   all_outputs.as_slice()).unwrap();
        let owners_memos = transfer_body.transfer.owners_memos.clone();
        dbg!(&transfer_body);
        let transfer_sig =
          SignedAddress { address: XfrAddress { key: *src_pub },
                          signature: compute_signature(src_priv, src_pub, &transfer_body) };

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig],
                                       transfer_type: TransferType::Standard };

        self.txn.operations.push(Operation::TransferAsset(transfer));

        let effect =
          TxnEffect::compute_effect(self.base_ledger.get_prng(), self.txn.clone()).unwrap();
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
      } // AccountsCommand::ToggleConfAmts() => {
        //     self.confidential_amounts = !conf_amts;
        // }
        // AccountsCommand::ToggleConfTypes() => {
        //     self.confidential_types = !conf_types;
        // }
    }
    Ok(())
  }
}

struct LedgerStandaloneAccounts {
  ledger: Popen,
  submit_port: usize,
  query_port: usize,
  client: reqwest::Client,
  prng: rand_chacha::ChaChaRng,
  accounts: HashMap<UserName, XfrKeyPair>,
  balances: HashMap<UserName, HashMap<UnitName, u64>>,
  utxos: HashMap<UserName, VecDeque<TxoSID>>, // by account
  units: HashMap<UnitName, (UserName, AssetTypeCode)>, // user, data
  owner_memos: HashMap<TxoSID, OwnerMemo>,
  // These only affect new issuances
  confidential_amounts: bool,
  #[allow(unused)]
  confidential_types: bool,
}

impl Drop for LedgerStandaloneAccounts {
  fn drop(&mut self) {
    self.ledger.terminate().unwrap();
    if self.ledger
           .wait_timeout(time::Duration::from_millis(10))
           .is_err()
    {
      self.ledger.kill().unwrap();
      self.ledger.wait().unwrap();
    }
  }
}

impl InterpretAccounts<PlatformError> for LedgerStandaloneAccounts {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(), PlatformError> {
    let conf_amts = self.confidential_amounts;
    let conf_types = false;
    let art = AssetRecordType::from_booleans(conf_amts, conf_types);
    dbg!(cmd);
    match cmd {
      AccountsCommand::NewUser(name) => {
        let keypair = XfrKeyPair::generate(&mut self.prng);

        self.accounts
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        dbg!("New user", &name, &keypair);

        self.accounts.insert(name.clone(), keypair);
        self.utxos.insert(name.clone(), VecDeque::new());
        self.balances.insert(name.clone(), HashMap::new());
      }
      AccountsCommand::NewUnit(name, issuer) => {
        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());

        self.units
            .get(name)
            .map_or_else(|| Ok(()), |_| Err(PlatformError::InputsError))?;

        let code = AssetTypeCode::gen_random();

        dbg!("New unit", &name, &issuer, &code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *pubkey;

        let body = DefineAssetBody { asset: properties };

        let sig = compute_signature(privkey, pubkey, &body);

        let op = DefineAsset { body,
                               pubkey: IssuerPublicKey { key: *pubkey },
                               signature: sig };

        let txn = Transaction { operations: vec![Operation::DefineAsset(op)],
                                credentials: vec![],
                                memos: vec![] };

        {
          // let serialize = serde_json::to_string(&tx).unwrap();

          let host = "localhost";
          let port = format!("{}", self.submit_port);
          let query1 = format!("http://{}:{}/submit_transaction", host, port);
          let query2 = format!("http://{}:{}/force_end_block", host, port);
          dbg!(&query1);
          dbg!(&query2);
          let h = serde_json::from_str::<TxnHandle>(&self.client
                                                         .post(&query1)
                                                         .json(&txn)
                                                         .send()
                                                         .unwrap()
                                                         .error_for_status()
                                                         .map_err(|x| {
                                                           PlatformError::IoError(format!("{}", x))
                                                         })?
                                                         .text()
                                                         .unwrap()).unwrap();
          let query3 = format!("http://{}:{}/txn_status/{}", host, port, h.0);
          self.client
              .post(&query2)
              .send()
              .unwrap()
              .error_for_status()
              .unwrap()
              .text()
              .unwrap();
          match serde_json::from_str::<TxnStatus>(&self.client
                                                       .get(&query3)
                                                       .send()
                                                       .unwrap()
                                                       .error_for_status()
                                                       .unwrap()
                                                       .text()
                                                       .unwrap()).unwrap()
          {
            TxnStatus::Committed((_sid, _txos)) => {}
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
        let (issuer, code) = self.units.get(unit).ok_or(PlatformError::InputsError)?;

        let new_seq_num = {
          let host = "localhost";
          let port = format!("{}", self.query_port);
          let query = format!("http://{}:{}/asset_issuance_num/{}",
                              host,
                              port,
                              code.to_base64());
          dbg!(&query);
          reqwest::get(&query).unwrap()
                              .error_for_status()
                              .unwrap()
                              .text()
                              .unwrap()
        };
        dbg!(&new_seq_num);
        let new_seq_num = serde_json::from_str::<u64>(&new_seq_num).unwrap();

        let keypair = self.accounts
                          .get(issuer)
                          .ok_or(PlatformError::InputsError)?;
        let (pubkey, privkey) = (keypair.get_pk_ref(), keypair.get_sk_ref());
        let utxos = self.utxos.get_mut(issuer).unwrap();

        *self.balances
             .get_mut(issuer)
             .unwrap()
             .get_mut(unit)
             .unwrap() += amt;

        let mut tx = Transaction::default();

        let ar = AssetRecordTemplate::with_no_asset_tracking(amt, code.val, art, *pubkey);
        let params = PublicParams::new();
        let (ba, _, owner_memo) =
          build_blind_asset_record(&mut self.prng, &params.pc_gens, &ar, None);

        let asset_issuance_body = IssueAssetBody::new(&code, new_seq_num, &[TxOutput(ba)]).unwrap();

        let sign = compute_signature(&privkey, &pubkey, &asset_issuance_body);

        let asset_issuance_operation = IssueAsset { body: asset_issuance_body,
                                                    pubkey: IssuerPublicKey { key: *pubkey },
                                                    signature: sign };

        let issue_op = Operation::IssueAsset(asset_issuance_operation);

        tx.operations.push(issue_op);

        let txos = {
          // let serialize = serde_json::to_string(&tx).unwrap();

          let host = "localhost";
          let port = format!("{}", self.submit_port);
          let query1 = format!("http://{}:{}/submit_transaction", host, port);
          let query2 = format!("http://{}:{}/force_end_block", host, port);
          dbg!(&query1);
          dbg!(&query2);
          let h = serde_json::from_str::<TxnHandle>(&self.client
                                                         .post(&query1)
                                                         .json(&tx)
                                                         .send()
                                                         .unwrap()
                                                         .error_for_status()
                                                         .map_err(|x| {
                                                           PlatformError::IoError(format!("{}", x))
                                                         })?
                                                         .text()
                                                         .unwrap()).unwrap();
          let query3 = format!("http://{}:{}/txn_status/{}", host, port, h.0);
          self.client
              .post(&query2)
              .send()
              .unwrap()
              .error_for_status()
              .unwrap()
              .text()
              .unwrap();
          match serde_json::from_str::<TxnStatus>(&self.client
                                                       .get(&query3)
                                                       .send()
                                                       .unwrap()
                                                       .error_for_status()
                                                       .unwrap()
                                                       .text()
                                                       .unwrap()).unwrap()
          {
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
        let src_keypair = self.accounts.get(src).ok_or(PlatformError::InputsError)?;
        let (src_pub, src_priv) = (src_keypair.get_pk_ref(), src_keypair.get_sk_ref());
        let dst_keypair = self.accounts.get(dst).ok_or(PlatformError::InputsError)?;
        let (dst_pub, _) = (dst_keypair.get_pk_ref(), dst_keypair.get_sk_ref());
        let (_, unit_code) = self.units.get(unit).ok_or(PlatformError::InputsError)?;

        if *self.balances.get(src).unwrap().get(unit).unwrap() < amt {
          return Err(PlatformError::InputsError);
        }
        if amt == 0 {
          return Ok(());
        }

        *self.balances.get_mut(src).unwrap().get_mut(unit).unwrap() -= amt;
        *self.balances.get_mut(dst).unwrap().get_mut(unit).unwrap() += amt;

        let mut src_records: Vec<OpenAssetRecord> = Vec::new();
        let mut total_sum = 0u64;
        let avail = self.utxos.get_mut(src).unwrap();
        let mut to_use: Vec<TxoSID> = Vec::new();
        let mut to_skip: Vec<TxoSID> = Vec::new();

        while total_sum < amt && !avail.is_empty() {
          let sid = avail.pop_front().unwrap();
          let blind_rec = serde_json::from_str::<TxOutput>(&{
            let host = "localhost";
            let port = format!("{}",self.query_port);
            reqwest::get(&format!("http://{}:{}/utxo_sid/{}",host,port,sid.0)).unwrap().error_for_status().unwrap().text().unwrap()
          }).unwrap().0;
          let memo = self.owner_memos.get(&sid).cloned();
          let open_rec = open_blind_asset_record(&blind_rec, &memo, &src_priv).unwrap();
          dbg!(sid, open_rec.get_amount(), open_rec.get_asset_type());
          if *open_rec.get_asset_type() != unit_code.val {
            to_skip.push(sid);
            continue;
          }

          debug_assert!(*open_rec.get_asset_type() == unit_code.val);
          to_use.push(sid);
          total_sum += *open_rec.get_amount();
          src_records.push(open_rec);
        }
        dbg!(&to_skip, &to_use);
        avail.extend(to_skip.into_iter());

        assert!(total_sum >= amt);

        let mut src_outputs: Vec<AssetRecord> = Vec::new();
        let mut dst_outputs: Vec<AssetRecord> = Vec::new();
        let mut all_outputs: Vec<AssetRecord> = Vec::new();

        {
          // Simple output to dst
          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          dst_outputs.push(ar);

          let ar = AssetRecordTemplate::with_no_asset_tracking(amt, unit_code.val, art, *dst_pub);
          let ar = AssetRecord::from_template_no_identity_tracking(&mut self.prng, &ar).unwrap();
          all_outputs.push(ar);
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
        }

        let src_outputs = src_outputs;
        let dst_outputs = dst_outputs;
        let all_outputs = all_outputs;
        assert!(!src_records.is_empty());
        dbg!(unit_code.val);
        for (ix, rec) in src_records.iter().enumerate() {
          dbg!(ix,
               rec.get_asset_type(),
               rec.get_amount(),
               rec.get_pub_key());
        }

        let mut sig_keys: Vec<XfrKeyPair> = Vec::new();

        for _ in to_use.iter() {
          sig_keys.push(XfrKeyPair::zei_from_bytes(&src_keypair.zei_to_bytes()));
        }

        let transfer_body =
          TransferAssetBody::new(&mut self.prng,
                                 to_use.iter().cloned().map(TxoRef::Absolute).collect(),
                                 src_records.as_slice(),
                                 all_outputs.as_slice()).unwrap();

        let mut owners_memos = transfer_body.transfer.owners_memos.clone();
        dbg!(&transfer_body);
        let transfer_sig =
          SignedAddress { address: XfrAddress { key: *src_pub },
                          signature: compute_signature(src_priv, src_pub, &transfer_body) };

        let transfer = TransferAsset { body: transfer_body,
                                       body_signatures: vec![transfer_sig],
                                       transfer_type: TransferType::Standard };
        let txn = Transaction { operations: vec![Operation::TransferAsset(transfer)],
                                credentials: vec![],
                                memos: vec![] };

        let txos = {
          // let serialize = serde_json::to_string(&tx).unwrap();

          let host = "localhost";
          let port = format!("{}", self.submit_port);
          let query1 = format!("http://{}:{}/submit_transaction", host, port);
          let query2 = format!("http://{}:{}/force_end_block", host, port);
          dbg!(&query1);
          dbg!(&query2);
          let h = serde_json::from_str::<TxnHandle>(&self.client
                                                         .post(&query1)
                                                         .json(&txn)
                                                         .send()
                                                         .unwrap()
                                                         .error_for_status()
                                                         .map_err(|x| {
                                                           PlatformError::IoError(format!("{}", x))
                                                         })?
                                                         .text()
                                                         .unwrap()).unwrap();
          let query3 = format!("http://{}:{}/txn_status/{}", host, port, h.0);
          self.client
              .post(&query2)
              .send()
              .unwrap()
              .error_for_status()
              .unwrap()
              .text()
              .unwrap();
          match serde_json::from_str::<TxnStatus>(&self.client
                                                       .get(&query3)
                                                       .send()
                                                       .unwrap()
                                                       .error_for_status()
                                                       .unwrap()
                                                       .text()
                                                       .unwrap()).unwrap()
          {
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
      } // AccountsCommand::ToggleConfAmts() => {
        //     self.confidential_amounts = !conf_amts;
        // }
        // AccountsCommand::ToggleConfTypes() => {
        //     self.confidential_types = !conf_types;
        // }
    }
    Ok(())
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct AccountsScenario {
  // These only affect new issuances
  confidential_amounts: bool,
  confidential_types: bool,
  cmds: Vec<AccountsCommand>,
}

#[cfg(test)]
impl Arbitrary for AccountsScenario {
  fn arbitrary<G: Gen>(g: &mut G) -> Self {
    // A random AccountsScenario has...
    let mut cmds: Vec<AccountsCommand> = Vec::new();

    // Some users (at least one)...
    let mut users: HashSet<UserName> = vec![UserName::arbitrary(g)].into_iter().collect();
    for extra_user in Vec::<UserName>::arbitrary(g) {
      if users.contains(&extra_user) {
        continue;
      }
      users.insert(extra_user);
    }

    for user in users.iter() {
      cmds.push(AccountsCommand::NewUser(user.clone()));
    }

    let user_vec: Vec<UserName> = users.into_iter().collect();

    // Some defined units-of-some-asset (at least one) issued by those
    // users...
    let mut units: HashSet<UnitName> = vec![UnitName::arbitrary(g)].into_iter().collect();
    for extra_unit in Vec::<UnitName>::arbitrary(g) {
      if units.contains(&extra_unit) {
        continue;
      }
      units.insert(extra_unit);
    }

    let unit_vec: Vec<UnitName> = units.into_iter().collect();
    let mut unit_amounts: HashMap<UnitName, usize> = HashMap::new();

    // Initial quantities of assets...
    for unit in unit_vec.iter() {
      let user = user_vec[usize::arbitrary(g) % user_vec.len()].clone();
      let amt = usize::arbitrary(g);
      unit_amounts.insert(unit.clone(), amt);
      // match g.next_u32() % 12 {
      //     0..=2 => { cmds.push(AccountsCommand::ToggleConfAmts()); }
      //     3..=4 => { cmds.push(AccountsCommand::ToggleConfTypes()); }
      //     _ => {}
      // }
      cmds.push(AccountsCommand::NewUnit(unit.clone(), user.clone()));
      cmds.push(AccountsCommand::Mint(amt, unit.clone()));
    }

    // And some activity.
    for (src, count, unit, dst) in Vec::<(usize, usize, usize, usize)>::arbitrary(g) {
      let src = user_vec[src % user_vec.len()].clone();
      let dst = user_vec[dst % user_vec.len()].clone();
      let unit = unit_vec[unit % unit_vec.len()].clone();
      let amt = unit_amounts.get_mut(&unit).unwrap();
      match g.next_u32() % 10 {
        0..=7 => {
          let count = if *amt != 0 { count % *amt } else { 0 };
          cmds.push(AccountsCommand::Send(src, count, unit, dst));
        }
        8..=9 => {
          *amt += count;
          cmds.push(AccountsCommand::Mint(count, unit));
        }
        // 5|6|10 => { cmds.push(AccountsCommand::ToggleConfAmts()); }
        // 7|11 => { cmds.push(AccountsCommand::ToggleConfTypes()); }
        _ => assert!(false),
      }
    }

    AccountsScenario { confidential_amounts: bool::arbitrary(g),
                       confidential_types: bool::arbitrary(g),
                       cmds }
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
      let shrink_inside = repeat((conf_amounts.clone(),conf_types.clone())).zip(ix.clone())
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
                    .chain((base_cmds[(i+1)..]).into_iter()
                      .map(|cmd| rename_by(&old_cmd,&x,cmd.clone())))
                    .collect()
                }
            )
          }
      );
      // then try "shrinking" the confidentiality parameters
      let shrink_conf =
        repeat(conf_amounts.clone()).zip(conf_types.shrink())
                                    .chain(conf_amounts.shrink().zip(repeat(conf_types.clone())))
                                    .zip(repeat(base_cmds.clone()))
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

    let with_one_big_txn = true; //cmds.cmds.len() < 20;

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
      Some(Box::new(
      LedgerStandaloneAccounts {
        ledger: Popen::create(&["/usr/bin/env", "bash", "-c", "flock .test_standalone_lock cargo run"],
                  PopenConfig {
                    cwd: Some(OsString::from("../ledger_standalone/")),
                    ..Default::default()
                  }).unwrap(),
        submit_port: 8669,
        query_port: 8668,
        client: reqwest::Client::new(),
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
    dbg!(&cmds);

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
        dbg!(&active_ledger_res);
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
    ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                            NewUnit(UnitName("".into()),
                                                                    UserName("".into())),
                                                            Send(UserName("".into()),
                                                                 0,
                                                                 UnitName("".into()),
                                                                 UserName("".into()))],
                                                 confidential_types: false,
                                                 confidential_amounts: false },
                              with_standalone);
    ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                            NewUnit(UnitName("".into()),
                                                                    UserName("".into())),
                                                            Mint(1, UnitName("".into())),
                                                            Send(UserName("".into()),
                                                                 1,
                                                                 UnitName("".into()),
                                                                 UserName("".into()))],
                                                 confidential_types: false,
                                                 confidential_amounts: false },
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
                                                 confidential_types: false,
                                                 confidential_amounts: false },
                              with_standalone);

    ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                            NewUnit(UnitName("".into()),
                                                                    UserName("".into())),
                                                            Mint(1, UnitName("".into())),
                                                            Send(UserName("".into()),
                                                                 1,
                                                                 UnitName("".into()),
                                                                 UserName("".into()))],
                                                 confidential_types: false,
                                                 confidential_amounts: false },
                              with_standalone);
    ledger_simulates_accounts(AccountsScenario { cmds: vec![NewUser(UserName("".into())),
                                                            NewUnit(UnitName("".into()),
                                                                    UserName("".into())),
                                                            Mint(1, UnitName("".into())),
                                                            Send(UserName("".into()),
                                                                 1,
                                                                 UnitName("".into()),
                                                                 UserName("".into()))],
                                                 confidential_types: false,
                                                 confidential_amounts: true },
                              with_standalone);

    ledger_simulates_accounts(AccountsScenario { confidential_amounts: false,
                                                 confidential_types: false,
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

    ledger_simulates_accounts(AccountsScenario { confidential_amounts: true,
                                                 confidential_types: false,
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

    ledger_simulates_accounts(AccountsScenario { confidential_amounts: false,
                                                 confidential_types: false,
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

  #[test]
  fn regression_quickcheck_found_no_standalone() {
    regression_quickcheck_found(false)
  }

  #[test]
  #[ignore]
  fn regression_quickcheck_found_with_standalone() {
    regression_quickcheck_found(true)
  }

  #[test]
  #[ignore]
  fn quickcheck_ledger_simulates() {
    QuickCheck::new().tests(5)
                     // .quickcheck(ledger_simulates_accounts_with_standalone
                     .quickcheck(ledger_simulates_accounts_no_standalone
                                 as fn(AccountsScenario) -> ());
  }
}
