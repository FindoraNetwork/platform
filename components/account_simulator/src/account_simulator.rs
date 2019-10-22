#[cfg(test)]
use quickcheck::{Arbitrary,Gen};
#[cfg(test)]
use rand::Rng;
#[cfg(test)]
use std::iter::{once,repeat};
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use findora::HasInvariants;
use ledger::data_model::*;
use ledger::store::*;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Clone, Debug, Eq, PartialEq)]
enum AccountsCommand {
    NewUser(String), // name
    NewUnit(String, String), // name, issuer
    Mint(usize, String), // count, unit
    Send(String, usize, String, String), // source,count,unit,dest
}

fn rename_user(old: &str, new: &str, ac: AccountsCommand) -> AccountsCommand {
  use AccountsCommand::*;
  match ac {
    NewUnit(unit,name) => NewUnit(unit, if name == old { new.to_string() } else { name }),
    Send(src,count,unit,dest) => Send(if src == old { new.to_string() } else { src },
      count, unit, if dest == old { new.to_string() } else { dest }),
    _ => ac,
  }
}

fn rename_unit(old: &str, new: &str, ac: AccountsCommand) -> AccountsCommand {
  use AccountsCommand::*;
  match ac {
    Mint(count,unit) => Mint(count,if unit == old { new.to_string() } else { unit }),
    Send(src,count,unit,dest) => Send(src,count,if unit == old { new.to_string() } else { unit },dest),
    _ => ac,
  }
}

fn rename_by(old: &AccountsCommand, new: &AccountsCommand, ac: AccountsCommand) -> AccountsCommand {
  use AccountsCommand::*;
  match (old,new) {
    (NewUser(old_name), NewUser(new_name)) => rename_user(old_name,new_name,ac),
    (NewUnit(old_name,iss1), NewUnit(new_name,iss2)) => rename_user(iss1,iss2,rename_unit(old_name,new_name,ac)),
    _ => ac,
  }
}

#[cfg(test)]
impl Arbitrary for AccountsCommand {
  fn arbitrary<G: Gen>(g: &mut G) -> AccountsCommand {
    match g.next_u32() % 10 {
      0     => AccountsCommand::NewUser(String::arbitrary(g)),
      1     => AccountsCommand::NewUnit(String::arbitrary(g),String::arbitrary(g)),
      2..=4 => AccountsCommand::Mint(usize::arbitrary(g),String::arbitrary(g)),
      5..=9 => AccountsCommand::Send(String::arbitrary(g), usize::arbitrary(g),
                String::arbitrary(g), String::arbitrary(g)),
      _ => panic!("Out of range"),
    }
  }

  fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    use AccountsCommand::*;
    match self {
      NewUser(name) => {
        Box::new(name.shrink().map(NewUser))
      },
      NewUnit(name,issuer) => {
        Box::new(name.clone().shrink().zip(repeat(issuer.clone()))
            .flat_map(|(name,issuer)|
                once(NewUnit(name.clone(),issuer.clone()))
                .chain(repeat(name).zip(issuer.clone().shrink())
                  .map(|(n,i)| NewUnit(n,i)))))
      },
      Mint(amt,unit) => {
        Box::new(amt.shrink().zip(repeat(unit.clone()))
            .flat_map(|(amt,unit)|
              once(Mint(amt.clone(),unit.clone()))
              .chain(repeat(amt).zip(unit.clone().shrink())
                .map(|(a,u)| Mint(a,u)))))
      },
      Send(src,amt,unit,dst) => {
        Box::new(src.clone().shrink().zip(repeat((*amt,unit.clone(),dst.clone())))
          .flat_map(|(src,(amt,unit,dst))|
            once(((src.clone(),amt.clone()),(unit.clone(),dst.clone())))
            .chain(
              repeat(src).zip(amt.shrink()).zip(repeat((unit,dst))))
          )
          .flat_map(|((src,amt),(unit,dst))|
            once((((src.clone(),amt.clone()),unit.clone()),dst.clone()))
            .chain(
              repeat((src,amt)).zip(unit.shrink()).zip(repeat(dst)))
          )
          .flat_map(|(((src,amt),unit),dst)|
            once(((src.clone(),amt.clone(),unit.clone()),dst.clone()))
            .chain(


              repeat((src,amt,unit)).zip(dst.shrink()))
          )
          .map(|((src,amt,unit),dst)|
            Send(src,amt,unit,dst)))
      },
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
struct SimpleAccountsState {
    accounts: HashMap<String, ()>,
    units_to_users: HashMap<String, String>,
    users_to_units: HashMap<String, HashSet<String>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
struct AccountsState {
    accounts: HashMap<String, HashMap<String,usize>>,
    units_to_users: HashMap<String, String>,
    users_to_units: HashMap<String, HashSet<String>>,
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

    for (name,_) in self.accounts.iter() {
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

    SimpleAccountsState{
      accounts: self.accounts.iter().map(|(acc,_)| (acc.clone(),())).collect(),
      units_to_users: self.units_to_users.clone(),
      users_to_units: self.users_to_units.clone(),
    }.deep_invariant_check()?;


    for (_,balances) in self.accounts.iter() {
      for (unit, _) in self.units_to_users.iter() {
        balances.get(unit).ok_or(())?;
      }
    }

    Ok(())
  }
}

trait InterpretAccounts<ErrT> {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(),ErrT>;
}

impl InterpretAccounts<()>  for SimpleAccountsState {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(),()> {
    match cmd {
      AccountsCommand::NewUser(name) => {
        self.accounts.get(name).map_or_else(|| Ok(()),|_| Err(()))?;
        self.users_to_units.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

        self.accounts.insert(name.clone(), ());
        self.users_to_units.insert(name.clone(), HashSet::new());
      }
      AccountsCommand::NewUnit(name,issuer) => {
        self.accounts.get(issuer).ok_or(())?;
        let unit_set = self.users_to_units.get_mut(issuer).ok_or(())?;
        self.units_to_users.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

        unit_set.insert(name.clone());
        self.units_to_users.insert(name.clone(),issuer.clone());
      }
      AccountsCommand::Mint(_,unit) => {
        self.accounts.get(self.units_to_users.get(unit).ok_or(())?).ok_or(())?;
      }
      AccountsCommand::Send(src,_,unit,dst) => {
        self.accounts.get(src).ok_or(())?;
        self.accounts.get(dst).ok_or(())?;
        self.units_to_users.get(unit).ok_or(())?;
      }
    }
    Ok(())
  }
}

impl InterpretAccounts<()> for AccountsState {
  fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(),()> {
    match cmd {
      AccountsCommand::NewUser(name) => {
        self.accounts.get(name).map_or_else(|| Ok(()),|_| Err(()))?;
        self.users_to_units.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

        self.accounts.insert(name.clone(),
          self.units_to_users.iter().map(|(unit,_)| (unit.clone(),0)).collect());
        self.users_to_units.insert(name.clone(), HashSet::new());
      }
      AccountsCommand::NewUnit(name,issuer) => {
        self.accounts.get(issuer).ok_or(())?;
        let unit_set = self.users_to_units.get_mut(issuer).ok_or(())?;
        self.units_to_users.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

        unit_set.insert(name.clone());
        self.units_to_users.insert(name.clone(),issuer.clone());

        for (user,acc) in self.accounts.iter_mut() {
          acc.insert(name.clone(),0);
        }
      }
      AccountsCommand::Mint(amt,unit) => {
        let acct = self.accounts.get_mut(self.units_to_users.get(unit).ok_or(())?).ok_or(())?;
        *acct.get_mut(unit).ok_or(())? += amt;
      }
      AccountsCommand::Send(src,amt,unit,dst) => {
        {
          let dst_acct = self.accounts.get(dst).ok_or(())?;
          let dst_column = dst_acct.get(unit).ok_or(())?;
        }
        {
          let src_acct = self.accounts.get_mut(src).ok_or(())?;
          let src_column = src_acct.get_mut(unit).ok_or(())?;
          if *src_column < *amt { return Err(()); }
          *src_column -= amt;
        }
        {
          let dst_acct = self.accounts.get_mut(dst).unwrap();
          let dst_column = dst_acct.get_mut(unit).unwrap();

          *dst_column += amt;
        }
      }
    }
    Ok(())
  }
}

// struct LedgerAccounts {
//   ledger: LedgerState,
//   accounts: HashMap<String,(XfrPublicKey, XfrSecretKey)>,
//   utxos: HashMap<String,VecDeque<TxoSID>>, // by account
//   units: HashMap<String,AssetTokenCode>,
// }

// impl InterpretAccounts<PlatformError> for LedgerState {
//   fn run_account_command(&mut self, cmd: &AccountsCommand) -> Result<(),()> {
//     match cmd {
//       AccountsCommand::NewUser(name) => {
//         self.accounts.get(name).map_or_else(|| Ok(()),|_| Err(()))?;
//         self.users_to_units.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

//         self.accounts.insert(name.clone(),
//           self.units_to_users.iter().map(|(unit,_)| (unit.clone(),0)).collect());
//         self.users_to_units.insert(name.clone(), HashSet::new());
//       }
//       AccountsCommand::NewUnit(name,issuer) => {
//         self.accounts.get(issuer).ok_or(())?;
//         let unit_set = self.users_to_units.get_mut(issuer).ok_or(())?;
//         self.units_to_users.get(name).map_or_else(|| Ok(()),|_| Err(()))?;

//         unit_set.insert(name.clone());
//         self.units_to_users.insert(name.clone(),issuer.clone());

//         for (user,acc) in self.accounts.iter_mut() {
//           acc.insert(name.clone(),0);
//         }
//       }
//       AccountsCommand::Mint(amt,unit) => {
//         let acct = self.accounts.get_mut(self.units_to_users.get(unit).ok_or(())?).ok_or(())?;
//         *acct.get_mut(unit).ok_or(())? += amt;
//       }
//       AccountsCommand::Send(src,amt,unit,dst) => {
//         {
//           let dst_acct = self.accounts.get(dst).ok_or(())?;
//           let dst_column = dst_acct.get(unit).ok_or(())?;
//         }
//         {
//           let src_acct = self.accounts.get_mut(src).ok_or(())?;
//           let src_column = src_acct.get_mut(unit).ok_or(())?;
//           if *src_column < *amt { return Err(()); }
//           *src_column -= amt;
//         }
//         {
//           let dst_acct = self.accounts.get_mut(dst).unwrap();
//           let dst_column = dst_acct.get_mut(unit).unwrap();

//           *dst_column += amt;
//         }
//       }
//     }
//     Ok(())
//   }
// }

#[derive(Clone, Debug, Eq, PartialEq)]
struct AccountsScenario {
  cmds: Vec<AccountsCommand>,
}

//impl Testable for AccountsScenario

#[cfg(test)]
impl Arbitrary for AccountsScenario {
  fn arbitrary<G: Gen>(g: &mut G) -> AccountsScenario {
    let mut cmds: Vec<AccountsCommand> = Vec::new();

    let mut users: HashSet<String> = vec![String::arbitrary(g)].into_iter().collect();
    for extra_user in Vec::<String>::arbitrary(g) {
      if users.contains(&extra_user) { continue; }
      users.insert(extra_user);
    }

    for user in users.iter() {
      cmds.push(AccountsCommand::NewUser(user.clone()));
    }

    let user_vec: Vec<String> = users.into_iter().collect();

    let mut units: HashSet<String> = vec![String::arbitrary(g)].into_iter().collect();
    for extra_unit in Vec::<String>::arbitrary(g) {
      if units.contains(&extra_unit) { continue; }
      units.insert(extra_unit);
    }

    let unit_vec: Vec<String> = units.into_iter().collect();
    let mut unit_amounts: HashMap<String,usize> = HashMap::new();

    for unit in unit_vec.iter() {
      let user = user_vec[usize::arbitrary(g) % user_vec.len()].clone();
      let amt = usize::arbitrary(g);
      unit_amounts.insert(unit.clone(),amt);
      cmds.push(AccountsCommand::NewUnit(unit.clone(), user.clone()));
      cmds.push(AccountsCommand::Mint(amt, unit.clone()));
    }

    for (src,count,unit,dst) in Vec::<(usize,usize,usize,usize)>::arbitrary(g) {
      let src = user_vec[src % user_vec.len()].clone();
      let dst = user_vec[dst % user_vec.len()].clone();
      let unit = unit_vec[unit % unit_vec.len()].clone();
      let amt = unit_amounts.get(&unit).unwrap();
      let count = if *amt != 0 { count % *amt } else { 0 };
      cmds.push(AccountsCommand::Send(src,count,unit,dst));
    }

    AccountsScenario { cmds }
  }

  fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    let ix = 0..(self.cmds.len());
    let base_cmds = self.cmds.clone();

    Box::new(
      ix.clone().zip(repeat(base_cmds.clone())).map(|(i,base_cmds)| AccountsScenario {
        cmds: base_cmds[..i].iter().chain(&base_cmds[(i+1)..]).cloned().collect()
      }).chain(ix.zip(repeat(base_cmds.clone())).flat_map(
        |(i,base_cmds)|
          {
            let old_cmd = base_cmds[i].clone();
            base_cmds[i].shrink().zip(repeat((old_cmd,base_cmds,i))).map(|(x,(old_cmd,base_cmds,i))|
              AccountsScenario { cmds:
                base_cmds[..i].iter().chain(&vec![x.clone()]).cloned()
                  .chain((base_cmds[(i+1)..]).into_iter()
                    .map(|cmd| rename_by(&old_cmd,&x,cmd.clone())))
                  .collect()
              }
            )
          }
      ))
    )
  }
}

#[cfg(test)]
mod test {
  use quickcheck;
  use super::*;

  #[quickcheck]
  fn SimpleAccounts_simplifies(cmds: AccountsScenario) {
    let cmds = cmds.cmds;
    let mut simple: SimpleAccountsState = Default::default();
    let mut normal:       AccountsState = Default::default();

    simple.deep_invariant_check().unwrap();
    normal.deep_invariant_check().unwrap();

    for cmd in cmds {
      simple.fast_invariant_check().unwrap();
      normal.fast_invariant_check().unwrap();

      let simpl_res = simple.run_account_command(&cmd);
      let zero_cmd = match cmd {
        AccountsCommand::Mint(amt,unit) => {
          //assert!(simpl_res.is_err());
          AccountsCommand::Mint(0,unit)
        },
        AccountsCommand::Send(src,amt,unit,dst) => {
          //assert!(simpl_res.is_err());
          AccountsCommand::Send(src,0,unit,dst)
        },
        _ => cmd,
      };

      let normal_res = normal.run_account_command(&zero_cmd);
      assert_eq!(simpl_res,normal_res);
    }

    simple.deep_invariant_check().unwrap();
    normal.deep_invariant_check().unwrap();
  }

  #[quickcheck]
  fn SimpleAccounts_simplifies_with_amounts(cmds: AccountsScenario) {
    let cmds = cmds.cmds;
    let mut prev_simple: SimpleAccountsState = Default::default();
    let mut prev_normal:       AccountsState = Default::default();

    let mut simple: SimpleAccountsState = Default::default();
    let mut normal:       AccountsState = Default::default();

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
        prev_simple.run_account_command(&cmd);
        prev_normal.run_account_command(&cmd);
      }
    }

    simple.deep_invariant_check().unwrap();
    normal.deep_invariant_check().unwrap();
  }
}

