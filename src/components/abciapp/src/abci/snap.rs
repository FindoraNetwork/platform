//!
//! # Recover Mechanism
//!
//! automatic operations:
//! - create a light-weight(COW) snapshot for each block
//! - clean up expired snapshots
//!

use ruc::*;
use std::{process::Command, str::FromStr};

/// Maximum number of snapshots that can be kept
pub const CAP_MAX: u32 = 1024;

/// Config structure of snapshot
pub struct SnapCfg {
    /// a global switch for enabling snapshot functions
    pub enable: bool,
    /// interval between adjacent snapshots, default to 10 blocks
    pub itv: u32,
    /// the maximum number of snapshots that will be stored, default to 100
    pub cap: u32,
    /// Zfs or Btrfs or External, will try a guess if missing
    pub mode: SnapMode,
    /// a data volume containing both ledger data and tendermint data
    pub target: String,
}

impl Default for SnapCfg {
    fn default() -> Self {
        SnapCfg {
            enable: false,
            itv: 10,
            cap: 100,
            mode: SnapMode::Zfs,
            target: "zfs/findora".to_owned(),
        }
    }
}

impl SnapCfg {
    /// create a simple instance
    #[inline(always)]
    pub fn new() -> Self {
        SnapCfg {
            enable: true,
            ..Self::default()
        }
    }

    /// generate a snapshot for the latest state of blockchain
    #[inline(always)]
    pub fn snapshot(&self, idx: u64) -> Result<()> {
        alt!(!self.enable, return Ok(()));
        alt!(0 != idx % self.itv as u64, return Ok(()));

        match self.mode {
            SnapMode::Zfs => zfs::gen_snapshot(self, idx).c(d!()),
            SnapMode::Btrfs => btrfs::gen_snapshot(self, idx).c(d!()),
            SnapMode::External => external::gen_snapshot(self, idx).c(d!()),
        }
    }

    /// rollback the state of blockchain to a specificed height
    #[inline(always)]
    pub fn rollback(&self, idx: Option<u64>, strict: bool) -> Result<()> {
        match self.mode {
            SnapMode::Zfs => zfs::rollback(self, idx, strict).c(d!()),
            SnapMode::Btrfs => btrfs::rollback(self, idx, strict).c(d!()),
            SnapMode::External => Err(eg!("please use `btm` tool in `External` mode")),
        }
    }

    /// Get snapshot list in desc order.
    #[inline(always)]
    pub fn get_sorted_snapshots(&self) -> Result<Vec<u64>> {
        match self.mode {
            SnapMode::Zfs => zfs::sorted_snapshots(self).c(d!()),
            SnapMode::Btrfs => btrfs::sorted_snapshots(self).c(d!()),
            SnapMode::External => Err(eg!("please use `btm` tool in `External` mode")),
        }
    }

    /// try to guess a correct mode
    /// NOTE: not suitable for `External` mode
    #[inline(always)]
    pub fn guess_mode(&self) -> Result<SnapMode> {
        zfs::check(&self.target)
            .c(d!())
            .map(|_| SnapMode::Zfs)
            .or_else(|e| btrfs::check(&self.target).c(d!(e)).map(|_| SnapMode::Btrfs))
    }

    #[inline(always)]
    fn get_cap(&self) -> u32 {
        alt!(self.cap > CAP_MAX, CAP_MAX, self.cap)
    }
}

/// # Inner Operations
///
/// assume:
/// - root volume of zfs is `zfs`
/// - root volume of btrfs is `/btrfs`
/// - business data is stored in `<root volume>/findora`
/// - target block height to recover is 123456
///
/// ## snapshot
///
/// ```shell
/// # zfs filesystem
/// zfs destroy zfs/findora@123456 2>/dev/null
/// zfs snapshot zfs/findora@123456
///
/// # btrfs filesystem
/// rm -rf /btrfs/FINDORA/123456 2>/dev/null
/// btrfs subvolume snapshot /btrfs/findora /btrfs/FINDORA/123456
/// ```
///
/// ## rollback
///
/// ```shell
/// # zfs filesystem
/// zfs rollback -r zfs/findora@123456
///
/// # btrfs filesystem
/// rm -rf /btrfs/findora || exit 1
/// btrfs subvolume snapshot /btrfs/FINDORA/123456 /btrfs/findora
/// ```
pub enum SnapMode {
    /// available on some Linux distributions and FreeBSD
    /// - Ubuntu Linux
    /// - Gentoo Linux
    /// - FreeBSD
    /// - ...
    Zfs,
    /// available on most Linux distributions,
    /// but its user experience is worse than zfs
    Btrfs,
    /// TODO: unimplemented!
    /// rely on an external independent process
    External,
}

impl Default for SnapMode {
    fn default() -> Self {
        Self::Zfs
    }
}

impl FromStr for SnapMode {
    type Err = Box<dyn RucError>;
    #[inline(always)]
    #[allow(missing_docs)]
    fn from_str(m: &str) -> Result<Self> {
        match m.to_lowercase().as_str() {
            "zfs" => Ok(Self::Zfs),
            "btrfs" => Ok(Self::Btrfs),
            "external" => Ok(Self::External),
            _ => Err(eg!()),
        }
    }
}

mod zfs {
    use super::{exec_output, SnapCfg};
    use ruc::*;

    #[inline(always)]
    pub(super) fn gen_snapshot(cfg: &SnapCfg, idx: u64) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        let cmd = format!(
            "
            zfs destroy {0}@{1} 2>/dev/null;
            zfs snapshot {0}@{1}
            ",
            &cfg.target, idx
        );
        exec_output(&cmd).c(d!()).map(|_| ())
    }

    pub(super) fn sorted_snapshots(cfg: &SnapCfg) -> Result<Vec<u64>> {
        let cmd = format!(
            r"zfs list -t snapshot {} | grep -o '@[0-9]\+' | sed 's/@//'",
            &cfg.target
        );
        let output = exec_output(&cmd).c(d!())?;

        let mut res = output
            .lines()
            .map(|l| l.parse::<u64>().c(d!()))
            .collect::<Result<Vec<u64>>>()?;
        res.sort_unstable_by(|a, b| b.cmp(a));

        Ok(res)
    }

    pub(super) fn rollback(cfg: &SnapCfg, idx: Option<u64>, strict: bool) -> Result<()> {
        // convert to AESC order for `binary_search`
        let snaps = sorted_snapshots(cfg)
            .c(d!())?
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        alt!(snaps.is_empty(), return Err(eg!("no snapshots")));

        let idx = idx.unwrap_or_else(|| snaps[snaps.len() - 1]);

        let cmd = match snaps.binary_search(&idx) {
            Ok(_) => {
                format!("zfs rollback -r {}@{}", &cfg.target, idx)
            }
            Err(i) => {
                if strict {
                    return Err(eg!("specified height does not exist"));
                } else {
                    let effective_idx = if 1 + i > snaps.len() {
                        snaps[snaps.len() - 1]
                    } else {
                        *(0..i).rev().find_map(|i| snaps.get(i)).c(d!())?
                    };
                    format!("zfs rollback -r {}@{}", &cfg.target, effective_idx)
                }
            }
        };

        exec_output(&cmd).c(d!()).map(|_| ())
    }

    #[inline(always)]
    pub(super) fn check(target: &str) -> Result<()> {
        let cmd = format!("zfs list {0} || zfs create {0}", target);
        exec_output(&cmd).c(d!()).map(|_| ())
    }

    fn clean_outdated(cfg: &SnapCfg) -> Result<()> {
        let snaps = sorted_snapshots(cfg).c(d!())?;
        let cap = cfg.get_cap() as usize;

        if 1 + cap > snaps.len() {
            return Ok(());
        }

        snaps[cap..].iter().for_each(|i| {
            let cmd = format!("zfs destroy {}@{}", &cfg.target, i);
            info_omit!(exec_output(&cmd));
        });

        Ok(())
    }
}

mod btrfs {
    use super::{exec_output, SnapCfg};
    use ruc::*;
    use std::path::PathBuf;

    #[inline(always)]
    pub(super) fn gen_snapshot(cfg: &SnapCfg, idx: u64) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        let cmd = format!(
            "
            btrfs subvolume delete {0}@{1} 2>/dev/null;
            btrfs subvolume snapshot {0} {0}@{1}
            ",
            &cfg.target, idx
        );
        exec_output(&cmd).c(d!()).map(|_| ())
    }

    pub(super) fn sorted_snapshots(cfg: &SnapCfg) -> Result<Vec<u64>> {
        let cmd = format!(
            r"btrfs subvolume list -so {} | grep -o '@[0-9]\+$' | sed 's/@//'",
            PathBuf::from(&cfg.target)
                .parent()
                .c(d!())?
                .to_str()
                .c(d!())?
        );
        let output = exec_output(&cmd).c(d!())?;

        let mut res = output
            .lines()
            .map(|l| l.parse::<u64>().c(d!()))
            .collect::<Result<Vec<u64>>>()?;
        res.sort_unstable_by(|a, b| b.cmp(a));

        Ok(res)
    }

    pub(super) fn rollback(cfg: &SnapCfg, idx: Option<u64>, strict: bool) -> Result<()> {
        // convert to AESC order for `binary_search`
        let snaps = sorted_snapshots(cfg)
            .c(d!())?
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        alt!(snaps.is_empty(), return Err(eg!("no snapshots")));

        let idx = idx.unwrap_or_else(|| snaps[snaps.len() - 1]);

        let cmd = match snaps.binary_search(&idx) {
            Ok(_) => {
                format!(
                    "
                    btrfs subvolume delete {0} 2>/dev/null;
                    btrfs subvolume snapshot {0}@{1} {0}
                    ",
                    &cfg.target, idx
                )
            }
            Err(i) => {
                if strict {
                    return Err(eg!("specified height does not exist"));
                } else {
                    let effective_idx = if 1 + i > snaps.len() {
                        snaps[snaps.len() - 1]
                    } else {
                        *(0..i).rev().find_map(|i| snaps.get(i)).c(d!())?
                    };
                    format!(
                        "
                        btrfs subvolume delete {0} 2>/dev/null;
                        btrfs subvolume snapshot {0}@{1} {0}
                        ",
                        &cfg.target, effective_idx
                    )
                }
            }
        };

        exec_output(&cmd).c(d!()).map(|_| ())
    }

    #[inline(always)]
    pub(super) fn check(target: &str) -> Result<()> {
        let cmd = format!(
            "btrfs subvolume list {0} || btrfs subvolume create {0}",
            target
        );
        exec_output(&cmd).c(d!()).map(|_| ())
    }

    fn clean_outdated(cfg: &SnapCfg) -> Result<()> {
        let snaps = sorted_snapshots(cfg).c(d!())?;
        let cap = cfg.get_cap() as usize;

        if 1 + cap > snaps.len() {
            return Ok(());
        }

        let list = snaps[cap..].iter().fold(String::new(), |acc, i| {
            acc + &format!("{}@{} ", &cfg.target, i)
        });

        let cmd = format!("btrfs subvolume delete -c {}", list);
        info_omit!(exec_output(cmd.trim_end()));

        Ok(())
    }
}

mod external {
    use super::SnapCfg;
    use ruc::*;

    #[inline(always)]
    pub(super) fn gen_snapshot(_cfg: &SnapCfg, _idx: u64) -> Result<()> {
        todo!()
    }
}

#[inline(always)]
fn exec_output(cmd: &str) -> Result<String> {
    let res = Command::new("sh").arg("-c").arg(cmd).output().c(d!())?;
    if res.status.success() {
        Ok(String::from_utf8_lossy(&res.stdout).into_owned())
    } else {
        Err(eg!(String::from_utf8_lossy(&res.stderr).into_owned()))
    }
}
