//!
//! # Recover Mechanism
//!
//! automatic operations:
//! - create a light-weight(COW) snapshot for each block
//! - clean up expired snapshots
//!

use ledger::store::fbnc;
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
    /// native or external, default to native
    pub mode: SnapMode,
    /// zfs or btrfs, will try a guess if missing, only useful in native mode
    pub infra: SnapInfra,
    /// a UDP address like "ADDR:PORT", only useful in external mode
    pub udp_daemon: Option<String>,
    /// a data volume containing both ledger data and tendermint data
    pub target: String,
}

impl Default for SnapCfg {
    fn default() -> Self {
        SnapCfg {
            enable: false,
            itv: 10,
            cap: 100,
            mode: SnapMode::Native,
            infra: SnapInfra::Zfs,
            udp_daemon: None,
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
    pub fn snapshot(&self, h: u64) -> Result<()> {
        alt!(!self.enable, return Ok(()));
        alt!(0 != h % self.itv as u64, return Ok(()));

        alt!(self.mode.is_external(), todo!());

        fbnc::flush_data();
        match self.infra {
            SnapInfra::Zfs => zfs::gen_snapshot(self, h).c(d!()),
            SnapInfra::Btrfs => btrfs::gen_snapshot(self, h).c(d!()),
        }
    }

    /// rollback the state of blockchain to a specificed height
    #[inline(always)]
    pub fn rollback(&self, height: Option<u64>, strict: bool) -> Result<()> {
        match self.infra {
            SnapInfra::Zfs => zfs::rollback(self, height, strict).c(d!()),
            SnapInfra::Btrfs => btrfs::rollback(self, height, strict).c(d!()),
        }
    }

    /// Get snapshot list in aesc order.
    /// NOTE: must use AESC order, `binary_search` need this feature
    #[inline(always)]
    pub fn get_sorted_snapshots(&self) -> Result<Vec<u64>> {
        match self.infra {
            SnapInfra::Zfs => zfs::sorted_snapshots(self).c(d!()),
            SnapInfra::Btrfs => btrfs::sorted_snapshots(self).c(d!()),
        }
    }

    /// try to guess which infra the `target` is on
    #[inline(always)]
    pub fn guess_infra(&self) -> Result<SnapInfra> {
        zfs::check(&self.target)
            .c(d!())
            .map(|_| SnapInfra::Zfs)
            .or_else(|e| {
                btrfs::check(&self.target)
                    .c(d!(e))
                    .map(|_| SnapInfra::Btrfs)
            })
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn is_native_mode(&self) -> bool {
        self.mode.is_native()
    }

    #[inline(always)]
    #[allow(missing_docs)]
    pub fn is_external_mode(&self) -> bool {
        self.mode.is_external()
    }

    #[inline(always)]
    fn get_cap(&self) -> u32 {
        alt!(self.cap > CAP_MAX, CAP_MAX, self.cap)
    }
}

/// Which mode to use to generate the snapshot
pub enum SnapMode {
    /// manage the generation of snapshots directly
    Native,
    ///
    /// TODO: unimplemented yet!
    ///
    /// rely on an external independent process
    External,
}

impl Default for SnapMode {
    fn default() -> Self {
        Self::Native
    }
}

impl FromStr for SnapMode {
    type Err = Box<dyn RucError>;
    #[inline(always)]
    #[allow(missing_docs)]
    fn from_str(m: &str) -> Result<Self> {
        match m.to_lowercase().as_str() {
            "native" => Ok(Self::Native),
            // "external" => Ok(Self::External),
            "external" => Err(eg!("unimplemented!")),
            _ => Err(eg!("invalid mode name!")),
        }
    }
}

impl SnapMode {
    #[inline(always)]
    fn is_native(&self) -> bool {
        matches!(self, Self::Native)
    }

    #[inline(always)]
    fn is_external(&self) -> bool {
        matches!(self, Self::External)
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
pub enum SnapInfra {
    /// available on some Linux distributions and FreeBSD
    /// - Ubuntu Linux
    /// - Gentoo Linux
    /// - FreeBSD
    /// - ...
    Zfs,
    /// available on most Linux distributions,
    /// but its user experience is worse than zfs
    Btrfs,
}

impl Default for SnapInfra {
    fn default() -> Self {
        Self::Zfs
    }
}

impl FromStr for SnapInfra {
    type Err = Box<dyn RucError>;
    #[inline(always)]
    #[allow(missing_docs)]
    fn from_str(m: &str) -> Result<Self> {
        match m.to_lowercase().as_str() {
            "zfs" => Ok(Self::Zfs),
            "btrfs" => Ok(Self::Btrfs),
            _ => Err(eg!()),
        }
    }
}

mod zfs {
    use super::{exec_output, SnapCfg};
    use ruc::*;

    #[inline(always)]
    pub(super) fn gen_snapshot(cfg: &SnapCfg, h: u64) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        let cmd = format!(
            "
            zfs destroy {0}@{1} 2>/dev/null;
            zfs snapshot {0}@{1}
            ",
            &cfg.target, h
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
        res.sort_unstable();

        Ok(res)
    }

    pub(super) fn rollback(
        cfg: &SnapCfg,
        height: Option<u64>,
        strict: bool,
    ) -> Result<()> {
        let snaps = sorted_snapshots(cfg).c(d!())?;
        alt!(snaps.is_empty(), return Err(eg!("no snapshots")));

        let h = height.unwrap_or_else(|| snaps[snaps.len() - 1]);

        let cmd = match snaps.binary_search(&h) {
            Ok(_) => {
                format!("zfs rollback -r {}@{}", &cfg.target, h)
            }
            Err(idx) => {
                if strict {
                    return Err(eg!("specified height does not exist"));
                } else {
                    let effective_h = if 1 + idx > snaps.len() {
                        snaps[snaps.len() - 1]
                    } else {
                        *(0..idx).rev().find_map(|i| snaps.get(i)).c(d!())?
                    };
                    format!("zfs rollback -r {}@{}", &cfg.target, effective_h)
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

        snaps[..(snaps.len() - cap)].iter().for_each(|i| {
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
    pub(super) fn gen_snapshot(cfg: &SnapCfg, h: u64) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        let cmd = format!(
            "
            btrfs subvolume delete {0}@{1} 2>/dev/null;
            btrfs subvolume snapshot {0} {0}@{1}
            ",
            &cfg.target, h
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
        res.sort_unstable();

        Ok(res)
    }

    pub(super) fn rollback(
        cfg: &SnapCfg,
        height: Option<u64>,
        strict: bool,
    ) -> Result<()> {
        let snaps = sorted_snapshots(cfg).c(d!())?;
        alt!(snaps.is_empty(), return Err(eg!("no snapshots")));

        let h = height.unwrap_or_else(|| snaps[snaps.len() - 1]);

        let cmd = match snaps.binary_search(&h) {
            Ok(_) => {
                format!(
                    "
                    btrfs subvolume delete {0} 2>/dev/null;
                    btrfs subvolume snapshot {0}@{1} {0}
                    ",
                    &cfg.target, h
                )
            }
            Err(idx) => {
                if strict {
                    return Err(eg!("specified height does not exist"));
                } else {
                    let effective_h = if 1 + idx > snaps.len() {
                        snaps[snaps.len() - 1]
                    } else {
                        *(0..idx).rev().find_map(|i| snaps.get(i)).c(d!())?
                    };
                    format!(
                        "
                        btrfs subvolume delete {0} 2>/dev/null;
                        btrfs subvolume snapshot {0}@{1} {0}
                        ",
                        &cfg.target, effective_h
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

        let list = snaps[..(snaps.len() - cap)]
            .iter()
            .fold(String::new(), |acc, i| {
                acc + &format!("{}@{} ", &cfg.target, i)
            });

        let cmd = format!("btrfs subvolume delete -c {}", list);
        info_omit!(exec_output(cmd.trim_end()));

        Ok(())
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
