//!
//! # Recover Mechanism
//!
//! automatic operations:
//! - create a light-weight(COW) snapshot for each block
//! - clean up expired snapshots
//!

use ruc::*;
use std::str::FromStr;

/// Maximum number of snapshots that can be kept
pub const CAP_MAX: u32 = 256;

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
    pub fn snapshot(&self) -> Result<()> {
        alt!(!self.enable, return Ok(()));

        alt!(self.mode.is_external(), todo!());

        match self.infra {
            SnapInfra::Zfs => zfs::gen_snapshot(self).c(d!()),
            SnapInfra::Btrfs => btrfs::gen_snapshot(self).c(d!()),
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

    /// Get snapshot list in desc order
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
    use super::SnapCfg;
    use ruc::*;

    pub(super) fn gen_snapshot(cfg: &SnapCfg) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        todo!()
    }

    pub(super) fn sorted_snapshots(_cfg: &SnapCfg) -> Result<Vec<u64>> {
        todo!()
    }

    pub(super) fn rollback(
        _cfg: &SnapCfg,
        _height: Option<u64>,
        _strict: bool,
    ) -> Result<()> {
        todo!()
    }

    pub(super) fn check(_target: &str) -> Result<()> {
        todo!()
    }

    fn clean_outdated(cfg: &SnapCfg) -> Result<()> {
        let _cap = cfg.get_cap();
        todo!()
    }
}

mod btrfs {
    use super::SnapCfg;
    use ruc::*;

    pub(super) fn gen_snapshot(cfg: &SnapCfg) -> Result<()> {
        clean_outdated(cfg).c(d!())?;
        todo!()
    }

    pub(super) fn sorted_snapshots(_cfg: &SnapCfg) -> Result<Vec<u64>> {
        todo!()
    }

    pub(super) fn rollback(
        _cfg: &SnapCfg,
        _height: Option<u64>,
        _strict: bool,
    ) -> Result<()> {
        todo!()
    }

    pub(super) fn check(_target: &str) -> Result<()> {
        todo!()
    }

    fn clean_outdated(cfg: &SnapCfg) -> Result<()> {
        let _cap = cfg.get_cap();
        todo!()
    }
}
