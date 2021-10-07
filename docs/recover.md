# Findorad Recover Mechanism

## Command Line Changes

```
findorad node

> running online, along with the blockchain process

--enable-snapshot        a global switch for enabling snapshot functions
--snapshot-itv           interval between adjacent snapshots, default to 10 blocks
--snapshot-cap           the maximum number of snapshots that will be stored, default to 100
--snapshot-mode          native/external, default to native
--snapshot-infra         zfs/btrfs, will try a guess if missing, only useful in native mode
--snapshot-server        an address pair like "ADDR:PORT", only useful in external mode
--snapshot-target        a data volume containing both ledger data and tendermint data
--snapshot-list          list all available snapshots in the form of block height

findorad node restore

> running offline, only useful in native mode

--snapshot-infra         zfs/btrfs, will try a guess if missing
--snapshot-target        a data volume containing both ledger data and tendermint data

--list-snapshots         same as `findorad node --snapshot-list`

--to-height              restore to a custom height, will try the closest smaller height if the target does not exist
--to-height-exact        restore to a custom height exactly, an error will be reported if the target does not exist
```

## Inner Operations

assume:
- root volume of zfs is `zfs`
- root volume of btrfs is `/btrfs`
- business data is stored in `<root volume>/findora`
- target block height to recover is 123456

#### snapshot

```shell
# zfs filesystem
zfs destroy zfs/findora@123456 2>/dev/null
zfs snapshot zfs/findora@123456

# btrfs filesystem
rm -rf /btrfs/FINDORA/123456 2>/dev/null
btrfs subvolume snapshot /btrfs/findora /btrfs/FINDORA/123456
```

#### rollback

```shell
# zfs filesystem
zfs rollback -r zfs/findora@123456

# btrfs filesystem
rm -rf /btrfs/findora || exit 1
btrfs subvolume snapshot /btrfs/FINDORA/123456 /btrfs/findora
```
