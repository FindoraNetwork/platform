# Data recovery mechanism

## Command line functions

```
findorad node

> running online, along with the blockchain process

--enable-snapshot        a global switch for enabling snapshot functions
--snapshot-itv           interval between adjacent snapshots, default to 10 blocks
--snapshot-cap           the maximum number of snapshots that will be stored, default to 100
--snapshot-mode          zfs/btrfs/external, will try a guess if missing
--snapshot-algo          fair/fade, default to `fair`
--snapshot-target        a data volume containing both ledger data and tendermint data
--snapshot-list          list all available snapshots in the form of block height

> running offline

--snapshot-rollback                rollback to the last available snapshot
-r, --snapshot-rollback-to         rollback to a custom height, \
                                   will try the closest smaller height if the target does not exist
-R, --snapshot-rollback-to-exact   rollback to a custom height exactly, \
                                   an error will be reported if the target does not exist
```

## User tutorial

#### Prepare the environment

1. Install and configure the basic file system:
    - zfs: [official documents](https://openzfs.github.io/openzfs-docs/Getting%20Started/index.html)
    - btrfs: [official documents](https://btrfs.wiki.kernel.org/index.php/Main_Page)
2. Switch to root account
    - the user whose UID is 0
    - its name is usually 'root', but it may not be
3. Create a root volume for testing
    - zfs
        - `zpool create zfs /dev/sdX` or `zpool create zfs <PATH-TO-A-FILE>`
        ```
        # assume the name of your zfs root volume is 'zfs'
        zfs destroy -R zfs/findora
        zfs create zfs/findora
        zfs set mountpoint=/data/findora zfs/findora
        ```
    - btrfs
        - `mkfs.btrfs -f /dev/sdX` or `mkfs.btrfs -f <PATH-TO-A-FILE>`
        - `mount /dev/sdX /btrfs` or `mount <PATH-TO-A-FILE> /btrfs`
        ```
        # assume the name of your btrfs root volume is '/btrfs'
        btrfs subvolume list -so /data/findora | xargs btrfs subvolume delete
        btrfs subvolume delete /data/findora
        btrfs subvolume create /data
        btrfs subvolume create /data/findora
        ```

#### Run findorad with snapshot

```shell
# init and start local node
# you should copy `findorad` to one of your $PATH before this step
findorad init -b /data/findora --mainnet
[zfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target zfs/findora
[btrfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target /data/findora

# show available snapshots
[zfs] findorad node --snapshot-target zfs/findora --snapshot-list
[btrfs] findorad node --snapshot-target /data/findora --snapshot-list
```

#### Let's Rollback

> you should stop all related processes before starting rollback in a formal scene
>
> ```shell
> pkill findorad
> pkill abcid
> pkill tendermint
> ```

Example: `--snapshot-rollback`

```shell
# corrupt data deliberately at run-time
# our processes will crash and can not restart again
rm -rf /data/findora/*

# rollback to the last available snapshot
[zfs] findorad node --snapshot-target zfs/findora --snapshot-rollback
[btrfs] findorad node --snapshot-target /data/findora --snapshot-rollback

# restart your node, it will start running from the last height correctly
[zfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target zfs/findora
[btrfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target /data/findora
```

Example: `--snapshot-rollback-to`

```shell
# corrupt data deliberately at run-time
# our processes will crash and can not restart again
rm -rf /data/findora/*

# try to rollback to the state at some height, such as 1000,
# will choose an available state at a smaller height if it does not exist
[zfs] findorad node --snapshot-target zfs/findora --snapshot-rollback-to 1000
[btrfs] findorad node --snapshot-target /data/findora --snapshot-rollback-to 1000

# restart your node, it will start running from the height of 1000(or 999, etc.) correctly
[zfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target zfs/findora
[btrfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target /data/findora
```

Example: `--snapshot-rollback-to-exact`

```shell
# corrupt data deliberately at run-time
# our processes will crash and can not restart again
rm -rf /data/findora/*

# try to rollback to the state at some height, such as 1000,
# will report error if it does not exist
[zfs] findorad node --snapshot-target zfs/findora --snapshot-rollback-to-exact 1000
[btrfs] findorad node --snapshot-target /data/findora --snapshot-rollback-to-exact 1000

# restart your node, it will start running from the height of 1000 correctly
[zfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target zfs/findora
[btrfs] findorad node -b /data/findora -q --enable-snapshot --snapshot-target /data/findora
```
