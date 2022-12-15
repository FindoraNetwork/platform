# Benchmarks

## Single machine benchmarks

#### Test results on a physical machine

> Hardwares:
> - 1 x [AMD EPYC 7773X, CPU 64 core, Memory 128 GB, Disk nvme ssd(raid0, 2 members)]
>
> Typography: 6 nodes(5 full nodes, 1 seed node) running on the same machine

**==>>** `make bench_50k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 21
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671111093
[##  Log  ##] Timestamp after sending: 1671111150
[##  Log  ##] BlockNumber after sending: 74
[##  Log  ##] Total number of on-chain transactions: 50000
[##  Log  ##] TPS(transaction per second): 877

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 75
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671111182
[##  Log  ##] Timestamp after sending: 1671111218
[##  Log  ##] BlockNumber after sending: 112
[##  Log  ##] Total number of on-chain transactions: 50000
[##  Log  ##] TPS(transaction per second): 1388
```

**==>>** `make bench_100k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 21
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671097783
[##  Log  ##] Timestamp after sending: 1671097877
[##  Log  ##] BlockNumber after sending: 110
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1063

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 112
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671097936
[##  Log  ##] Timestamp after sending: 1671098008
[##  Log  ##] BlockNumber after sending: 182
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1388
```

**==>>** `make bench_200k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 25
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671111665
[##  Log  ##] Timestamp after sending: 1671111859
[##  Log  ##] BlockNumber after sending: 144
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 1030

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 146
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671111966
[##  Log  ##] Timestamp after sending: 1671112113
[##  Log  ##] BlockNumber after sending: 284
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 1360
```

#### Test results on a AWS virtual machine

TODO

## Multiple machine benchmarks

#### Test results on physical machines

> Hardware:
> - 1 x [AMD EPYC 7773X, CPU 64 core, Memory 128 GB, Disk nvme ssd(raid0, 2 members)]
> - 1 x [AMD Ryzen 9 5900X, CPU 12 core, Memory 32 GB, Disk nvme ssd(single)]
> - 1 x [AMD Ryzen 7 5700G, CPU 8 core, Memory 32 GB, Disk nvme ssd(single)]
>
> Typography: 6 nodes(5 full nodes, 1 seed node) running on 3 different machines

`make dbench_100k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 24
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671098291
[##  Log  ##] Timestamp after sending: 1671098576
[##  Log  ##] BlockNumber after sending: 258
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 350

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 260
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671098638
[##  Log  ##] Timestamp after sending: 1671098928
[##  Log  ##] BlockNumber after sending: 501
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 344
```

#### Test results on AWS virtual machines

TODO
