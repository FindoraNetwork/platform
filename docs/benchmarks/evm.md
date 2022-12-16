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
[##  Log  ##] Timestamp before sending: 1671170226
[##  Log  ##] Timestamp after sending: 1671170266
[##  Log  ##] BlockNumber after sending: 63
[##  Log  ##] Total number of on-chain transactions: 50000
[##  Log  ##] TPS(transaction per second): 1250

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 64
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671170296
[##  Log  ##] Timestamp after sending: 1671170338
[##  Log  ##] BlockNumber after sending: 102
[##  Log  ##] Total number of on-chain transactions: 50000
[##  Log  ##] TPS(transaction per second): 1190
```

**==>>** `make bench_100k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 23
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671171065
[##  Log  ##] Timestamp after sending: 1671171143
[##  Log  ##] BlockNumber after sending: 103
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1282

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 105
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671171200
[##  Log  ##] Timestamp after sending: 1671171280
[##  Log  ##] BlockNumber after sending: 180
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1250
```

**==>>** `make bench_200k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 26
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671171465
[##  Log  ##] Timestamp after sending: 1671171627
[##  Log  ##] BlockNumber after sending: 192
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 1234

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 195
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671171732
[##  Log  ##] Timestamp after sending: 1671171890
[##  Log  ##] BlockNumber after sending: 344
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 1265
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

`make dbench_50k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 24
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671193192
[##  Log  ##] Timestamp after sending: 1671193238
[##  Log  ##] BlockNumber after sending: 46
[##  Log  ##] Total number of on-chain transactions: 49984
[##  Log  ##] TPS(transaction per second): 1086

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 47
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671193248
[##  Log  ##] Timestamp after sending: 1671193284
[##  Log  ##] BlockNumber after sending: 76
[##  Log  ##] Total number of on-chain transactions: 49984
[##  Log  ##] TPS(transaction per second): 1388
```

`make dbench_100k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 26
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671194925
[##  Log  ##] Timestamp after sending: 1671195002
[##  Log  ##] BlockNumber after sending: 77
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1298

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 82
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671195027
[##  Log  ##] Timestamp after sending: 1671195091
[##  Log  ##] BlockNumber after sending: 129
[##  Log  ##] Total number of on-chain transactions: 100000
[##  Log  ##] TPS(transaction per second): 1562
```

`make dbench_200k`:

```
[##  Log  ##] Deploying ERC20 contract ...

[##  Log  ##] Transfering native token ...

[##  Log  ##] BlockNumber before sending: 29
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671194228
[##  Log  ##] Timestamp after sending: 1671194451
[##  Log  ##] BlockNumber after sending: 76
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 896

[##  Log  ##] Transfering erc20 token ...

[##  Log  ##] BlockNumber before sending: 77
[** Debug **] Start sending ...
[##  Log  ##] Timestamp before sending: 1671194472
[##  Log  ##] Timestamp after sending: 1671194603
[##  Log  ##] BlockNumber after sending: 172
[##  Log  ##] Total number of on-chain transactions: 200000
[##  Log  ##] TPS(transaction per second): 1526
```

#### Test results on AWS virtual machines

TODO
