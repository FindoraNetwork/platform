# Benchmarks

## Single machine benchmarks

#### Test results on a physical machine

> Hardwares: 1 * [AMD EPYC 7773X, CPU 64 core, Memory 128 GB, Disk nvme ssd]
>
> Typography: 6 nodes(5 full nodes, 1 seed node) running on the same machine

`make bench_100k`:

```
```

## Multiple machine benchmarks

#### Test results on physical machines

> Hardware:
> - 1 x [AMD EPYC 7773X, CPU 64 core, Memory 128 GB, Disk nvme ssd(raid0, 2 members)]
> - 1 x [AMD Ryzen 9 5900X, CPU 12 core, Memory 32 GB, Disk nvme ssd(single)]
> - 1 x [AMD Ryzen 7 5700G, CPU 8 core, Memory 32 GB, Disk nvme ssd(single)]
>
> Typography: 5 nodes(5 full nodes, 1 seed node) running on 3 different machines

`make dbench_100k`:

```
```
