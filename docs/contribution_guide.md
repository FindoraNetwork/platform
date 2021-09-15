# Contribution Guide

## Environment Preparements

- [**Check This Page**](./compile_build.md)

## Compiling

- `make`

## Running

```shell
make join_testnet
# OR
make join_mainnet
```

## Code Style

#### Introduction of dependency

correct style:

```rust
use std::{env, thread, rand::random};
use clap::Arg;
```

wrong style:

```rust
extern crate rand;

use std::env;
use std::thread;
use rand::random;

// avoid '*' in importing
use clap::*;
```

#### Warnings

> Warnings are not allowed in any formal code; However, they are allowed in the test code.

correct style:

```rust
// lib.rs
#![deny(warnings)]
```

wrong style:

```rust
// any formal modular
#![allow(warnings)]
```

#### Comments & Document

correct style:

```rust
mod abc {
    //!
    //! # Modular Docs
    //!

    #![deny(missing_docs)]

    fn xxx() {}
}
```

wrong style:

```rust
/// # Modular Docs
mod abc {
    #![allow(missing_docs)]

    fn xxx() {}
}
```

#### The order of `mod` and `use`

correct style:

```rust
mod a;
mod b;

use std::env;
```

wrong style:

```rust
use std::env;

mod a;
mod b;
```
