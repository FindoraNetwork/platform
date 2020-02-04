// Copyright 2019 © Findora. All rights reserved.

#![deny(warnings)]

// Reference: /usr/include/sysexits.h
pub mod exit_code {
  pub const OK: i32 = 0;

  pub const USAGE: i32 = 64;

  pub const DATA_ERR: i32 = 65;

  pub const NO_INPUT: i32 = 66;

  pub const NO_USER: i32 = 67;

  pub const NO_HOST: i32 = 68;

  pub const UNAVAILABLE: i32 = 69;

  pub const SOFTWARE: i32 = 70;

  pub const OS_ERR: i32 = 71;

  pub const OS_FILE: i32 = 72;

  pub const CANT_CREATE: i32 = 73;

  pub const IO_ERR: i32 = 74;

  pub const TEMP_FAIL: i32 = 75;

  pub const PROTOCOL: i32 = 76;

  pub const NO_PERM: i32 = 77;

  pub const CONFIG: i32 = 78;
}
