#![deny(warnings)]
use percent_encoding::{percent_decode, utf8_percent_encode, AsciiSet, CONTROLS};
use std::io::{Error, ErrorKind};
use std::path::PathBuf;

pub fn string_of_type<T>(_: &T) -> String {
  std::any::type_name::<T>().to_string()
}

pub fn print_type_of<T>(msg: &str, _: &T) {
  println!("type of {}: {}", msg, std::any::type_name::<T>())
}

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

pub fn urldecode(s: &str) -> String {
  let iter = percent_decode(s.as_bytes());
  iter.decode_utf8().unwrap().to_string()
}

pub fn urlencode(input: &str) -> String {
  let iter = utf8_percent_encode(input, FRAGMENT);
  iter.collect()
}

const PROTOCOL: &str = "http";
const SERVER_HOST: &str = "localhost";

/// Port for querying values.
pub const QUERY_PORT: &str = "8668";
/// Port for submitting transactions.
pub const SUBMIT_PORT: &str = "8669";

/// Sets the protocol and host.
///
/// Environment variables `PROTOCOL` and `SERVER_HOST` set the protocol and host,
///
/// By default, the protocol is `http` and the host is `testnet.findora.org`.
pub fn protocol_host() -> (&'static str, &'static str) {
  (std::option_env!("PROTOCOL").unwrap_or(PROTOCOL),
   std::option_env!("SERVER_HOST").unwrap_or(SERVER_HOST))
}

pub fn fresh_tmp_dir() -> PathBuf {
  let base_dir = std::env::temp_dir();
  let base_dirname = "findora_ledger";
  let mut i = 0;
  let mut dirname = None;
  while dirname.is_none() {
    assert!(i < 10240); // TODO(joe): fail more gracefully
    let name = std::format!("{}_{}", base_dirname, i);
    let path = base_dir.join(name);
    match std::fs::create_dir(&path) {
      Ok(()) => {
        dirname = Some(path);
      }
      Err(_) => {
        i += 1;
      }
    }
  }

  // Safe unwrap -- the loop would never terminate if it stayed None
  dirname.unwrap()
}

pub fn se(s: String) -> Option<Error> {
  Some(Error::new(ErrorKind::Other, s))
}

pub fn er<T>(s: String) -> Result<T, Error> {
  Err(Error::new(ErrorKind::Other, s))
}

pub trait HasInvariants<ErrT> {
  // Simple sanity checks, preferably constant-time. Could be toggled in a production environment
  // without jeopardizing moderate performance requirements.
  fn fast_invariant_check(&self) -> Result<(), ErrT>;
  // Computationally intensive checks, intended for a testing environment.
  fn deep_invariant_check(&self) -> Result<(), ErrT>;
}

/// Convert a u64 into a string with commas.
fn commas_u64(input: u64) -> String {
  if input < 10000 {
    return format!("{}", input);
  }

  let mut value = input;
  let mut result = "".to_string();

  while value > 1000 {
    result = format!(",{:03.3}", value % 1000) + &result;
    value /= 1000;
  }

  if value == 1000 {
    result = "1,000".to_owned() + &result;
  } else {
    result = format!("{}", value) + &result;
  }

  result
}

/// Convert an i64 into a string with commas.
fn commas_i64(input: i64) -> String {
  if input == 0 {
    return "0".to_string();
  }

  let sign = input < 0;
  let mut result;

  if input == std::i64::MIN {
    result = commas_u64(1u64 << 63);
  } else if input < 0 {
    result = commas_u64(-input as u64);
  } else {
    result = commas_u64(input as u64);
  }

  if sign {
    result = "-".to_owned() + &result;
  }

  result
}

pub trait Commas {
  fn commas(self) -> String;
}

impl Commas for u64 {
  fn commas(self) -> String {
    crate::commas_u64(self)
  }
}

impl Commas for u32 {
  fn commas(self) -> String {
    crate::commas_u64(self as u64)
  }
}

impl Commas for u16 {
  fn commas(self) -> String {
    crate::commas_u64(self as u64)
  }
}

impl Commas for u8 {
  fn commas(self) -> String {
    crate::commas_u64(self as u64)
  }
}

impl Commas for usize {
  fn commas(self) -> String {
    crate::commas_u64(self as u64)
  }
}

impl Commas for i64 {
  fn commas(self) -> String {
    crate::commas_i64(self)
  }
}

impl Commas for i32 {
  fn commas(self) -> String {
    crate::commas_i64(self as i64)
  }
}

impl Commas for i16 {
  fn commas(self) -> String {
    crate::commas_i64(self as i64)
  }
}

impl Commas for i8 {
  fn commas(self) -> String {
    crate::commas_i64(self as i64)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_commas() {
    // Test u64
    assert_eq!("0", 0u64.commas());
    assert_eq!("100", 100u64.commas());
    assert_eq!("999", 999u64.commas());
    assert_eq!("1000", 1000_u64.commas());
    assert_eq!("9999", 9999u64.commas());
    assert_eq!("10,000", 10000_u64.commas());
    assert_eq!("1,000,000", (1000u64 * 1000u64).commas());
    assert_eq!("1,048,576", (1024 * 1024_u64).commas());
    assert_eq!("999,000", (999 * 1000_u64).commas());
    assert_eq!("2000", (2 * 1000_u64).commas());
    assert_eq!("1,000,000,000", (1000 * 1000 * 1000_u64).commas());
    assert_eq!("18,446,744,073,709,551,615", std::u64::MAX.commas());

    // Test u32
    assert_eq!("0", 0u32.commas());
    assert_eq!("100", 100u32.commas());
    assert_eq!("999", 999u32.commas());
    assert_eq!("1000", 1000_u32.commas());
    assert_eq!("9999", 9999u32.commas());
    assert_eq!("10,000", 10000_u32.commas());
    assert_eq!("1,000,000", (1000u32 * 1000u32).commas());
    assert_eq!("1,048,576", (1024 * 1024_u32).commas());
    assert_eq!("999,000", (999 * 1000_u32).commas());
    assert_eq!("2000", (2 * 1000_u32).commas());
    assert_eq!("1,000,000,000", (1000 * 1000 * 1000_u32).commas());
    assert_eq!("4,294,967,295", std::u32::MAX.commas());

    // Test u16
    assert_eq!("0", 0u16.commas());
    assert_eq!("100", 100u16.commas());
    assert_eq!("999", 999u16.commas());
    assert_eq!("1000", 1000_u16.commas());
    assert_eq!("9999", 9999u16.commas());
    assert_eq!("10,000", 10000_u16.commas());
    assert_eq!("2000", (2 * 1000_u16).commas());
    assert_eq!("65,535", std::u16::MAX.commas());

    // Test u8
    assert_eq!("0", 0u8.commas());
    assert_eq!("1", 1u8.commas());
    assert_eq!("100", 100u8.commas());
    assert_eq!("255", std::u8::MAX.commas());

    // Test i64
    assert_eq!("0", 0i64.commas());
    assert_eq!("100", 100i64.commas());
    assert_eq!("999", 999i64.commas());
    assert_eq!("1000", 1000.commas());
    assert_eq!("9999", 9999i64.commas());
    assert_eq!("10,000", 10000_i64.commas());
    assert_eq!("1,000,000", (1000i64 * 1000i64).commas());
    assert_eq!("999,000", (999i64 * 1000i64).commas());
    assert_eq!("2000", (2 * 1000_i64).commas());
    assert_eq!("1,000,000,000", (1000 * 1000 * 1000_i64).commas());
    assert_eq!("9,223,372,036,854,775,807", std::i64::MAX.commas());
    assert_eq!("-100", (-100_i64).commas());
    assert_eq!("-999", (-999_i64).commas());
    assert_eq!("-1000", (-1000_i64).commas());
    assert_eq!("-1,000,000", (-1000 * 1000_i64).commas());
    assert_eq!("-1,048,576", (-1024 * 1024_i64).commas());
    assert_eq!("-999,000", (-999 * 1000_i64).commas());
    assert_eq!("-2000", (-2 * 1000_i64).commas());
    assert_eq!("-1,000,000,000", (-1000 * 1000 * 1000_i64).commas());
    assert_eq!("-9,223,372,036,854,775,808", (std::i64::MIN).commas());

    // Test i32.
    assert_eq!("0", 0i32.commas());
    assert_eq!("100", 100i32.commas());
    assert_eq!("999", 999i32.commas());
    assert_eq!("1000", 1000.commas());
    assert_eq!("9999", 9999i32.commas());
    assert_eq!("10,000", 10000_i32.commas());
    assert_eq!("1,000,000", (1000i32 * 1000i32).commas());
    assert_eq!("999,000", (999i32 * 1000i32).commas());
    assert_eq!("2000", (2 * 1000_i32).commas());
    assert_eq!("1,000,000,000", (1000 * 1000 * 1000_i32).commas());
    assert_eq!("2,147,483,647", std::i32::MAX.commas());
    assert_eq!("-100", (-100_i32).commas());
    assert_eq!("-999", (-999_i32).commas());
    assert_eq!("-1000", (-1000_i32).commas());
    assert_eq!("-1,000,000", (-1000 * 1000_i32).commas());
    assert_eq!("-1,048,576", (-1024 * 1024_i32).commas());
    assert_eq!("-999,000", (-999 * 1000_i32).commas());
    assert_eq!("-2000", (-2 * 1000_i32).commas());
    assert_eq!("-1,000,000,000", (-1000 * 1000 * 1000_i32).commas());
    assert_eq!("-2,147,483,648", (std::i32::MIN).commas());

    // Test i16
    assert_eq!("0", 0i16.commas());
    assert_eq!("100", 100i16.commas());
    assert_eq!("999", 999i16.commas());
    assert_eq!("1000", 1000.commas());
    assert_eq!("9999", 9999i16.commas());
    assert_eq!("10,000", 10000_i16.commas());
    assert_eq!("2000", (2 * 1000_i16).commas());
    assert_eq!("32,767", std::i16::MAX.commas());
    assert_eq!("-100", (-100_i16).commas());
    assert_eq!("-999", (-999_i16).commas());
    assert_eq!("-1000", (-1000_i16).commas());
    assert_eq!("-2000", (-2 * 1000_i16).commas());
    assert_eq!("-32,768", (std::i16::MIN).commas());

    // Test i8
    assert_eq!("0", 0i8.commas());
    assert_eq!("-1", (-1i8).commas());
    assert_eq!("100", 100i8.commas());
    assert_eq!("127", std::i8::MAX.commas());
    assert_eq!("-100", (-100_i8).commas());
    assert_eq!("-128", (std::i8::MIN).commas());
  }
}
