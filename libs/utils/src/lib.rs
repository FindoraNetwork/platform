use percent_encoding::{percent_decode, utf8_percent_encode, AsciiSet, CONTROLS};


pub fn string_of_type<T>(_: &T) -> String {
  format!("{}", std::any::type_name::<T>())
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
