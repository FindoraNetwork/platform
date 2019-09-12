// Writes a debug log entry when enabled.

pub struct EnableMap {
  pub error_enabled: bool,
  pub debug_enabled: bool,
  pub warning_enabled: bool,
  pub info_enabled: bool,
  pub log_enabled: bool,
}

// Define a set of defaults for anyone who
// prefers one.
pub const DEFAULT_MAP: EnableMap = EnableMap { error_enabled: true,
                                               debug_enabled: false,
                                               warning_enabled: true,
                                               info_enabled: false,
                                               log_enabled: true };

impl EnableMap {
  pub fn error_enabled(&self) -> bool {
    self.error_enabled
  }

  pub fn debug_enabled(&self) -> bool {
    self.debug_enabled
  }

  pub fn info_enabled(&self) -> bool {
    self.info_enabled
  }

  pub fn warning_enabled(&self) -> bool {
    self.warning_enabled
  }

  pub fn log_enabled(&self) -> bool {
    self.log_enabled
  }
}

#[macro_export]
macro_rules! log_impl {
  ($level:tt, $module:tt, $($x:tt)+) => {
    println!("{}  {:10.10}  {:16.16}  {}",
      timestamp(), stringify!($level), stringify!($module), format!($($x)+));
  }
}

// Write a log entry when enabled.
#[macro_export]
macro_rules! error {
    ($module:ident, $($x:tt)+) => {
      if $module.error_enabled() {
        log_impl!(error, $module, $($x)+);
      }
    }
}

// Write a debug log entry when enabled.
#[macro_export]
macro_rules! debug {
    ($module:ident, $($x:tt)+) => {
      if $module.debug_enabled() {
        log_impl!(error, $module, $($x)+);
      }
    }
}

// Write a debug log entry when enabled.
#[macro_export]
macro_rules! warning {
    ($module:ident, $($x:tt)+) => {
      if $module.warning_enabled() {
        log_impl!(error, $module, $($x)+);
      }
    }
}

// Write a debug log entry when enabled.
#[macro_export]
macro_rules! info {
    ($module:ident, $($x:tt)+) => {
      if $module.info_enabled() {
        log_impl!(error, $module, $($x)+);
      }
    }
}

// Write a log entry.
#[macro_export]
macro_rules! log {
    ($module:ident, $($x:tt)+) => {
      if $module.log_enabled() {
        log_impl!(error, $module, $($x)+);
      }
    }
}

// Returns Some(Error::...).
#[macro_export]
macro_rules! se {
    ($($x:tt)+) => { Some(Error::new(ErrorKind::Other, format!($($x)+))) }
}

// Returns Err(Error::new...).
#[macro_export]
macro_rules! er {
    ($($x:tt)+) => { Err(Error::new(ErrorKind::Other, format!($($x)+))) }
}

// Returns a deserializer error:  Err(serde::de::Error::...)
#[macro_export]
macro_rules! sde  {
    ($($x:tt)+) => {
        Err(serde::de::Error::custom(format!($($x)+)))
    }
}

// Produce a timestamp of UTC down to milliseconds, with rounding.
// Ignore leap seconds.
pub fn timestamp() -> String {
  use chrono::DateTime;
  use chrono::Datelike;
  use chrono::Timelike;
  use chrono::Utc;

  let now: DateTime<Utc> = Utc::now();

  format!("{:04}/{:02}/{:02}  {:02}:{:02}:{:02}.{:03} UTC",
          now.year(),
          now.month(),
          now.day(),
          now.hour(),
          now.minute(),
          now.second(),
          (now.nanosecond() + 500 * 1000) / (1000 * 1000))
}

/// Convert a u64 into a string with commas.
pub fn commas_u(input: u64) -> String {
  if input == 0 {
    return "0".to_string();
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

/// Convert a usize into a string with commas.
pub fn commas_us(input: usize) -> String {
  commas_u(input as u64)
}

/// Convert an i64 into a string with commas.
pub fn commas_i(input: i64) -> String {
  if input == 0 {
    return "0".to_string();
  }

  let sign = input < 0;
  let mut result;

  if input == std::i64::MIN {
    result = commas_u(1u64 << 63);
  } else if input < 0 {
    result = commas_u(-input as u64);
  } else {
    result = commas_u(input as u64);
  }

  if sign {
    result = "-".to_owned() + &result;
  }

  result
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basic_logging() {
    let root = EnableMap { error_enabled: true,
                           debug_enabled: true,
                           warning_enabled: true,
                           info_enabled: true,
                           log_enabled: true };

    log!(root, "Here at {}", timestamp());
    info!(root, "Here at {}", timestamp());
    debug!(root, "Here at {}", timestamp());
    warning!(root, "Here at {}", timestamp());
    error!(root, "Here at {}", timestamp());
    log!(root, "Here at {}", commas_u(0));
    log!(root, "Here at {}", commas_u(100));
    log!(root, "Here at {}", commas_u(999));
    log!(root, "Here at {}", commas_u(1000));
    log!(root, "Here at {}", commas_u(1000 * 1000));
    log!(root, "Here at {}", commas_u(1024 * 1024));
    log!(root, "Here at {}", commas_u(999 * 1000));
    log!(root, "Here at {}", commas_u(2 * 1000));
    log!(root, "Here at {}", commas_u(1000 * 1000 * 1000));
    log!(root,
         "Here at {} is u64::MAX should be {}",
         commas_u(std::u64::MAX),
         std::u64::MAX);
    log!(root, "Here at {}", commas_i(100));
    log!(root, "Here at {}", commas_i(999));
    log!(root, "Here at {}", commas_i(1000));
    log!(root, "Here at {}", commas_i(1000 * 1000));
    log!(root, "Here at {}", commas_i(999 * 1000));
    log!(root, "Here at {}", commas_i(2 * 1000));
    log!(root, "Here at {}", commas_i(1000 * 1000 * 1000));
    log!(root, "Here at {} is i64::MAX", commas_i(std::i64::MAX));
    log!(root, "Here at {}", commas_i(-100));
    log!(root, "Here at {}", commas_i(-999));
    log!(root, "Here at {}", commas_i(-1000));
    log!(root, "Here at {}", commas_i(-1000 * 1000));
    log!(root, "Here at {}", commas_i(-1024 * 1024));
    log!(root, "Here at {}", commas_i(-999 * 1000));
    log!(root, "Here at {}", commas_i(-2 * 1000));
    log!(root, "Here at {}", commas_i(-1000 * 1000 * 1000));
    log!(root,
         "Here at {} should be {}",
         commas_i(std::i64::MIN),
         std::i64::MIN);
  }
}
