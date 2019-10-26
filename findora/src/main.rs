use findora::dw::write_packet;
use findora::dw::Command;
use findora::dw::Contents::LoggingFlags;
use findora::LoggingEnableFlags;
use std::env;
use std::io::Read;
use std::net::TcpStream;
use std::process::exit;

fn main() {
  let arguments: Vec<String> = env::args().collect();

  if arguments.len() != 5 {
    usage(&arguments);
    return;
  }

  let mut stream = match TcpStream::connect(&arguments[1]) {
    Err(e) => {
      println!("The connect to {} failed:  {}", &arguments[1], e);
      return;
    }
    Ok(stream) => stream,
  };

  // Currently, there's only one command, but that might change.
  match arguments[2].as_ref() {
    "log" => {
      log_command(&mut stream, &arguments[3], &arguments[4]);
    }
    _ => {
      usage(&arguments);
    }
  }
}

fn usage(arguments: &[String]) {
  println!("Usage:");
  println!("    {} <host:port> log <category> <flags>", arguments[0]);
}

fn log_command(stream: &mut TcpStream, category: &str, flags_str: &str) {
  let flags = flags_str.as_bytes();

  if flags.len() != 5 {
    flags_usage();
    return;
  }

  // Create and write the packet.
  let enable_flags = create_enable_flags(category, flags);
  let command = Command { contents: LoggingFlags(enable_flags) };
  let result = write_packet(stream, &command);

  if let Err(e) = result {
    println!("The packet send failed:  {}", e);
    return;
  }

  // Now read the result.  The server returns a one-byte result
  // indicating whether an error occurred.  A 0 implies success.
  let mut buffer = [0_u8; 1];
  let result = stream.read(&mut buffer[..]);

  match result {
    Ok(count) => {
      if count != 1 {
        println!("The read return an invalid count:  {}", count);
      } else if buffer[0] != 0 {
        println!("The command failed:  is the category correct?");
      } else {
        println!("The flags for category \"{}\" have been set.", category);
      }
    }
    Err(e) => {
      println!("The read for the status failed:  {}", e);
    }
  }
}

fn create_enable_flags(name: &str, flags: &[u8]) -> LoggingEnableFlags {
  LoggingEnableFlags { name: name.to_owned(),
                       log: decode(flags[0]).1,
                       error: decode(flags[1]).1,
                       warning: decode(flags[2]).1,
                       debug: decode(flags[3]).1,
                       info: decode(flags[4]).1,
                       modify_log: decode(flags[0]).0,
                       modify_error: decode(flags[1]).0,
                       modify_warning: decode(flags[2]).0,
                       modify_debug: decode(flags[3]).0,
                       modify_info: decode(flags[4]).0 }
}

// Decode a single flag character into an modifier flag and
// a value.
//   t  set the flag to true
//   f  set the flag to false
//   x  leave the flag unchanged
fn decode(value: u8) -> (bool, bool) {
  match value as char {
    't' => (true, true),
    'f' => (true, false),
    'x' => (false, false), // The second value is ignored by the server
    _ => {
      flags_usage();
      exit(0);
    }
  }
}

fn flags_usage() {
  println!("The flags must be 5 characters long.  Each");
  println!("character should be \"t\", \"f\", or \"x\".");
  println!("An \"x\" indicates that the value should not");
  println!("be changed.");
  println!("The flags are:");
  println!("  log");
  println!("  error");
  println!("  warning");
  println!("  debug");
  println!("  info");
  println!("in that order.");
}

#[cfg(test)]
mod tests {
  use super::*;
  use findora::dw::get_test_socket;

  #[test]
  fn test_basic_program() {
    assert!(decode('t' as u8) == (true, true));
    assert!(decode('f' as u8) == (true, false));
    assert!(decode('x' as u8) == (false, false));

    let flags = create_enable_flags("test", "tftft".as_ref());

    assert!(flags.modify_log == true);
    assert!(flags.modify_error == true);
    assert!(flags.modify_warning == true);
    assert!(flags.modify_debug == true);
    assert!(flags.modify_info == true);

    assert!(flags.log == true);
    assert!(flags.error == false);
    assert!(flags.warning == true);
    assert!(flags.debug == false);
    assert!(flags.info == true);

    let flags = create_enable_flags("test", "txtxt".as_ref());

    assert!(flags.modify_log == true);
    assert!(flags.modify_error == false);
    assert!(flags.modify_warning == true);
    assert!(flags.modify_debug == false);
    assert!(flags.modify_info == true);

    assert!(flags.log == true);
    assert!(flags.error == false);
    assert!(flags.warning == true);
    assert!(flags.debug == false);
    assert!(flags.info == true);

    let mut stream = get_test_socket();
    let command = Command { contents: LoggingFlags(flags) };

    write_packet(&mut stream, &command).unwrap();

    // Read the result return from the command.  It should
    // be zero, indicating success.
    let mut buffer = [0_u8; 1];
    let result = stream.read(&mut buffer[..]).unwrap();

    assert!(result == 1);
    assert!(buffer[0] == 0);

    log_command(&mut stream, "test", "tftft");
  }
}
